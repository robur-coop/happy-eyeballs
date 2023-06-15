(* Lwt tasks are spawned:
 - create starts an asynchronous timer task
 - the actions resulting from timer are scheduled in one separate task
 - the actions returned from Happy_eyeballs.connect/event are scheduled in
   respective separate tasks
*)

let src = Logs.Src.create "happy-eyeballs.lwt" ~doc:"Happy Eyeballs Lwt"
module Log = (val Logs.src_log src : Logs.LOG)

let now = Mtime_clock.elapsed_ns

type t = {
  mutable waiters : ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.u Happy_eyeballs.Waiter_map.t ;
  mutable cancel_connecting : (int * unit Lwt.u) list Happy_eyeballs.Waiter_map.t;
  mutable he : Happy_eyeballs.t ;
  dns : Dns_client_lwt.t ;
  timer_interval : float ;
  timer_condition : unit Lwt_condition.t ;
}

let safe_close fd =
  if Lwt_unix.state fd = Lwt_unix.Closed then
    Lwt.return_unit
  else
    Lwt_unix.close fd

let try_connect ip port =
  let open Lwt_result.Infix in
  let fd =
    let fam = match ip with
      | Ipaddr.V4 _ -> Lwt_unix.PF_INET
      | Ipaddr.V6 _ -> Lwt_unix.PF_INET6
    in
    Lwt_unix.(socket fam SOCK_STREAM 0)
  in
  Lwt.catch
    (fun () ->
       let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
       Lwt_result.ok (Lwt_unix.connect fd addr) >|= fun () ->
       fd)
    (fun e ->
       Lwt_result.ok (safe_close fd) >>= fun () ->
       Lwt_result.fail (`Msg ("connect failure: " ^ Printexc.to_string e)))

let rec act t action =
  let open Lwt.Infix in
  Log.debug (fun m -> m "action %a" Happy_eyeballs.pp_action action);
  begin
    match action with
    | Happy_eyeballs.Resolve_a host ->
      begin
        Dns_client_lwt.getaddrinfo t.dns Dns.Rr_map.A host >|= function
        | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_a (host, res))
        | Error `Msg msg -> Ok (Happy_eyeballs.Resolved_a_failed (host, msg))
      end
    | Happy_eyeballs.Resolve_aaaa host ->
      begin
        Dns_client_lwt.getaddrinfo t.dns Dns.Rr_map.Aaaa host >|= function
        | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_aaaa (host, res))
        | Error `Msg msg -> Ok (Happy_eyeballs.Resolved_aaaa_failed (host, msg))
      end
    | Happy_eyeballs.Connect (host, id, attempt, (ip, port)) ->
      begin
        let cancelled, cancel = Lwt.task () in
        let entry = attempt, cancel in
        t.cancel_connecting <-
          Happy_eyeballs.Waiter_map.update id
            (function None -> Some [ entry ] | Some c -> Some (entry :: c))
            t.cancel_connecting;
        let conn =
          try_connect ip port >>= function
          | Ok fd ->
            let cancel_connecting, others =
              Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
            in
            t.cancel_connecting <- cancel_connecting;
            List.iter (fun (att, w) -> if att <> attempt then Lwt.wakeup_later w ())
              (Option.value ~default:[] others);
            let waiters, r = Happy_eyeballs.Waiter_map.find_and_remove id t.waiters in
            t.waiters <- waiters;
            begin match r with
              | Some waiter ->
                Lwt.wakeup_later waiter (Ok ((ip, port), fd));
                Lwt.return (Ok (Happy_eyeballs.Connected (host, id, (ip, port))))
              | None ->
                (* waiter already vanished *)
                safe_close fd >>= fun () ->
                Lwt.return (Error ())
            end
          | Error `Msg msg ->
            t.cancel_connecting <-
              Happy_eyeballs.Waiter_map.update id
                (function None -> None | Some c ->
                  match List.filter (fun (att, _) -> not (att = attempt)) c with
                  | [] -> None
                  | c -> Some c)
                t.cancel_connecting;
            Lwt.return (Ok (Happy_eyeballs.Connection_failed (host, id, (ip, port), msg)))
        in
        Lwt.pick [ conn; (cancelled >|= fun () -> Error ()); ]
      end
    | Happy_eyeballs.Connect_failed (host, id, msg) ->
      let cancel_connecting, others =
        Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
      in
      t.cancel_connecting <- cancel_connecting;
      List.iter (fun (_, w) -> Lwt.wakeup_later w ()) (Option.value ~default:[] others);
      let waiters, r = Happy_eyeballs.Waiter_map.find_and_remove id t.waiters in
      t.waiters <- waiters;
      begin match r with
        | Some waiter ->
          let err =
            Fmt.str "connection to %s failed: %s"
              (match Ipaddr.of_domain_name host with
               | None -> Domain_name.to_string host
               | Some ip -> Ipaddr.to_string ip)
              msg
          in
          Lwt.wakeup_later waiter (Error (`Msg err));
          Lwt.return (Error ())
        | None ->
          (* waiter already vanished *)
          Lwt.return (Error ())
      end
  end >>= function
  | Error () -> Lwt.return_unit
  | Ok ev ->
    let he, actions = Happy_eyeballs.event t.he (now ()) ev in
    t.he <- he;
    Lwt_list.iter_p (act t) actions

let handle_timer_actions t actions =
  Lwt.async (fun () -> Lwt_list.iter_p (fun a -> act t a) actions)

let rec timer t =
  let open Lwt.Infix in
  let rec loop () =
    let he, cont, actions = Happy_eyeballs.timer t.he (now ()) in
    t.he <- he ;
    handle_timer_actions t actions ;
    match cont with
    | `Suspend ->
      timer t
    | `Act ->
      Lwt_unix.sleep t.timer_interval >>= fun () ->
      loop ()
  in
  Lwt_condition.wait t.timer_condition >>= fun () ->
  loop ()

let create ?(happy_eyeballs = Happy_eyeballs.create (now ())) ?dns ?(timer_interval = Duration.of_ms 10) () =
  let dns =
    Option.value ~default:
      (let timeout = Happy_eyeballs.resolve_timeout happy_eyeballs in
       Dns_client_lwt.create ~timeout ())
      dns
  in
  let waiters = Happy_eyeballs.Waiter_map.empty
  and cancel_connecting = Happy_eyeballs.Waiter_map.empty
  and timer_condition = Lwt_condition.create ()
  in
  let timer_interval = Duration.to_f timer_interval in
  let t = { waiters ; cancel_connecting ; he = happy_eyeballs ; dns ; timer_interval ; timer_condition } in
  Lwt.async (fun () -> timer t);
  t

let handle_actions t actions =
  List.iter (fun a -> Lwt.async (fun () -> act t a)) actions

let connect_host t host ports =
  let waiter, notify = Lwt.task () in
  let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
  t.waiters <- waiters;
  let ts = now () in
  let he, actions = Happy_eyeballs.connect t.he ts ~id host ports in
  t.he <- he;
  Lwt_condition.signal t.timer_condition ();
  handle_actions t actions;
  let open Lwt.Infix in
  waiter >|= fun r ->
  Log.debug (fun m -> m "connection %s to %a after %a"
                (match r with Ok _ -> "ok" | Error _ -> "failed")
                Domain_name.pp host Duration.pp (Int64.sub (now ()) ts));
  r

let connect_ip t addresses =
  let waiter, notify = Lwt.task () in
  let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
  t.waiters <- waiters;
  let ts = now () in
  let he, actions = Happy_eyeballs.connect_ip t.he ts ~id addresses in
  t.he <- he;
  Lwt_condition.signal t.timer_condition ();
  handle_actions t actions;
  let open Lwt.Infix in
  waiter >|= fun r ->
  Log.debug (fun m -> m "connection %s to %a after %a"
                (match r with Ok _ -> "ok" | Error _ -> "failed")
                Fmt.(list ~sep:(any ", ") (pair ~sep:(any ":") Ipaddr.pp int))
                addresses
                Duration.pp (Int64.sub (now ()) ts));
  r

let connect t host ports =
  match Ipaddr.of_string host with
  | Ok ip -> connect_ip t (List.map (fun p -> (ip, p)) ports)
  | Error _ ->
    let open Lwt_result.Infix in
    Lwt_result.lift
      (Result.bind (Domain_name.of_string host) Domain_name.host) >>= fun h ->
    connect_host t h ports
