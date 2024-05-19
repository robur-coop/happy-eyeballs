(* Lwt tasks are spawned:
 - create starts an asynchronous timer task
 - the actions resulting from timer are scheduled in one separate task
 - the actions returned from Happy_eyeballs.connect/event are scheduled in
   respective separate tasks
*)

let src = Logs.Src.create "happy-eyeballs.lwt" ~doc:"Happy Eyeballs Lwt"
module Log = (val Logs.src_log src : Logs.LOG)

let now = Mtime_clock.elapsed_ns

type getaddrinfo = [ `A | `AAAA ] -> [ `host ] Domain_name.t -> (Ipaddr.Set.t, [ `Msg of string ]) result Lwt.t

type t = {
  mutable waiters : ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.u Happy_eyeballs.Waiter_map.t ;
  mutable cancel_connecting : (int * unit Lwt.u) list Happy_eyeballs.Waiter_map.t;
  mutable he : Happy_eyeballs.t ;
  timer_interval : float ;
  timer_condition : unit Lwt_condition.t ;
  counter : int ;
  mutable getaddrinfo : getaddrinfo ;
}

let _cnt = ref 0

let inject getaddrinfo t =
  incr _cnt;
  t.getaddrinfo <- getaddrinfo;
  if !_cnt > 1 then
    Log.warn (fun m -> m "inject was called the %u times" !_cnt)

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
  Log.debug (fun m -> m "[%u] action %a" t.counter
                Happy_eyeballs.pp_action action);
  begin
    match action with
    | Happy_eyeballs.Resolve_a host | Happy_eyeballs.Resolve_aaaa host ->
      begin
        let record = match action with
          | Happy_eyeballs.Resolve_a _ -> `A
          | Happy_eyeballs.Resolve_aaaa _ -> `AAAA
          | _ -> assert false (* never occur! *)
        in
        t.getaddrinfo record host >|= fun res ->
        match res, record with
        | Ok set, `A ->
          let fold ip set = match ip with
            | Ipaddr.V4 ipv4 -> Ipaddr.V4.Set.add ipv4 set
            | Ipaddr.V6 ipv6 ->
              Log.warn (fun m -> m "received the IPv6 address %a querying A of %a (ignoring)"
                           Ipaddr.V6.pp ipv6 Domain_name.pp host);
              set
          in
          Ok (Happy_eyeballs.Resolved_a (host, Ipaddr.Set.fold fold set Ipaddr.V4.Set.empty))
        | Ok set, `AAAA ->
          let fold ip set = match ip with
            | Ipaddr.V6 ipv6 -> Ipaddr.V6.Set.add ipv6 set
            | Ipaddr.V4 ipv4 ->
              Log.warn (fun m -> m "received the IPv4 address %a querying AAAA of %a (ignoring)"
                           Ipaddr.V4.pp ipv4 Domain_name.pp host);
              set
          in
          Ok (Happy_eyeballs.Resolved_aaaa (host, Ipaddr.Set.fold fold set Ipaddr.V6.Set.empty))
        | Error `Msg msg, _ -> Ok (Happy_eyeballs.Resolved_a_failed (host, msg))
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

let ctr = ref 0
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let getaddrinfo record domain_name =
  let open Lwt.Infix in
  let getaddrinfo_option = match record with
    | `A -> [ Unix.AI_FAMILY Unix.PF_INET ]
    | `AAAA -> [ Unix.AI_FAMILY Unix.PF_INET6 ] in
  let getaddrinfo_option = Unix.AI_SOCKTYPE Unix.SOCK_STREAM :: getaddrinfo_option in
  Lwt.catch
    (fun () -> Lwt_unix.getaddrinfo (Domain_name.to_string domain_name) "" getaddrinfo_option >|= fun r -> Ok r)
    (fun exn -> Lwt.return (Error exn)) >|= function
  | Error exn -> error_msgf "while resolving %a, ran into exception %s" Domain_name.pp domain_name
                   (Printexc.to_string exn)
  | Ok [] -> error_msgf "%a not found" Domain_name.pp domain_name
  | Ok addrs ->
    let set = List.fold_left (fun set { Unix.ai_addr; _ } -> match ai_addr with
      | Unix.ADDR_INET (inet_addr, _) -> Ipaddr.Set.add (Ipaddr_unix.of_inet_addr inet_addr) set
      | Unix.ADDR_UNIX _ -> set)
      Ipaddr.Set.empty addrs in
    Ok set

let create ?(happy_eyeballs = Happy_eyeballs.create (now ())) ?(getaddrinfo= getaddrinfo)
  ?(timer_interval = Duration.of_ms 10) () =
  let waiters = Happy_eyeballs.Waiter_map.empty
  and cancel_connecting = Happy_eyeballs.Waiter_map.empty
  and timer_condition = Lwt_condition.create ()
  in
  let timer_interval = Duration.to_f timer_interval in
  incr ctr;
  let t = { waiters ; cancel_connecting ; he = happy_eyeballs ; getaddrinfo ; timer_interval ; timer_condition ; counter = !ctr } in
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
  Log.debug (fun m -> m "[%u] connection %s to %a after %a"
                t.counter (match r with Ok _ -> "ok" | Error _ -> "failed")
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
  Log.debug (fun m -> m "[%u] connection %s to %a after %a"
                t.counter (match r with Ok _ -> "ok" | Error _ -> "failed")
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
