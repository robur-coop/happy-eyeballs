module type S = sig
  module Transport : Dns_client.S
    with type io_addr = [ `Plaintext of Ipaddr.t * int  | `Tls of Tls.Config.client * Ipaddr.t * int ]
     and type +'a io = 'a Lwt.t

  type t
  type dns
  type flow

  val create : ?happy_eyeballs:Happy_eyeballs.t ->
    ?dns:dns -> ?timer_interval:int64 -> Transport.stack -> t

  val connect_host : t -> [`host] Domain_name.t -> int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t

  val connect_ip : t -> (Ipaddr.t * int) list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t

  val connect : t -> string -> int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t
end

(* Lwt tasks are spawned:
 - create starts an asynchronous timer task
 - the actions resulting from timer are scheduled in one separate task
 - the actions returned from Happy_eyeballs.connect/event are scheduled in
   respective separate tasks
*)

let src = Logs.Src.create "happy-eyeballs.mirage" ~doc:"Happy Eyeballs Mirage"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (S : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S with type Transport.stack = S.t) : sig
  include S
    with module Transport = DNS.Transport
     and type dns = DNS.t
     and type flow = S.TCP.flow

  (* note: the v6_connect_timeout is kept until 1.0.0 since it is referenced in mirage *)
  val connect_device : ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?v6_connect_timeout:int64 -> ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    ?timer_interval:int64 -> dns -> Transport.stack -> t Lwt.t
end = struct
  module Transport = DNS.Transport
  type dns = DNS.t

  type flow = S.TCP.flow

  type t = {
    dns : DNS.t ;
    stack : S.t ;
    mutable waiters : ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.u Happy_eyeballs.Waiter_map.t ;
    mutable cancel_connecting : (int * unit Lwt.u) list Happy_eyeballs.Waiter_map.t;
    mutable he : Happy_eyeballs.t ;
    timer_interval : int64 ;
    timer_condition : unit Lwt_condition.t ;
  }

  let try_connect stack addr =
    let open Lwt.Infix in
    S.TCP.create_connection (S.tcp stack) addr >|= fun r ->
    Result.map_error
      (fun err -> `Msg (Fmt.to_to_string S.TCP.pp_error err)) r

  let rec act t action =
    let open Lwt.Infix in
    Log.debug (fun m -> m "action %a" Happy_eyeballs.pp_action action);
    begin
      match action with
      | Happy_eyeballs.Resolve_a host ->
        begin
          DNS.getaddrinfo t.dns Dns.Rr_map.A host >|= function
          | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_a (host, res))
          | Error `Msg msg -> Ok (Happy_eyeballs.Resolved_a_failed (host, msg))
        end
      | Happy_eyeballs.Resolve_aaaa host ->
        begin
          DNS.getaddrinfo t.dns Dns.Rr_map.Aaaa host >|= function
          | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_aaaa (host, res))
          | Error `Msg msg -> Ok (Happy_eyeballs.Resolved_aaaa_failed (host, msg))
        end
      | Happy_eyeballs.Connect (host, id, attempt, addr) ->
        begin
          let cancelled, cancel = Lwt.task () in
          let entry = attempt, cancel in
          t.cancel_connecting <-
            Happy_eyeballs.Waiter_map.update id
              (function None -> Some [ entry ] | Some c -> Some (entry :: c))
              t.cancel_connecting;
          let conn =
            try_connect t.stack addr >>= function
            | Ok flow ->
              let cancel_connecting, others =
                Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
              in
              t.cancel_connecting <- cancel_connecting;
              List.iter (fun (att, u) -> if att <> attempt then Lwt.wakeup_later u ())
                (Option.value ~default:[] others);
              let waiters, r = Happy_eyeballs.Waiter_map.find_and_remove id t.waiters in
              t.waiters <- waiters;
              begin match r with
                | Some waiter ->
                  Lwt.wakeup_later waiter (Ok (addr, flow));
                  Lwt.return (Ok (Happy_eyeballs.Connected (host, id, addr)))
                | None ->
                  (* waiter already vanished *)
                  S.TCP.close flow >>= fun () ->
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
              Lwt.return (Ok (Happy_eyeballs.Connection_failed (host, id, addr, msg)))
          in
          Lwt.pick [ conn ; (cancelled >|= fun () -> Error ()); ]
        end
      | Happy_eyeballs.Connect_failed (host, id, msg) ->
        let cancel_connecting, others =
          Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
        in
        t.cancel_connecting <- cancel_connecting;
        List.iter (fun (_, u) -> Lwt.wakeup_later u ()) (Option.value ~default:[] others);
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
    | Error _ -> Lwt.return_unit
    | Ok ev ->
      let he, actions = Happy_eyeballs.event t.he (C.elapsed_ns ()) ev in
      t.he <- he;
      Lwt_list.iter_p (act t) actions

  let handle_timer_actions t actions =
    Lwt.async (fun () -> Lwt_list.iter_p (fun a -> act t a) actions)

  let rec timer t =
    let open Lwt.Infix in
    let rec loop () =
      let he, cont, actions = Happy_eyeballs.timer t.he (C.elapsed_ns ()) in
      t.he <- he ;
      handle_timer_actions t actions ;
      match cont with
      | `Suspend ->
        timer t
      | `Act ->
        T.sleep_ns t.timer_interval >>= fun () ->
        loop ()
    in
    Lwt_condition.wait t.timer_condition >>= fun () ->
    loop ()

  let create ?(happy_eyeballs = Happy_eyeballs.create (C.elapsed_ns ())) ?dns ?(timer_interval = Duration.of_ms 10) stack =
    let dns =
      Option.value ~default:
        (let timeout = Happy_eyeballs.resolve_timeout happy_eyeballs in
         DNS.create ~timeout stack)
        dns
    and waiters = Happy_eyeballs.Waiter_map.empty
    and cancel_connecting = Happy_eyeballs.Waiter_map.empty
    and timer_condition = Lwt_condition.create ()
    in
    let t = { dns ; stack ; waiters ; cancel_connecting ; he = happy_eyeballs ; timer_interval ; timer_condition } in
    Lwt.async (fun () -> timer t);
    t

  let handle_actions t actions =
    List.iter (fun a -> Lwt.async (fun () -> act t a)) actions

  let open_msg_error = function
    | Ok _ as r -> r
    | Error (`Msg _) as r -> r

  let connect_host t host ports =
    let waiter, notify = Lwt.task () in
    let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
    t.waiters <- waiters;
    let ts = C.elapsed_ns () in
    let he, actions = Happy_eyeballs.connect t.he ts ~id host ports in
    t.he <- he;
    Lwt_condition.signal t.timer_condition ();
    handle_actions t actions;
    let open Lwt.Infix in
    waiter >|= fun r ->
    Log.debug (fun m -> m "connection %s to %a after %a"
                  (match r with Ok _ -> "ok" | Error _ -> "failed")
                  Domain_name.pp host
                  Duration.pp (Int64.sub (C.elapsed_ns ()) ts));
    open_msg_error r

  let connect_ip t addresses =
    let waiter, notify = Lwt.task () in
    let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
    t.waiters <- waiters;
    let ts = C.elapsed_ns () in
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
                  Duration.pp (Int64.sub (C.elapsed_ns ()) ts));
    open_msg_error r

  let connect t host ports =
    match Ipaddr.of_string host with
    | Ok ip -> connect_ip t (List.map (fun p -> (ip, p)) ports)
    | Error _ ->
      let open Lwt_result.Infix in
      Lwt_result.lift
        (Result.bind (Domain_name.of_string host) Domain_name.host) >>= fun h ->
      connect_host t h ports

  (* note: the v6_connect_timeout is kept until 1.0.0 since it is referenced in mirage *)
  let connect_device ?aaaa_timeout ?connect_delay ?v6_connect_timeout:_ ?connect_timeout
    ?resolve_timeout ?resolve_retries ?timer_interval dns stack =
    let happy_eyeballs =
      Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
        ?resolve_timeout ?resolve_retries (C.elapsed_ns ())
    in
    let happy_eyeballs = create ~happy_eyeballs ~dns ?timer_interval stack in
    Lwt.return happy_eyeballs
end
