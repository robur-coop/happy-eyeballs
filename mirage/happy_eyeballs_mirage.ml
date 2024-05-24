module type S = sig
  type t
  type stack
  type flow

  type getaddrinfo = [ `A | `AAAA ] -> [ `host ] Domain_name.t -> (Ipaddr.Set.t, [ `Msg of string ]) result Lwt.t

  val create : ?happy_eyeballs:Happy_eyeballs.t ->
    ?getaddrinfo:getaddrinfo -> ?timer_interval:int64 -> stack -> t

  val inject : t -> getaddrinfo -> unit

  val connect_host : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    [`host] Domain_name.t -> int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t

  val connect_ip : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> (Ipaddr.t * int) list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t

  val connect : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    string -> int list ->
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

let ctr = ref 0

module Make (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (S : Tcpip.Stack.V4V6) : sig
  include S
    with type flow = S.TCP.flow
     and type stack = S.t

  val connect_device : ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    ?timer_interval:int64 -> ?getaddrinfo:getaddrinfo -> stack -> t Lwt.t
end = struct
  type stack = S.t

  type flow = S.TCP.flow

  type getaddrinfo = [ `A | `AAAA ] -> [ `host ] Domain_name.t -> (Ipaddr.Set.t, [ `Msg of string ]) result Lwt.t

  type t = {
    stack : S.t ;
    mutable waiters : ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.u Happy_eyeballs.Waiter_map.t ;
    mutable cancel_connecting : (int * unit Lwt.u) list Happy_eyeballs.Waiter_map.t;
    mutable he : Happy_eyeballs.t ;
    timer_interval : int64 ;
    timer_condition : unit Lwt_condition.t ;
    counter : int ;
    mutable getaddrinfo : getaddrinfo option ;
  }

  let _cnt = ref 0

  let inject t getaddrinfo =
    incr _cnt;
    t.getaddrinfo <- Some getaddrinfo;
    if !_cnt > 1 then
      Log.warn (fun m -> m "inject was called the %u times" !_cnt)

  let try_connect stack addr =
    let open Lwt.Infix in
    S.TCP.create_connection (S.tcp stack) addr >|= fun r ->
    Result.map_error
      (fun err -> `Msg (Fmt.to_to_string S.TCP.pp_error err)) r

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
          match t.getaddrinfo with
          | None ->
            Log.err (fun m -> m "trying to lookup %a, but there's no getaddrinfo"
                        Domain_name.pp host);
            Lwt.return (Error ())
          | Some getaddrinfo ->
            getaddrinfo record host >|= fun res ->
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

  let create ?(happy_eyeballs = Happy_eyeballs.create (C.elapsed_ns ())) ?getaddrinfo ?(timer_interval = Duration.of_ms 10) stack =
    let waiters = Happy_eyeballs.Waiter_map.empty
    and cancel_connecting = Happy_eyeballs.Waiter_map.empty
    and timer_condition = Lwt_condition.create ()
    in
    incr ctr;
    let t = { stack ; waiters ; cancel_connecting ; he = happy_eyeballs ; timer_interval ; timer_condition ; counter = !ctr ; getaddrinfo } in
    Lwt.async (fun () -> timer t);
    t

  let handle_actions t actions =
    List.iter (fun a -> Lwt.async (fun () -> act t a)) actions

  let open_msg_error = function
    | Ok _ as r -> r
    | Error (`Msg _) as r -> r

  let connect_host t ?aaaa_timeout ?connect_delay ?connect_timeout
    ?resolve_timeout ?resolve_retries host ports =
    let waiter, notify = Lwt.task () in
    let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
    t.waiters <- waiters;
    let ts = C.elapsed_ns () in
    let he, actions =
      Happy_eyeballs.connect t.he ts ?aaaa_timeout ?connect_delay
        ?connect_timeout ?resolve_timeout ?resolve_retries ~id host ports
    in
    t.he <- he;
    Lwt_condition.signal t.timer_condition ();
    handle_actions t actions;
    let open Lwt.Infix in
    waiter >|= fun r ->
    Log.debug (fun m -> m "[%u] connection %s to %a after %a"
                  t.counter (match r with Ok _ -> "ok" | Error _ -> "failed")
                  Domain_name.pp host
                  Duration.pp (Int64.sub (C.elapsed_ns ()) ts));
    open_msg_error r

  let connect_ip t ?aaaa_timeout ?connect_delay ?connect_timeout addresses =
    let waiter, notify = Lwt.task () in
    let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
    t.waiters <- waiters;
    let ts = C.elapsed_ns () in
    let he, actions =
      Happy_eyeballs.connect_ip t.he ts ?aaaa_timeout ?connect_delay
        ?connect_timeout ~id addresses
    in
    t.he <- he;
    Lwt_condition.signal t.timer_condition ();
    handle_actions t actions;
    let open Lwt.Infix in
    waiter >|= fun r ->
    Log.debug (fun m -> m "[%u] connection %s to %a after %a"
                  t.counter (match r with Ok _ -> "ok" | Error _ -> "failed")
                  Fmt.(list ~sep:(any ", ") (pair ~sep:(any ":") Ipaddr.pp int))
                  addresses
                  Duration.pp (Int64.sub (C.elapsed_ns ()) ts));
    open_msg_error r

  let connect t ?aaaa_timeout ?connect_delay ?connect_timeout
    ?resolve_timeout ?resolve_retries host ports =
    match Ipaddr.of_string host with
    | Ok ip ->
      connect_ip t ?aaaa_timeout ?connect_delay ?connect_timeout
        (List.map (fun p -> (ip, p)) ports)
    | Error _ ->
      let open Lwt_result.Infix in
      Lwt_result.lift
        (Result.bind (Domain_name.of_string host) Domain_name.host) >>= fun h ->
      connect_host t ?aaaa_timeout ?connect_delay ?connect_timeout
        ?resolve_timeout ?resolve_retries h ports

  let connect_device ?aaaa_timeout ?connect_delay ?connect_timeout
    ?resolve_timeout ?resolve_retries ?timer_interval ?getaddrinfo stack =
    let happy_eyeballs =
      Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
        ?resolve_timeout ?resolve_retries (C.elapsed_ns ())
    in
    let happy_eyeballs = create ~happy_eyeballs ?getaddrinfo ?timer_interval stack in
    Lwt.return happy_eyeballs
end
