let src_daemon = Logs.Src.create "happy-daemon"

module Logd = (val Logs.src_log src_daemon : Logs.LOG)

let src_client = Logs.Src.create "happy"

module Logc = (val Logs.src_log src_client : Logs.LOG)

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Format.fprintf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Format.fprintf ppf "%s:%u" (Unix.string_of_inet_addr inet_addr) port

let to_sockaddr (ipaddr, port) =
  Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port)

let clock = Mtime_clock.elapsed_ns

[@@@warning "-30"]

type state = ((Ipaddr.t * int) * Unix.file_descr) Miou.Computation.t

and entry = Happy_eyeballs.id * attempt * [ `host ] Domain_name.t * addr
and attempt = int
and addr = Ipaddr.t * int
and cancel = attempt * unit Miou.t
and connect_ip =
  { aaaa_timeout : int64 option
  ; connect_delay : int64 option
  ; connect_timeout : int64 option
  ; state : state
  ; addrs : addr list }
and connect =
  { aaaa_timeout : int64 option
  ; connect_delay : int64 option
  ; connect_timeout : int64 option
  ; resolve_timeout : int64 option
  ; resolve_retries : int option
  ; state : state
  ; host : [ `host ] Domain_name.t
  ; ports : int list }
and action =
  [ `Connect_ip of connect_ip
  | `Connect of connect ]
and connected = [ `Connected of entry * Miou_unix.Ownership.file_descr ]

type event =
  [ connected
  | `Connection_failed of entry * string
  | `Resolution_v4 of
    [ `host ] Domain_name.t * (Ipaddr.V4.Set.t, [ `Msg of string ]) result
  | `Resolution_v6 of
    [ `host ] Domain_name.t * (Ipaddr.V6.Set.t, [ `Msg of string ]) result ]
and getaddrinfo = [ `A | `AAAA ] -> [ `host ] Domain_name.t -> (Ipaddr.Set.t, [ `Msg of string ]) result

[@@@warning "+30"]

let getaddrinfo record domain_name =
  Logd.debug (fun m -> m "Resolve %a via the system's resolver" Domain_name.pp domain_name);
  let opt = match record with
    | `A -> [ Unix.AI_FAMILY Unix.PF_INET ]
    | `AAAA -> [ Unix.AI_FAMILY Unix.PF_INET6 ] in
  let opt = Unix.AI_SOCKTYPE Unix.SOCK_STREAM :: opt in
  match Unix.getaddrinfo (Domain_name.to_string domain_name) "" opt with
  | [] -> error_msgf "%a not found" Domain_name.pp domain_name
  | addrs ->
    let set = List.fold_left (fun set { Unix.ai_addr; _ } -> match ai_addr with
      | Unix.ADDR_INET (inet_addr, _) -> Ipaddr.Set.add (Ipaddr_unix.of_inet_addr inet_addr) set
      | Unix.ADDR_UNIX _ -> set) Ipaddr.Set.empty addrs in
    if Ipaddr.Set.is_empty set
    then error_msgf "%a not found as an inet service" Domain_name.pp domain_name
    else Ok set

type t = {
    mutable cancel_connecting: cancel list Happy_eyeballs.Waiter_map.t
  ; mutable waiters: state Happy_eyeballs.Waiter_map.t
  ; condition: Miou.Condition.t
  ; mutex: Miou.Mutex.t
  ; queue: [ action | event ] Miou.Queue.t
  ; mutable set : bool
  ; mutable getaddrinfo: getaddrinfo
  ; timer_interval : float
}

let create timer_interval getaddrinfo =
  {
    cancel_connecting= Happy_eyeballs.Waiter_map.empty
  ; waiters= Happy_eyeballs.Waiter_map.empty
  ; condition= Miou.Condition.create ()
  ; mutex= Miou.Mutex.create ()
  ; queue= Miou.Queue.create ()
  ; set= false
  ; getaddrinfo
  ; timer_interval
  }

let try_connect t ~meta addr () =
  let id, attempt, _, _ = meta in
  let addr = to_sockaddr addr in
  Logd.debug (fun m ->
      m "connect to %a (%d:%d)" pp_sockaddr addr (Obj.magic id) attempt);
  let socket =
    match Unix.domain_of_sockaddr addr with
    | Unix.PF_UNIX ->
      let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Miou_unix.Ownership.of_file_descr socket
    | Unix.PF_INET -> Miou_unix.Ownership.tcpv4 ()
    | Unix.PF_INET6 -> Miou_unix.Ownership.tcpv6 ()
  in
  try
    Miou_unix.Ownership.connect socket addr;
    Logd.debug (fun m ->
        m "connected to %a (%d:%d)" pp_sockaddr addr (Obj.magic id) attempt);
    Miou.Ownership.transfer (Miou_unix.Ownership.resource socket);
    Miou.Mutex.protect t.mutex @@ fun () ->
    Miou.Queue.enqueue t.queue (`Connected (meta, socket));
    Miou.Condition.signal t.condition
  with Unix.Unix_error (err, _, _) ->
    Logd.err (fun m ->
        m "error connecting to %a: %s" pp_sockaddr addr (Unix.error_message err));
    Miou_unix.Ownership.close socket;
    let msg =
      Fmt.str "error connecting to %a: %s" pp_sockaddr addr
        (Unix.error_message err)
    in
    Miou.Mutex.protect t.mutex @@ fun () ->
    Miou.Queue.enqueue t.queue (`Connection_failed (meta, msg));
    Miou.Condition.signal t.condition

let connect t ~prms:orphans host id attempt addr =
  let meta = (id, attempt, host, addr) in
  Logd.debug (fun m ->
      m "connect to %a (%d:%d)" Domain_name.pp host (Obj.magic id) attempt);
  let prm : unit Miou.t = Miou.async ~orphans (try_connect t ~meta addr) in
  let entry = (attempt, prm) in
  t.cancel_connecting <-
    Happy_eyeballs.Waiter_map.update id
      (function None -> Some [ entry ] | Some cs -> Some (entry :: cs))
      t.cancel_connecting

exception Connection_failed of [ `host ] Domain_name.t * string

let empty_bt = Printexc.get_callstack 0
let connection_failed host reason =
  Connection_failed (host, reason), empty_bt

let handle_one_action t ~prms action =
  match action with
  | Happy_eyeballs.Connect (host, id, attempt, addr) ->
      connect t ~prms host id attempt addr
  | Happy_eyeballs.Connect_failed (host, id, reason) ->
      Logd.warn (fun m ->
          m "connection to %a failed: %s" Domain_name.pp host reason);
      let cancel_connecting, others =
        Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
      in
      t.cancel_connecting <- cancel_connecting;
      List.iter
        (fun (_, prm) -> Miou.cancel prm)
        (Option.value ~default:[] others);
      (* clean waiter *)
      let waiters, waiter =
        Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
      in
      t.waiters <- waiters;
      let trans waiter =
        let err = connection_failed host reason in
        ignore (Miou.Computation.try_cancel waiter err) in
      Option.iter trans waiter
  | Happy_eyeballs.(Resolve_a host | Resolve_aaaa host) ->
      let record = match action with
        | Happy_eyeballs.Resolve_a _ -> `A
        | Happy_eyeballs.Resolve_aaaa _ -> `AAAA
        | _ -> assert false in
      let _ =
        Miou.async ~orphans:prms @@ fun () ->
        match t.getaddrinfo `A host with
        | Ok result ->
          let ipv4, ipv6 = Ipaddr.Set.fold (fun ip (ipv4, ipv6) -> match ip with
            | Ipaddr.V4 v -> Ipaddr.V4.Set.add v ipv4, ipv6
            | Ipaddr.V6 v -> ipv4, Ipaddr.V6.Set.add v ipv6)
            result Ipaddr.(V4.Set.empty, V6.Set.empty) in
          let result = match record with
            | `A ->
                if Ipaddr.V4.Set.is_empty ipv4
                then `Resolution_v4 (host, error_msgf "%a unreachable via IPv4" Domain_name.pp host)
                else `Resolution_v4 (host, Ok ipv4)
            | `AAAA ->
                if Ipaddr.V6.Set.is_empty ipv6
                then `Resolution_v6 (host, error_msgf "%a unreachable via IPv6" Domain_name.pp host)
                else `Resolution_v6 (host, Ok ipv6) in
          Miou.Mutex.protect t.mutex @@ fun () ->
          Miou.Queue.enqueue t.queue result;
          Miou.Condition.signal t.condition
        | Error err ->
          let result = match record with
            | `A -> `Resolution_v4 (host, Error err)
            | `AAAA -> `Resolution_v6 (host, Error err) in
          Miou.Mutex.protect t.mutex @@ fun () ->
          Miou.Queue.enqueue t.queue result;
          Miou.Condition.signal t.condition
      in
      ()

let to_event t = function
  | `Connection_failed ((id, attempt, host, addr), msg) ->
      let fold = function
        | None -> None
        | Some cs -> (
            match List.filter (fun (att, _) -> not (att = attempt)) cs with
            | [] -> None
            | cs -> Some cs)
      in
      t.cancel_connecting <-
        Happy_eyeballs.Waiter_map.update id fold t.cancel_connecting;
      Happy_eyeballs.Connection_failed (host, id, addr, msg)
  | `Connected ((id, attempt, host, addr), fd) ->
      let cancel_connecting, others =
        Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
      in
      t.cancel_connecting <- cancel_connecting;
      List.iter
        (fun (att, prm) -> if att <> attempt then Miou.cancel prm)
        (Option.value ~default:[] others);
      let waiters, waiter = Happy_eyeballs.Waiter_map.find_and_remove id t.waiters in
      t.waiters <- waiters;
      let () =
        match waiter with
        | None ->
            Logd.warn (fun m -> m "Loose a connected socket to %a (%a) (%d:%d)" Domain_name.pp host
              pp_sockaddr (to_sockaddr addr) (Obj.magic id) attempt);
            Miou_unix.Ownership.close fd
        | Some waiter ->
            (* NOTE(dinosaure): the task is suspended **before** [disown]. If a
               cancellation appear, [disown] is **not** executed and Miou
               properly close [fd]. If we are able to [disown], the
               responsability to close the [fd] falls to the person who
               requested the socket if we are able to transfer it. Otherwise,
               we just [Unix.close]. *)
            Miou.Ownership.disown (Miou_unix.Ownership.resource fd);
            let fd = Miou_unix.Ownership.to_file_descr fd in
            let set = Miou.Computation.try_return waiter (addr, fd) in
            Logd.debug (fun m -> m "file-descr transmitted? %b" set);
            if not set then Unix.close fd
      in
      Logd.debug (fun m ->
          m "connected to %a (%a) (%d:%d)" Domain_name.pp host pp_sockaddr
            (to_sockaddr addr) (Obj.magic id) attempt);
      Happy_eyeballs.Connected (host, id, addr)
  | `Resolution_v4 (host, Ok ips) ->
      Logd.debug (fun m -> m "%a resolved" Domain_name.pp host);
      Happy_eyeballs.Resolved_a (host, ips)
  | `Resolution_v4 (host, Error (`Msg msg)) ->
      Logd.warn (fun m ->
          m "impossible to resolve %a: %s" Domain_name.pp host msg);
      Happy_eyeballs.Resolved_a_failed (host, msg)
  | `Resolution_v6 (host, Ok ips) ->
      Logd.debug (fun m -> m "%a resolved" Domain_name.pp host);
      Happy_eyeballs.Resolved_aaaa (host, ips)
  | `Resolution_v6 (host, Error (`Msg msg)) ->
      Logd.warn (fun m ->
          m "impossible to resolve %a: %s" Domain_name.pp host msg);
      Happy_eyeballs.Resolved_aaaa_failed (host, msg)

let to_actions t he user's_actions =
  let fold (he, actions) = function
    | `Connect_ip { aaaa_timeout
                  ; connect_delay
                  ; connect_timeout
                  ; state
                  ; addrs } ->
        let waiters, id = Happy_eyeballs.Waiter_map.register state t.waiters in
        t.waiters <- waiters;
        let he, actions' = Happy_eyeballs.connect_ip he (clock ())
          ?aaaa_timeout ?connect_delay ?connect_timeout ~id addrs in
        (he, actions @ actions')
    | `Connect { aaaa_timeout
               ; connect_delay
               ; connect_timeout
               ; resolve_timeout
               ; resolve_retries
               ; state
               ; host
               ; ports } ->
        let waiters, id = Happy_eyeballs.Waiter_map.register state t.waiters in
        t.waiters <- waiters;
        let he, actions' = Happy_eyeballs.connect he (clock ())
          ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout ?resolve_retries
          ~id host ports
        in
        (he, actions @ actions')
  in
  List.fold_left fold (he, []) user's_actions

let await_actions_or_events t () =
  Miou.Mutex.protect t.mutex @@ fun () ->
  while Miou.Queue.is_empty t.queue do
    Miou.Condition.wait t.condition t.mutex
  done

exception Timeout

let with_timeout ~timeout:ts fn =
  let timeout () = Miou_unix.sleep ts; raise Timeout in
  let prm1 = Miou.async timeout in
  let prm0 = Miou.async fn in
  Miou.await_first [ prm0; prm1 ]

let continue t cont he =
  let fn () = match cont with
    | `Act -> with_timeout ~timeout:t.timer_interval  (await_actions_or_events t)
    | `Suspend ->
      try Ok (await_actions_or_events t ())
      with exn -> Error exn in
  match fn () with
  | Error Timeout -> (he, [], [])
  | Ok () ->
      let user's_actions_and_events = Miou.Queue.(to_list (transfer t.queue)) in
      Logd.debug (fun m ->
          m "got %d actions or events" (List.length user's_actions_and_events));
      let user's_actions, events =
        List.partition_map
          (function
            | #action as action -> Either.Left action
            | #event as event -> Either.Right event)
          user's_actions_and_events
      in
      Logd.debug (fun m ->
          m "got %d actions and %d events"
            (List.length user's_actions)
            (List.length events));
      let he, actions = to_actions t he user's_actions in
      (he, actions, events)
  | Error Miou.Cancelled -> (he, [], [])
  | Error exn ->
      Logd.err (fun m ->
          m "Got an unexpected exception (suspend): %s" (Printexc.to_string exn));
      raise exn

let rec clean_up prms =
  match Miou.care prms with
  | Some (Some prm) ->
      let _ = Miou.await prm in
      clean_up prms
  | Some None | None -> Miou.yield ()

let rec go t ~prms he () =
  Logd.debug (fun m -> m "daemon tick");
  clean_up prms;
  let he, cont, actions = Happy_eyeballs.timer he (clock ()) in
  List.iter (handle_one_action ~prms t) actions;
  let he, actions, events = continue t cont he in
  Logd.debug (fun m ->
      m "got %d action(s) and %d event(s)" (List.length actions)
        (List.length events));
  let he, actions =
    List.fold_left
      (fun (he, actions) event ->
        let he, actions' = Happy_eyeballs.event he (clock ()) (to_event t event) in
        (he, List.rev_append actions actions'))
      (he, actions) events
  in
  Logd.debug (fun m -> m "daemon handles %d action(s)" (List.length actions));
  List.iter (handle_one_action ~prms t) actions;
  go t ~prms he ()

let launch_daemon t he () =
  let prms = Miou.orphans () in
  Miou.call (go t ~prms he)

let connect_ip ?aaaa_timeout ?connect_delay ?connect_timeout t addrs =
  let state = Miou.Computation.create () in
  Miou.Mutex.protect t.mutex @@ fun () ->
  let connect_ip =
    { aaaa_timeout
    ; connect_delay
    ; connect_timeout
    ; state
    ; addrs } in
  Miou.Queue.enqueue t.queue (`Connect_ip connect_ip);
  Miou.Condition.signal t.condition;
  state

let connect_ip ?aaaa_timeout ?connect_delay ?connect_timeout t ips =
  try connect_ip ?aaaa_timeout ?connect_delay ?connect_timeout t ips
  with exn ->
    Logc.err (fun m ->
        m "Got an unexpected exception: %S" (Printexc.to_string exn));
    raise exn

type daemon = unit Miou.t

let create ?happy_eyeballs:(he= Happy_eyeballs.create (clock ())) ?(getaddrinfo= getaddrinfo)
  ?(timer_interval= Duration.of_ms 10) () =
  let t = create (Duration.to_f timer_interval) getaddrinfo in
  launch_daemon t he (), t

let error_injection =
  {text|
It's impossible to hot-load a new getaddrinfo into the happy-eyeballs instance.
|text}

let inject t getaddrinfo =
  if t.set
  then invalid_arg error_injection;
  t.getaddrinfo <- getaddrinfo;
  t.set <- true

let kill = Miou.cancel

let connect_ip ?aaaa_timeout ?connect_delay ?connect_timeout t ips =
  let state = connect_ip ?aaaa_timeout ?connect_delay ?connect_timeout t ips in
  match Miou.Computation.await state with
  | Ok (addr, fd) -> Ok (addr, Miou_unix.of_file_descr fd)
  | Error (Connection_failed (host, reason), _) ->
      error_msgf "Impossible to connect to %a: %s" Domain_name.pp host reason
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let connect_host ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout ?resolve_retries t host ports =
  let state = Miou.Computation.create () in
  let () =
    Miou.Mutex.protect t.mutex @@ fun () ->
    let connect =
      { aaaa_timeout
      ; connect_delay
      ; connect_timeout
      ; resolve_timeout
      ; resolve_retries
      ; state
      ; host
      ; ports } in
    Miou.Queue.enqueue t.queue (`Connect connect);
    Miou.Condition.signal t.condition
  in
  match Miou.Computation.await state with
  | Ok (addr, fd) -> Ok (addr, Miou_unix.of_file_descr fd)
  | Error (Connection_failed (host, reason), _) ->
      error_msgf "Impossible to connect to %a: %s" Domain_name.pp host reason
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let connect ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout ?resolve_retries t str ports =
  match Ipaddr.of_string str with
  | Ok ipaddr ->
    connect_ip ?aaaa_timeout ?connect_delay ?connect_timeout t
      (List.map (fun port -> (ipaddr, port)) ports)
  | Error _ -> (
      match Result.bind (Domain_name.of_string str) Domain_name.host with
      | Ok domain_name ->
        connect_host ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout ?resolve_retries
          t domain_name ports
      | Error _ -> error_msgf "Invalid endpoint: %S" str)
