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

type state =
  | In_progress
  | Connected of (Ipaddr.t * int) * Unix.file_descr
  | Failed of string

type entry = Happy_eyeballs.id * attempt * [ `host ] Domain_name.t * addr
and attempt = int
and addr = Ipaddr.t * int

type cancel = attempt * unit Miou.t

type action =
  [ `Connect_ip of state Atomic.t * addr list
  | `Connect of state Atomic.t * [ `host ] Domain_name.t * int list ]

type connected = [ `Connected of entry * Miou_unix.Ownership.file_descr ]

type event =
  [ connected
  | `Connection_failed of entry * string
  | `Resolution_v4 of
    [ `host ] Domain_name.t * (Ipaddr.V4.Set.t, [ `Msg of string ]) result
  | `Resolution_v6 of
    [ `host ] Domain_name.t * (Ipaddr.V6.Set.t, [ `Msg of string ]) result ]

and getaddrinfo = {
    getaddrinfo:
      'response 'a.
         'response Dns.Rr_map.key
      -> 'a Domain_name.t
      -> ('response, [ `Msg of string ]) result
}
[@@unboxed]

let dummy =
  let getaddrinfo _ _ = Error (`Msg "Not implemented") in
  { getaddrinfo }

(** The happy-eyeballs part. *)

type stack = {
    mutable cancel_connecting: cancel list Happy_eyeballs.Waiter_map.t
  ; mutable waiters: state Atomic.t Happy_eyeballs.Waiter_map.t
  ; condition: Miou.Condition.t
  ; mutex: Miou.Mutex.t
  ; queue: [ action | event ] Miou.Queue.t
  ; mutable set : bool
  ; mutable getaddrinfo: getaddrinfo
}

type happy = stack

let create_happy () =
  {
    cancel_connecting= Happy_eyeballs.Waiter_map.empty
  ; waiters= Happy_eyeballs.Waiter_map.empty
  ; condition= Miou.Condition.create ()
  ; mutex= Miou.Mutex.create ()
  ; queue= Miou.Queue.create ()
  ; set= false
  ; getaddrinfo= dummy
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

let getpeername fd = try Some (Unix.getpeername fd) with _exn -> None

let connect t ~prms:orphans host id attempt addr =
  let meta = (id, attempt, host, addr) in
  Logd.debug (fun m ->
      m "connect to %a (%d:%d)" Domain_name.pp host (Obj.magic id) attempt);
  let prm : unit Miou.t = Miou.call_cc ~orphans (try_connect t ~meta addr) in
  let entry = (attempt, prm) in
  t.cancel_connecting <-
    Happy_eyeballs.Waiter_map.update id
      (function None -> Some [ entry ] | Some cs -> Some (entry :: cs))
      t.cancel_connecting

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
      let msg =
        Fmt.str "Connection to %a failed: %s" Domain_name.pp host reason
      in
      let transition waiter =
        let set = Atomic.compare_and_set waiter In_progress (Failed msg) in
        if not set then begin
          match Atomic.get waiter with
          | Connected (_, fd) ->
              let sockaddr = getpeername fd in
              let fd = Miou_unix.of_file_descr fd in
              Logd.warn (fun m ->
                  m "close the file-descriptor of %a (%a): %s" Domain_name.pp
                    host
                    Fmt.(option ~none:(const string "<none>") pp_sockaddr)
                    sockaddr reason);
              Miou_unix.close fd;
              Atomic.set waiter (Failed msg)
          | In_progress -> Atomic.set waiter (Failed msg)
          | Failed _ -> ()
        end
      in
      Option.iter transition waiter
  | Happy_eyeballs.Resolve_a host ->
      let _ =
        Miou.call_cc ~orphans:prms @@ fun () ->
        let result =
          match t.getaddrinfo.getaddrinfo Dns.Rr_map.A host with
          | Ok (_ttl, res) -> Ok res
          | Error _ as err -> err
        in
        Miou.Mutex.protect t.mutex @@ fun () ->
        Miou.Queue.enqueue t.queue (`Resolution_v4 (host, result));
        Miou.Condition.signal t.condition
      in
      ()
  | Happy_eyeballs.Resolve_aaaa host ->
      let _ =
        Miou.call_cc ~orphans:prms @@ fun () ->
        let result =
          match t.getaddrinfo.getaddrinfo Dns.Rr_map.Aaaa host with
          | Ok (_ttl, res) -> Ok res
          | Error _ as err -> err
        in
        Miou.Mutex.protect t.mutex @@ fun () ->
        Miou.Queue.enqueue t.queue (`Resolution_v6 (host, result));
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
        (fun (att, prm) ->
          if att <> attempt then begin
            Logd.debug (fun m -> m "cancel (%d:%d)" (Obj.magic id) att);
            (* NOTE(dinosaure): 2 situations exists about cancellation:
               1) the given [prm] is fully resolved, this implies that the
                  ownership transfer has been made and that the event
                  [`Connected] has already been sent. This event will not find
                  its associated [waiter] (as it has already been filled by this
                  first current event) and we should [Miou.Ownership.close]
                  properly.
               2) the given [prm] is not yet finished, so cancellation will call
                  the socket finalizer and it will be closed cleanly. By this
                  way, we are sure that we don't have file-descriptor leaks. *)
            Miou.cancel prm
          end)
        (Option.value ~default:[] others);
      let waiters, waiter =
        Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
      in
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
            let connected = Connected (addr, fd) in
            let set = Atomic.compare_and_set waiter In_progress connected in
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
    | `Connect_ip (waiter, addrs) ->
        let waiters, id = Happy_eyeballs.Waiter_map.register waiter t.waiters in
        t.waiters <- waiters;
        let he, actions' = Happy_eyeballs.connect_ip he (clock ()) ~id addrs in
        (he, actions @ actions')
    | `Connect (waiter, host, ports) ->
        let waiters, id = Happy_eyeballs.Waiter_map.register waiter t.waiters in
        t.waiters <- waiters;
        let he, actions' =
          Happy_eyeballs.connect he (clock ()) ~id host ports
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
  let prm1 = Miou.call_cc timeout in
  let prm0 = Miou.call_cc fn in
  Miou.await_first [ prm0; prm1 ]

let continue t cont he =
  let fn () = match cont with
    | `Act -> with_timeout ~timeout:Duration.(to_f (of_ms 10)) (await_actions_or_events t)
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
        let he, actions' =
          Happy_eyeballs.event he (clock ()) (to_event t event)
        in
        (he, List.rev_append actions actions'))
      (he, actions) events
  in
  Logd.debug (fun m -> m "daemon handles %d action(s)" (List.length actions));
  List.iter (handle_one_action ~prms t) actions;
  go t ~prms he ()

let launch_daemon ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout
    ?resolve_retries t () =
  let prms = Miou.orphans () in
  let he =
    Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
      ?resolve_timeout ?resolve_retries (clock ())
  in
  Miou.call (go t ~prms he)

let connect_ip t ips =
  let waiter = Atomic.make In_progress in
  Miou.Mutex.protect t.mutex @@ fun () ->
  Miou.Queue.enqueue t.queue (`Connect_ip (waiter, ips));
  Miou.Condition.signal t.condition;
  waiter

let connect_ip t ips =
  try connect_ip t ips
  with exn ->
    Logc.err (fun m ->
        m "Got an unexpected exception: %S" (Printexc.to_string exn));
    raise exn

let rec wait value =
  Logc.debug (fun m -> m "wait for a connected socket");
  match Atomic.get value with
  | In_progress ->
      Miou_unix.sleep Duration.(to_f (of_ms 10));
      Miou.yield ();
      wait value
  | Connected (addr, fd) -> (addr, fd)
  | Failed msg -> failwith msg

let same_address ipaddr' port' = function
  | `Plaintext (ipaddr, port) -> Ipaddr.compare ipaddr ipaddr' = 0 && port = port'
  | `Tls (_, ipaddr, port) -> Ipaddr.compare ipaddr ipaddr' = 0 && port = port'

type daemon = unit Miou.t

let make ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout
    ?resolve_retries () =
  let v = create_happy () in
  launch_daemon ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout
    ?resolve_retries v (), v

let error_injection =
  {text|
It's impossible to hot-load a new getaddrinfo into the happy-eyeballs instance.
|text}

let inject_resolver ~getaddrinfo happy =
  if happy.set
  then failwith error_injection;
  happy.getaddrinfo <- getaddrinfo;
  happy.set <- true

let kill = Miou.cancel

(* The DNS part (what is required to make a [dns-client]). *)

type +'a io = 'a

type io_addr =
  [ `Plaintext of Ipaddr.t * int | `Tls of Tls.Config.client * Ipaddr.t * int ]

type t = {
    nameservers: io_addr list
  ; proto: Dns.proto
  ; timeout: float
  ; happy: happy
}

type context =
  { fd : [ `Udp of Miou_unix.file_descr
         | `Tcp of Miou_unix.file_descr
         | `Tls of Tls_miou_unix.t ]
  ; timeout : float }

let connect_to_nameservers t =
  match t.proto with
  | `Tcp ->
    let ip_of_nameserver = function
      | `Plaintext (ipaddr, port) -> (ipaddr, port)
      | `Tls (_, ipaddr, port) -> (ipaddr, port) in
    let ips = List.map ip_of_nameserver t.nameservers in
    let waiter = connect_ip t.happy ips in
    let prm = Miou.call_cc @@ fun () -> wait waiter in
    let ((ipaddr, port) as addr), fd = Miou.await_exn prm in
    begin match List.find (same_address ipaddr port) t.nameservers with
    | `Plaintext _ -> addr, `Tcp (Miou_unix.of_file_descr fd)
    | `Tls (config, _, _) ->
      try let fd = Tls_miou_unix.client_of_fd config (Miou_unix.of_file_descr fd) in
          (addr, `Tls fd)
      with exn -> Unix.close fd; raise exn end
  | `Udp ->
    let is_plaintext = function `Plaintext v -> Either.Left v | _ -> Either.Right () in
    let[@warning "-8"] (ipaddr, port) :: _, _ = List.partition_map is_plaintext t.nameservers in
    let proto_number, socket_type = Unix.((getprotobyname "udp").p_proto, SOCK_DGRAM) in
    let domain = match ipaddr with
      | Ipaddr.V4 _ -> Unix.PF_INET
      | Ipaddr.V6 _ -> Unix.PF_INET6 in
    let fd = Unix.socket domain socket_type proto_number in
    let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port) in
    let connect () =
      Unix.connect fd addr;
      ((ipaddr, port), `Udp (Miou_unix.of_file_descr fd)) in
    match with_timeout ~timeout:t.timeout connect with
    | Ok value -> value
    | Error exn -> Unix.close fd; raise exn

let connect_to_nameservers t =
  try Ok (connect_to_nameservers t)
  with Failure msg -> Error (`Msg msg)
     | Timeout -> error_msgf "Connection to nameservers (via UDP) timeout"
     | End_of_file -> error_msgf "Connection to nameservers (via TLS) impossible"
     | Unix.Unix_error (err, f, v) ->
       error_msgf "%s(%s): %s" f v (Unix.error_message err)
     | exn -> error_msgf "Unexpected exception: %s" (Printexc.to_string exn)

let nameservers { nameservers; proto; _ } = (proto, nameservers)
let bind x f = f x
let lift = Fun.id
let rng = Mirage_crypto_rng.generate ?g:None

let connect t =
  let ( >>= ) = Result.bind in
  connect_to_nameservers t >>= fun ((addr, port), fd) ->
  Logc.debug (fun m -> m "Connected to a nameserver %a:%d" Ipaddr.pp addr port);
  match fd with
  | `Tcp _ | `Tls _ -> Ok (`Tcp, { fd; timeout= t.timeout })
  | `Udp _ -> Ok (`Udp, { fd; timeout= t.timeout })

let send_recv_tls ~timeout ~id fd str =
  let send () = Tls_miou_unix.write fd str in
  let recv () =
    let rec go buf rx_len =
      let expected_len =
        if rx_len >= 2 then Some (Bytes.get_uint16_be buf 0) else None in
      match expected_len with
      | None ->
        let len = Tls_miou_unix.read fd buf ~off:rx_len in
        if rx_len + len >= 2 && len > 0 then go buf (rx_len + len)
        else failwith "TLS connection closed by nameserver"
      | Some expected_len when rx_len >= expected_len + 2 ->
        let id' = Bytes.get_uint16_be buf 2 in
        if id = id'
        then Cstruct.of_bytes buf ~off:0 ~len:(expected_len + 2)
        else
          let buf' = Bytes.make 2048 '\000' in
          let rx_len' = rx_len - (expected_len + 2) in
          Bytes.blit buf (expected_len + 2) buf' 0 rx_len';
          go buf' rx_len'
      | Some expected_len when Bytes.length buf >= expected_len + 2 ->
        let len = (expected_len + 2) - rx_len in
        Tls_miou_unix.really_read fd buf ~off:rx_len ~len;
        go buf (rx_len + len)
      | Some expected_len ->
        let buf' = Bytes.make (expected_len + 2) '\000' in
        Bytes.blit buf 0 buf' 0 rx_len;
        go buf rx_len in
    go (Bytes.make 2048 '\000') 0 in
  let ( >>= ) = Result.bind in
  match with_timeout ~timeout send >>= fun () ->
        with_timeout ~timeout recv with
  | Ok _ as rx -> rx
  | Error Timeout -> error_msgf "DNS request timeout"
  | Error (Failure msg) -> Error (`Msg msg)
  | Error (End_of_file | Tls_miou_unix.Closed_by_peer) ->
    error_msgf "End of file reading from nameserver"
  | Error exn ->
    error_msgf "Got an unexpected exception: %s"
      (Printexc.to_string exn)

let send_recv { fd; timeout } ({ Cstruct.len; _ } as tx) =
  if len > 4 then begin
    let str = Cstruct.to_string tx in
    match fd with
    | `Tls fd ->
      let id = String.get_int16_be str 2 in
      send_recv_tls ~timeout ~id fd str 
    | `Udp fd | `Tcp fd ->
        let fd = Miou_unix.to_file_descr fd in
        Unix.clear_nonblock fd;
        let send () =
          Logc.debug (fun m -> m "sending a dns packet to resolver");
          Unix.setsockopt_float fd Unix.SO_SNDTIMEO timeout;
          let len = Unix.send_substring fd str 0 (String.length str) [] in
          if len <> String.length str
          then failwith "Broken write to upstream nameserver" in
        let recv () =
          let buffer = Bytes.make 2048 '\000' in
          Unix.setsockopt_float fd Unix.SO_RCVTIMEO timeout;
          let len = Unix.recv fd buffer 0 (Bytes.length buffer) [] in
          (* TODO(dinosaure): should we check rx_len and continue until we got
             the full packet (only for tcp/ip)? *)
          if len > 0 && len <= Bytes.length buffer
          then Cstruct.of_bytes buffer ~len
          else failwith "Reading from nameserver socket failed" in
        let ( >>= ) = Result.bind in
        match with_timeout ~timeout send >>= fun () ->
              with_timeout ~timeout recv with
        | Ok _ as rx -> rx
        | Error Timeout -> error_msgf "DNS request timeout"
        | Error (Failure msg) -> Error (`Msg msg)
        | Error exn ->
            error_msgf "Got an unexpected exception: %s"
              (Printexc.to_string exn)
  end
  else error_msgf "Invalid context (data length <= 4)"

let close { fd; _ } = match fd with
  | `Tcp fd | `Udp fd -> Miou_unix.close fd
  | `Tls fd -> Tls_miou_unix.close fd

let of_ns ns = Int64.to_float ns /. 1_000_000_000.

let create ?nameservers ~timeout happy =
  let proto, nameservers =
    match nameservers with
    | None -> (`Udp, [ `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53) ])
    | Some (a, nss) -> (a, nss)
  in
  { nameservers; proto; timeout= of_ns timeout; happy }

external reraise : exn -> 'a = "%reraise"

let connect_ip t ips =
  let waiter = connect_ip t ips in
  let prm = Miou.call_cc @@ fun () -> wait waiter in
  match Miou.await prm with
  | Ok (addr, fd) -> Ok (addr, Miou_unix.of_file_descr fd)
  | Error (Failure msg) -> Error (`Msg msg)
  | Error exn -> reraise exn

let connect_host t host ports =
  let waiter = Atomic.make In_progress in
  let () =
    Miou.Mutex.protect t.mutex @@ fun () ->
    Miou.Queue.enqueue t.queue (`Connect (waiter, host, ports));
    Miou.Condition.signal t.condition
  in
  let prm = Miou.call_cc @@ fun () -> wait waiter in
  match Miou.await prm with
  | Ok (addr, fd) -> Ok (addr, Miou_unix.of_file_descr fd)
  | Error (Failure msg) -> Error (`Msg msg)
  | Error exn -> reraise exn

let connect_endpoint t str ports =
  match Ipaddr.of_string str with
  | Ok ipaddr -> connect_ip t (List.map (fun port -> (ipaddr, port)) ports)
  | Error _ -> (
      match Result.bind (Domain_name.of_string str) Domain_name.host with
      | Ok domain_name -> connect_host t domain_name ports
      | Error _ -> error_msgf "Invalid endpoint: %S" str)
