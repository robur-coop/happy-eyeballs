
let src = Logs.Src.create "happy-eyeballs" ~doc:"Happy Eyeballs"
module Log = (val Logs.src_log src : Logs.LOG)

module IM = Map.Make(Int)

type conn_state =
  | Resolving
  | Waiting_for_aaaa of int64 * Dns.Rr_map.Ipv4_set.t (* TODO ensure non-empty list *)
  | Connecting of int64 * Ipaddr.t * Ipaddr.t list

type connection = {
  created : int64 ;
  port : int ;
  state : conn_state ;
}

type t = {
  started : int64 ;
  conns : connection IM.t Domain_name.Host_map.t ;
}

type action =
  | Resolve_a of [`host] Domain_name.t
  | Resolve_aaaa of [`host] Domain_name.t
  | Connect of [`host] Domain_name.t * int * Ipaddr.t * int
  | Connect_failed of [`host] Domain_name.t * int * int

let pp_action ppf = function
  | Resolve_a host -> Fmt.pf ppf "resolve A %a" Domain_name.pp host
  | Resolve_aaaa host -> Fmt.pf ppf "resolve AAAA %a" Domain_name.pp host
  | Connect (host, id, ip, port) ->
    Fmt.pf ppf "%u connect %a (using %a:%u)" id Domain_name.pp host
      Ipaddr.pp ip port
  | Connect_failed (host, id, port) ->
    Fmt.pf ppf "%u connect failed %a (port %u)" id Domain_name.pp host port

(* it likely makes sense to record resolving_failed events as well *)
type event =
  | Resolved_a of [`host] Domain_name.t * Dns.Rr_map.Ipv4_set.t
  | Resolved_aaaa of [`host] Domain_name.t * Dns.Rr_map.Ipv6_set.t
  | Connection_failed of [`host] Domain_name.t * int * Ipaddr.t * int
  | Connected of [`host] Domain_name.t * int * Ipaddr.t * int

let pp_event ppf = function
  | Resolved_a (host, ips) ->
    Fmt.pf ppf "resolved A %a: %a" Domain_name.pp host
      Fmt.(list ~sep:(unit ", ") Ipaddr.V4.pp)
      (Dns.Rr_map.Ipv4_set.elements ips)
  | Resolved_aaaa (host, ips) ->
    Fmt.pf ppf "resolved AAAA %a: %a" Domain_name.pp host
      Fmt.(list ~sep:(unit ", ") Ipaddr.V6.pp)
      (Dns.Rr_map.Ipv6_set.elements ips)
  | Connection_failed (host, id, ip, port) ->
    Fmt.pf ppf "%u connection to %a failed %a:%d" id Domain_name.pp host
      Ipaddr.pp ip port
  | Connected (host, id, ip, port) ->
    Fmt.pf ppf "%u connected to %a (using %a:%d)" id Domain_name.pp host
      Ipaddr.pp ip port

let create started = {
  started ;
  conns = Domain_name.Host_map.empty ;
}

let add_conn host id conn c =
  Domain_name.Host_map.update host
    (function
      | None -> Some (IM.singleton id conn)
      | Some cs -> Some (IM.add id conn cs))
    c

let aaaa_timeout = Duration.of_ms 50

let connect_timeout = Duration.of_sec 1

let tick now host id conn =
  let open Rresult.R.Infix in
  begin
    match conn.state with
    | Resolving -> Ok (Resolving, []) (* TODO retry on timeout *)
    | Waiting_for_aaaa (started, ips) ->
      Log.debug (fun m -> m "waiting_for_aaaa, %Lu > %Lu"
                    (Int64.sub now started) aaaa_timeout);
      if Int64.sub now started > aaaa_timeout then
        let ip = Dns.Rr_map.Ipv4_set.choose ips in
        let ips = Dns.Rr_map.Ipv4_set.remove ip ips in
        let ips =
          List.map (fun ip -> Ipaddr.V4 ip) (Dns.Rr_map.Ipv4_set.elements ips)
        in
        let ip = Ipaddr.V4 ip in
        Ok (Connecting (now, ip, ips), [ Connect (host, id, ip, conn.port) ])
      else
        Ok (Waiting_for_aaaa (started, ips), [])
    | Connecting (started, ip, ips) ->
      if Int64.sub now started > connect_timeout then
        match ips with
        | [] -> Error ()
        | ip :: ips ->
          Ok (Connecting (now, ip, ips), [ Connect (host, id, ip, conn.port) ])
      else
        Ok (Connecting (started, ip, ips), [])
  end >>| fun (state, actions) ->
  { conn with state }, actions

let timer t now =
  Log.debug (fun m -> m "timer");
  let conns, actions =
    Domain_name.Host_map.fold (fun host v (t, actions) ->
        let v, actions = IM.fold (fun id conn (acc, actions) ->
            match tick now host id conn with
            | Ok (conn, action) -> IM.add id conn acc, actions @ action
            | Error () -> acc, actions @ [ Connect_failed (host, id, conn.port) ]
          ) v (IM.empty, actions)
        in
        let t =
          if IM.cardinal v = 0 then t else Domain_name.Host_map.add host v t
        in
        t, actions) t.conns (Domain_name.Host_map.empty, [])
  in
  Log.debug (fun m -> m "timer %d actions" (List.length actions));
  { t with conns }, actions

let connect t now ~id host port =
  let conn = { created = now ; port ; state = Resolving } in
  { t with conns = add_conn host id conn t.conns },
  [ Resolve_aaaa host ; Resolve_a host ]

(* the idea is to first separate into V4 and V6 addresses, and then mix them *)
let mix
    ?first
    ?(ipv4 = Dns.Rr_map.Ipv4_set.empty)
    ?(ipv6 = Dns.Rr_map.Ipv6_set.empty) ips =
  let ipv4, ipv6 =
    List.fold_left (fun (ipv4, ipv6) -> function
        | Ipaddr.V4 ip -> Dns.Rr_map.Ipv4_set.add ip ipv4, ipv6
        | Ipaddr.V6 ip -> ipv4, Dns.Rr_map.Ipv6_set.add ip ipv6)
      (ipv4, ipv6) ips
  in
  let v4_len, v6_len =
    Dns.Rr_map.Ipv4_set.cardinal ipv4, Dns.Rr_map.Ipv6_set.cardinal ipv6
  in
  let v4, v6 =
    List.map (fun ip -> Ipaddr.V4 ip) (Dns.Rr_map.Ipv4_set.elements ipv4),
    List.map (fun ip -> Ipaddr.V6 ip) (Dns.Rr_map.Ipv6_set.elements ipv6)
  in
  match v4_len, v6_len with
  | 0, _ -> v6
  | _, 0 -> v4
  | _, _ ->
    let rec shuffle a b = function
      | 0 -> []
      | n -> match a, b with
        | [], _ -> shuffle v4 b n
        | _, [] -> shuffle a v6 n
        | hd :: tl, hd' :: tl' ->
          match first with
          | Some Ipaddr.V4 _ | None -> hd' :: hd :: shuffle tl tl' (pred n)
          | Some Ipaddr.V6 _ -> hd :: hd' :: shuffle tl tl' (pred n)
    in
    shuffle v4 v6 (max v4_len v6_len)

let event t now e =
  Log.debug (fun m -> m "received event %a" pp_event e);
  match e with
  | Resolved_a (name, ips) ->
    let conns =
      match Domain_name.Host_map.find name t.conns with
      | None ->
        Log.warn (fun m -> m "resolved %a, but no entry in conns"
                     Domain_name.pp name);
        t.conns
      | Some cs ->
        let cs = IM.fold (fun id c cs ->
            let state = match c.state with
              | Resolving -> Waiting_for_aaaa (now, ips)
              | Waiting_for_aaaa (ts, ips') ->
                Logs.warn (fun m -> m "already waiting for AAAA with %a"
                              Fmt.(list ~sep:(unit ", ") Ipaddr.V4.pp)
                              (Dns.Rr_map.Ipv4_set.elements ips'));
                Waiting_for_aaaa (ts, Dns.Rr_map.Ipv4_set.union ips' ips)
              | Connecting (ts, ip, ips') ->
                Connecting (ts, ip, mix ~first:ip ~ipv4:ips ips')
            in
            IM.add id { c with state } cs)
            cs IM.empty
        in
        Domain_name.Host_map.add name cs t.conns
    in
    { t with conns }, []
  | Resolved_aaaa (name, ips) ->
    let conns, actions =
      match Domain_name.Host_map.find name t.conns with
      | None ->
        Log.warn (fun m -> m "resolved %a, but no entry in conns"
                     Domain_name.pp name);
        t.conns, []
      | Some cs ->
        let cs, actions = IM.fold (fun id c (cs, actions) ->
            let state, actions' = match c.state with
              | Resolving ->
                let ips = mix ~ipv6:ips [] in
                let ip, ips = List.hd ips, List.tl ips in
                Connecting (now, ip, ips), [ Connect (name, id, ip, c.port) ]
              | Waiting_for_aaaa (_ts, ips') ->
                let ips = mix ~ipv4:ips' ~ipv6:ips [] in
                let ip, ips = List.hd ips, List.tl ips in
                Connecting (now, ip, ips), [ Connect (name, id, ip, c.port) ]
              | Connecting (ts, ip, ips') ->
                Connecting (ts, ip, mix ~first:ip ~ipv6:ips ips'), []
            in
            IM.add id { c with state } cs, actions @ actions')
            cs (IM.empty, [])
        in
        Domain_name.Host_map.add name cs t.conns, actions
    in
    { t with conns }, actions
  | Connection_failed (name, id, ip, _port) ->
    let conns, actions =
      match Domain_name.Host_map.find name t.conns with
      | None ->
        Log.warn (fun m -> m "connection failed to %a, but no entry in conns"
                     Domain_name.pp name);
        t.conns, []
      | Some cs ->
        match IM.find_opt id cs with
        | None ->
          Log.warn (fun m -> m "%u connection failed to %a, but no entry in IM"
                       id Domain_name.pp name);
          t.conns, []
        | Some c ->
          match c.state with
          | Connecting (_ts, ip', []) when Ipaddr.compare ip ip' = 0 ->
            let cs = IM.remove id cs in
            Domain_name.Host_map.add name cs t.conns,
            [ Connect_failed (name, id, c.port) ]
          | Connecting (_ts, ip', nip :: ips) when Ipaddr.compare ip ip' = 0 ->
            let state = Connecting (now, nip, ips) in
            let cs = IM.add id { c with state } cs in
            Domain_name.Host_map.add name cs t.conns,
            [ Connect (name, id, nip, c.port) ]
          | _ -> t.conns, []
    in
    { t with conns }, actions
  | Connected (name, id, _ip, _port) ->
    let conns =
      Domain_name.Host_map.update name (function
          | None -> None
          | Some xs ->
            let m = IM.remove id xs in
            if IM.cardinal m = 0 then None else Some m)
        t.conns
    in
    { t with conns }, []
