let src = Logs.Src.create "happy-eyeballs" ~doc:"Happy Eyeballs"
module Log = (val Logs.src_log src : Logs.LOG)

type conn_state =
  | Resolving
  | Waiting_for_aaaa of int64 * Ipaddr.V4.Set.t (* TODO ensure non-empty set *)
  | Connecting of int64 * (Ipaddr.t * int) list * (Ipaddr.t * int) list

type connection = {
  created : int64 ;
  state : conn_state ;
  ports : int list ;
  resolved : [ `none | `v4 | `v6 | `both ] ;
  resolve_left : int ;
  attempt : int ;
}

let resolve st ev = match st, ev with
  | `none, `v4 -> `v4
  | `none, `v6 -> `v6
  | `v6, `v4 -> `both
  | `v4, `v6 -> `both
  | x, _ -> x

module IM = Map.Make(Int)

type t = {
  aaaa_timeout : int64 ;
  connect_delay : int64 ;
  connect_timeout : int64 ;
  resolve_timeout : int64 ;
  started : int64 ;
  resolve_retries : int ;
  conns : connection IM.t Domain_name.Host_map.t ;
}

let resolve_timeout t = t.resolve_timeout

type id = int

type action =
  | Resolve_a of [`host] Domain_name.t
  | Resolve_aaaa of [`host] Domain_name.t
  | Connect of [`host] Domain_name.t * id * int * (Ipaddr.t * int)
  | Connect_failed of [`host] Domain_name.t * id * string

let host_or_ip v =
  match Ipaddr.of_domain_name v with
  | None -> Domain_name.to_string v
  | Some ip -> Ipaddr.to_string ip

let pp_action ppf = function
  | Resolve_a host -> Fmt.pf ppf "resolve A %a" Domain_name.pp host
  | Resolve_aaaa host -> Fmt.pf ppf "resolve AAAA %a" Domain_name.pp host
  | Connect (host, id, attempt, (ip, port)) ->
    Fmt.pf ppf "%u connect %s (using %a:%u), attempt %u" id (host_or_ip host)
         Ipaddr.pp ip port attempt
  | Connect_failed (host, id, reason) ->
    Fmt.pf ppf "%u connect failed %s: %s" id (host_or_ip host) reason

type event =
  | Resolved_a of [`host] Domain_name.t * Ipaddr.V4.Set.t
  | Resolved_aaaa of [`host] Domain_name.t * Ipaddr.V6.Set.t
  | Resolved_a_failed of [`host] Domain_name.t * string
  | Resolved_aaaa_failed of [`host] Domain_name.t * string
  | Connection_failed of [`host] Domain_name.t * id * (Ipaddr.t * int) * string
  | Connected of [`host] Domain_name.t * id * (Ipaddr.t * int)

let pp_event ppf = function
  | Resolved_a (host, ips) ->
    Fmt.pf ppf "resolved A %a: %a" Domain_name.pp host
      Fmt.(list ~sep:(any ", ") Ipaddr.V4.pp)
      (Ipaddr.V4.Set.elements ips)
  | Resolved_aaaa (host, ips) ->
    Fmt.pf ppf "resolved AAAA %a: %a" Domain_name.pp host
      Fmt.(list ~sep:(any ", ") Ipaddr.V6.pp)
      (Ipaddr.V6.Set.elements ips)
  | Resolved_a_failed (host, reason) ->
    Fmt.pf ppf "resolve A failed for %a: %s" Domain_name.pp host reason
  | Resolved_aaaa_failed (host, reason) ->
    Fmt.pf ppf "resolve AAAA failed for %a: %s" Domain_name.pp host reason
  | Connection_failed (host, id, (ip, port), reason) ->
    Fmt.pf ppf "%u connection to %s failed %a:%d: %s" id (host_or_ip host)
      Ipaddr.pp ip port reason
  | Connected (host, id, (ip, port)) ->
    Fmt.pf ppf "%u connected to %s (using %a:%d)" id (host_or_ip host)
      Ipaddr.pp ip port

let create
    ?(aaaa_timeout = Duration.of_ms 50)
    ?(connect_delay = Duration.of_ms 50)
    ?(connect_timeout = Duration.of_sec 10)
    ?(resolve_timeout = Duration.of_sec 1)
    ?(resolve_retries = 3)
    started =
  {
    aaaa_timeout ;
    connect_delay ;
    connect_timeout ;
    resolve_timeout ;
    resolve_retries ;
    started ;
    conns = Domain_name.Host_map.empty ;
  }

let add_conn host id conn c =
  Domain_name.Host_map.update host
    (function
      | None -> Some (IM.singleton id conn)
      | Some cs -> Some (IM.add id conn cs))
    c

let expand_list ips ports =
  List.flatten (List.map (fun ip -> List.map (fun p -> (ip, p)) ports) ips)

(* all input has been verified that ips and ports are non-empty. *)
let expand_list_split ips ports =
  match expand_list ips ports with
  | hd :: tl -> hd, tl
  | _ -> failwith "ips or ports are empty"

let tick t now host id conn =
  match conn.state with
  | Resolving when Int64.sub now conn.created > t.resolve_timeout ->
    begin
      let ok actions =
        Ok ({ conn with created = now ; resolve_left = conn.resolve_left - 1 },
            actions)
      in
      match conn.resolve_left <= 1, conn.resolved with
      | true, _ | _, `both -> Error ()
      | false, `none -> ok [ Resolve_a host ; Resolve_aaaa host ]
      | false, `v4 -> ok [ Resolve_aaaa host ]
      | false, `v6 -> ok [ Resolve_a host ]
    end
  | Waiting_for_aaaa (started, ips) when Int64.sub now started > t.aaaa_timeout ->
    let ips = List.map (fun ip -> Ipaddr.V4 ip) (Ipaddr.V4.Set.elements ips) in
    let dst, dsts = expand_list_split ips conn.ports in
    let state = Connecting (now, [ dst ], dsts)
    and attempt = conn.attempt + 1
    in
    Ok ({ conn with state ; attempt }, [ Connect (host, id, conn.attempt, dst) ])
  | Connecting (last_conn, active_conns, dsts) ->
    (* if there are further IP addresses, and there was no activity within
       connect_delay, start the next connection. *)
    if Int64.sub now conn.created > t.connect_timeout then
      Error ()
    else if Int64.sub now last_conn > t.connect_delay then
      (match dsts with
       | [] -> Ok (conn, [])
       | dst :: dsts ->
         let state = Connecting (now, dst :: active_conns, dsts)
         and attempt = conn.attempt + 1
         in
         Ok ({ conn with state ; attempt }, [ Connect (host, id, conn.attempt, dst) ]))
    else
      Ok (conn, [])
  | _ -> Ok (conn, [])

let timer t now =
  let conns, actions =
    Domain_name.Host_map.fold (fun host v (dm, actions) ->
        let v, actions = IM.fold (fun id conn (acc, actions) ->
            match tick t now host id conn with
            | Ok (conn, action) -> IM.add id conn acc, action @ actions
            | Error () -> acc, Connect_failed (host, id, "timeout") :: actions)
            v (IM.empty, actions)
        in
        let dm =
          if IM.cardinal v = 0 then dm else Domain_name.Host_map.add host v dm
        in
        dm, actions) t.conns (Domain_name.Host_map.empty, [])
  in
  (match actions with
   | [] when not (Domain_name.Host_map.is_empty conns) -> ()
   | _ ->
     Log.debug (fun m -> m "timer continue %B, %d actions: %a"
                   (not (Domain_name.Host_map.is_empty conns))
                   (List.length actions)
                   Fmt.(list ~sep:(any "@.") pp_action) actions));
  { t with conns },
  (if Domain_name.Host_map.is_empty conns then `Suspend else `Act),
  actions

let connect t now ~id host ports =
  Log.debug (fun m -> m "connect: id %d host %a" id Domain_name.pp host);
  if ports = [] then failwith "empty port list not supported";
  let conn = {
    created = now ;
    ports ;
    state = Resolving ;
    resolved = `none ;
    resolve_left = t.resolve_retries ;
    attempt = 0 ;
  } in
  let actions = [ Resolve_aaaa host ; Resolve_a host ] in
  Log.debug (fun m -> m "actions: %a" Fmt.(list ~sep:(any "@.") pp_action)
                actions);
  { t with conns = add_conn host id conn t.conns }, actions

let merge ?(ipv4 = Ipaddr.V4.Set.empty) ?(ipv6 = Ipaddr.V6.Set.empty) ips =
  List.fold_left (fun (ipv4, ipv6) -> function
      | Ipaddr.V4 ip -> Ipaddr.V4.Set.add ip ipv4, ipv6
      | Ipaddr.V6 ip -> ipv4, Ipaddr.V6.Set.add ip ipv6)
    (ipv4, ipv6) ips

let shuffle ?first v4 v6 =
  match List.length v4, List.length v6 with
  | 0, _ -> v6
  | _, 0 -> v4
  | v4l, v6l ->
    let rec shuffle a b = function
      | 0 -> []
      | n -> match a, b with
        | [], _ -> shuffle v4 b n
        | _, [] -> shuffle a v6 n
        | hd :: tl, hd' :: tl' ->
          match first with
          | Some Ipaddr.V6 _ -> hd :: hd' :: shuffle tl tl' (pred n)
          | None | Some Ipaddr.V4 _ -> hd' :: hd :: shuffle tl tl' (pred n)
    in
    shuffle v4 v6 (max v4l v6l)

(* the idea is to first separate into V4 and V6 addresses, and then mix them *)
let mix ?first ?ipv4 ?ipv6 ips =
  let ipv4, ipv6 = merge ?ipv4 ?ipv6 ips in
  let v4, v6 =
    Ipaddr.V4.Set.fold (fun ip acc -> Ipaddr.V4 ip :: acc) ipv4 [],
    Ipaddr.V6.Set.fold (fun ip acc -> Ipaddr.V6 ip :: acc) ipv6 []
  in
  shuffle ?first v4 v6

let mix_dsts ?(ipv4 = Ipaddr.V4.Set.empty) ?(ipv6 = Ipaddr.V6.Set.empty) ports dst dsts =
  let v4_present, v6_present = merge (List.map fst (dst @ dsts)) in
  let ipv4 = Ipaddr.V4.Set.diff ipv4 v4_present
  and ipv6 = Ipaddr.V6.Set.diff ipv6 v6_present
  in
  let v4_dsts, v6_dsts =
    List.fold_left (fun (ipv4, ipv6) -> function
        | Ipaddr.V4 _, _ as a -> a :: ipv4, ipv6
        | Ipaddr.V6 _, _ as a -> ipv4, a :: ipv6)
      ([], []) dsts
  in
  let v4s =
    expand_list
      (Ipaddr.V4.Set.fold (fun ip acc -> Ipaddr.V4 ip :: acc) ipv4 [])
      ports
  and v6s =
    expand_list
      (Ipaddr.V6.Set.fold (fun ip acc -> Ipaddr.V6 ip :: acc) ipv6 [])
      ports
  in
  let first = match dst with [] -> None | (ip, _) :: _ -> Some ip in
  shuffle ?first (List.rev v4_dsts @ v4s) (List.rev v6_dsts @ v6s)

let connect_ip t now ~id dsts =
  Log.debug (fun m -> m "connect_ip id %d dsts %a"
                id
                Fmt.(list ~sep:(any ", ") (pair ~sep:(any ":") Ipaddr.pp int)) dsts);
  let dst, dsts = match dsts with
    | dst :: dsts -> dst, dsts
    | [] -> failwith "addresses are empty"
  in
  let state = Connecting (now, [ dst ], dsts) in
  let conn = {
    created = now ;
    ports = [] ;
    state ;
    resolved = `both ;
    resolve_left = 0 ;
    attempt = 1 ;
  } in
  let host = Ipaddr.to_domain_name (fst dst) in
  let actions = [ Connect (host, id, 0, dst) ] in
  Log.debug (fun m -> m "actions: %a" Fmt.(list ~sep:(any "@.") pp_action)
                actions);
  { t with conns = add_conn host id conn t.conns }, actions

let event t now e =
  Log.debug (fun m -> m "received event %a" pp_event e);
  let t, actions =
    match e with
    | Resolved_a (name, ips) ->
      let conns, actions =
        match Domain_name.Host_map.find name t.conns with
        | None -> t.conns, []
        | Some cs ->
          let cs, actions = IM.fold (fun id c (cs, actions) ->
              let resolved = resolve c.resolved `v4 in
              let state, attempt, actions = match c.state with
                | Resolving when resolved = `both ->
                  let ips =
                    List.map (fun ip -> Ipaddr.V4 ip) (Ipaddr.V4.Set.elements ips)
                  in
                  let dst, dsts = expand_list_split ips c.ports in
                  Connecting (now, [ dst ], dsts), c.attempt + 1,
                  Connect (name, id, c.attempt, dst) :: actions
                | Resolving -> Waiting_for_aaaa (now, ips), c.attempt, actions
                | Waiting_for_aaaa (ts, ips') ->
                  Logs.warn (fun m -> m "already waiting for AAAA with %a"
                                Fmt.(list ~sep:(any ", ") Ipaddr.V4.pp)
                                (Ipaddr.V4.Set.elements ips'));
                  Waiting_for_aaaa (ts, Ipaddr.V4.Set.union ips' ips), c.attempt,
                  actions
                | Connecting (ts, dst, dsts) ->
                  let dsts = mix_dsts ~ipv4:ips c.ports dst dsts in
                  Connecting (ts, dst, dsts), c.attempt, actions
              in
              IM.add id { c with state ; resolved ; attempt } cs, actions)
              cs (IM.empty, [])
          in
          Domain_name.Host_map.add name cs t.conns, actions
      in
      { t with conns }, actions
    | Resolved_a_failed (name, reason) ->
      let conns, actions =
        match Domain_name.Host_map.find name t.conns with
        | None -> t.conns, []
        | Some cs ->
          let cs, actions = IM.fold (fun id c (cs, actions) ->
              let resolved = resolve c.resolved `v4 in
              match c.state with
              | Resolving when resolved = `both ->
                cs, Connect_failed (name, id, reason) :: actions
              | _ -> IM.add id { c with resolved } cs, actions)
              cs (IM.empty, [])
          in
          (if IM.is_empty cs then
             Domain_name.Host_map.remove name t.conns
           else
             Domain_name.Host_map.add name cs t.conns), actions
      in
      { t with conns }, actions
    | Resolved_aaaa (name, ips) ->
      let conns, actions =
        match Domain_name.Host_map.find name t.conns with
        | None -> t.conns, []
        | Some cs ->
          let cs, actions = IM.fold (fun id c (cs, actions) ->
              let resolved = resolve c.resolved `v6 in
              let state, attempt, actions' = match c.state with
                | Resolving ->
                  let ips = mix ~ipv6:ips [] in
                  let dst, dsts = expand_list_split ips c.ports in
                  Connecting (now, [ dst ], dsts), c.attempt + 1,
                  [ Connect (name, id, c.attempt, dst) ]
                | Waiting_for_aaaa (_ts, ips') ->
                  let ips = mix ~ipv4:ips' ~ipv6:ips [] in
                  let dst, dsts = expand_list_split ips c.ports in
                  Connecting (now, [ dst ], dsts), c.attempt + 1,
                  [ Connect (name, id, c.attempt, dst) ]
                | Connecting (ts, dst, dsts) ->
                  let dsts = mix_dsts ~ipv6:ips c.ports dst dsts in
                  Connecting (ts, dst, dsts), c.attempt, []
              in
              IM.add id { c with state ; resolved ; attempt } cs, actions @ actions')
              cs (IM.empty, [])
          in
          Domain_name.Host_map.add name cs t.conns, actions
      in
      { t with conns }, actions
    | Resolved_aaaa_failed (name, reason) ->
      let conns, actions =
        match Domain_name.Host_map.find name t.conns with
        | None -> t.conns, []
        | Some cs ->
          let cs, actions = IM.fold (fun id c (cs, actions) ->
              let resolved = resolve c.resolved `v6 in
              match c.state with
              | Resolving when resolved = `both ->
                cs, Connect_failed (name, id, reason) :: actions
              | Waiting_for_aaaa (_ts, ips) ->
                let ips =
                  List.map (fun ip -> Ipaddr.V4 ip) (Ipaddr.V4.Set.elements ips)
                in
                let dst, dsts = expand_list_split ips c.ports in
                let state = Connecting (now, [ dst ], dsts) in
                let attempt = c.attempt + 1 in
                IM.add id { c with state ; resolved ; attempt } cs,
                Connect (name, id, c.attempt, dst) :: actions
              | _ -> IM.add id { c with resolved } cs, actions)
              cs (IM.empty, [])
          in
          (if IM.is_empty cs then
             Domain_name.Host_map.remove name t.conns
           else
             Domain_name.Host_map.add name cs t.conns), actions
      in
      { t with conns }, actions
    | Connection_failed (name, id, (ip, port), reason) ->
      let conns, actions =
        match Domain_name.Host_map.find name t.conns with
        | None ->
          Log.warn (fun m -> m "connection failed to %s: %s; no entry in conns"
                       (host_or_ip name) reason);
          t.conns, []
        | Some cs ->
          match IM.find_opt id cs with
          | None ->
            Log.warn (fun m -> m "%u connection failed to %s: %s; no entry in IM"
                         id (host_or_ip name) reason);
            t.conns, []
          | Some c ->
            let not_failed (ip', port') = not (Ipaddr.compare ip ip' = 0 && port = port') in
            match c.state with
            | Connecting (ts, dst, []) ->
              let dst' = List.filter not_failed dst in
              begin match dst', c.resolved with
                | [], `both ->
                  let cs = IM.remove id cs in
                  Domain_name.Host_map.add name cs t.conns,
                  [ Connect_failed (name, id, reason) ]
                | [], _ ->
                  let state = Resolving in
                  let cs = IM.add id { c with state } cs in
                  Domain_name.Host_map.add name cs t.conns, []
                | dst', _ ->
                  let state = Connecting (ts, dst', []) in
                  let cs = IM.add id { c with state } cs in
                  Domain_name.Host_map.add name cs t.conns, []
              end
            | Connecting (ts, dst, ndst :: dsts) ->
              let dst' = List.filter not_failed dst in
              let state = Connecting (ts, ndst :: dst', dsts) in
              let attempt = c.attempt + 1 in
              let cs = IM.add id { c with state ; attempt } cs in
              Domain_name.Host_map.add name cs t.conns,
              [ Connect (name, id, c.attempt, ndst) ]
            | _ -> t.conns, []
      in
      { t with conns }, actions
    | Connected (name, id, (_ip, _port)) ->
      let conns =
        Domain_name.Host_map.update name (function
            | None ->
              Log.warn (fun m -> m "connected to an unexpected domain: %a"
                           Domain_name.pp name);
              None
            | Some xs ->
              let m = IM.remove id xs in
              if IM.cardinal m = 0 then None else Some m)
          t.conns
      in
      { t with conns }, []
  in
  Log.debug (fun m -> m "actions: %a" Fmt.(list ~sep:(any "@.") pp_action)
                actions);
  t, actions

module Waiter_map = struct
  include Map.Make(Int)

  let _id = ref 0

  let register v t =
    incr _id;
    let id = !_id in
    add id v t, id

  let find_and_remove id t =
    match find_opt id t with
    | None -> t, None
    | Some x -> remove id t, Some x
end
