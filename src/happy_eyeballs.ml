let src = Logs.Src.create "happy-eyeballs" ~doc:"Happy Eyeballs"
module Log = (val Logs.src_log src : Logs.LOG)

(* A connection is attempted to [set of ips] * [sequence of ports], with the
   first successful socket being returned.

   The [set of ips] is either provided manually or by DNS resolution.
   Connections to IPv6 addresses is prefered, and connection attempts are mixed
   between IP protocols (first, an IPv6, then IPv4, then IPv6, ...) if
   available. If only one protocol family is available, this is used. *)

(* The state machine of a connection, constructors
   - connect (initial state: Resolving, actions: Resolve_a & Resolve_aaaa)
   - connect_ip (initial state: Connecting, action: Connect)

   Since resolution is done in parallel for IPv4 and IPv6 (and both may result
   in failure), that is tracked as well (in the "resolved" field of a
   connection, this transition is done before state transition):
    `none --[`v4]--> `v4
    `none --[`v6]--> `v6
    `v6 --[`v4]--> `both
    `v4 --[`v6]--> `both

   *Resolving* (resolved=`none)--[Resolved_a]--> (resolved:=`v4) Waiting_for_aaaa
   *Resolving* (resolved=`v6)--[Resolved_a]--> (resolved:=`both) Connecting
   *Resolving* (resolved=`none)--[Resolved_a_failed]--> (resolved:=`v4) Resolving
   *Resolving* (resolved=`v6--[Resolved_a_failed]--> (resolved:=`both) FAIL

   *Resolving* (resolved=`none|`v4)--[Resolved_aaaa]--> Connecting
   *Resolving* (resolved=`none)--[Resolved_aaaa_failed]--> (resolved:=`v6) Resolving
   *Resolving* (resolved=`v4)--[Resolved_aaaa_failed]--> (resolved:=`both) FAIL

   *Resolving* --[resolve_timeout]--> FAIL

   *Waiting_for_aaaa* --[Resolved_aaaa/Resolved_aaaa_failed/aaaa_timeout]--> Connecting
   *Waiting_for_aaaa* --[Resolved_a/Resolved_a_failed]--> Waiting_for_aaaa

   *Connecting* --[Resolved_a/Resolved_a_failed/Resolved_aaaa/Resolved_aaaa_failed]--> Connecting
   *Connecting* (more options available) --[Connection_failed/connect_timeout]--> Connecting (next)
   *Connecting* (resolved=`v6, no more options available) --[Connection_failed]--> Resolving
   *Connecting* (resolved=`both, no more options available) --[Connection_failed/connect_timeout]--> FAIL

   *Connecting* --[Connected]--> SUCCESS!

*)

type conn_state =
  | Resolving
  | Waiting_for_aaaa of int64 * Ipaddr.V4.Set.t (* TODO ensure non-empty set *)
  | Connecting of int64 * (Ipaddr.t * int) * (Ipaddr.t * int) list

type connection = {
  created : int64 ;
  state : conn_state ;
  ports : int list ;
  resolved : [ `none | `v4 | `v6 | `both ] ;
}

let resolve st ev = match st, ev with
  | `none, `v4 -> `v4
  | `none, `v6 -> `v6
  | `v6, `v4 -> `both
  | `v4, `v6 -> `both
  | x, _ -> x

module IM = Map.Make(Int)

type t = {
  started : int64 ;
  conns : connection IM.t Domain_name.Host_map.t ;
}

type action =
  | Resolve_a of [`host] Domain_name.t
  | Resolve_aaaa of [`host] Domain_name.t
  | Connect of [`host] Domain_name.t * int * (Ipaddr.t * int)
  | Connect_failed of [`host] Domain_name.t * int

let pp_action ppf = function
  | Resolve_a host -> Fmt.pf ppf "resolve A %a" Domain_name.pp host
  | Resolve_aaaa host -> Fmt.pf ppf "resolve AAAA %a" Domain_name.pp host
  | Connect (host, id, (ip, port)) ->
    Fmt.pf ppf "%u connect %a (using %a:%u)" id Domain_name.pp host
      Ipaddr.pp ip port
  | Connect_failed (host, id) ->
    Fmt.pf ppf "%u connect failed %a" id Domain_name.pp host

(* it likely makes sense to record resolving_failed events as well *)
type event =
  | Resolved_a of [`host] Domain_name.t * Ipaddr.V4.Set.t
  | Resolved_aaaa of [`host] Domain_name.t * Ipaddr.V6.Set.t
  | Resolved_a_failed of [`host] Domain_name.t
  | Resolved_aaaa_failed of [`host] Domain_name.t
  | Connection_failed of [`host] Domain_name.t * int * (Ipaddr.t * int)
  | Connected of [`host] Domain_name.t * int * (Ipaddr.t * int)

let pp_event ppf = function
  | Resolved_a (host, ips) ->
    Fmt.pf ppf "resolved A %a: %a" Domain_name.pp host
      Fmt.(list ~sep:(unit ", ") Ipaddr.V4.pp)
      (Ipaddr.V4.Set.elements ips)
  | Resolved_aaaa (host, ips) ->
    Fmt.pf ppf "resolved AAAA %a: %a" Domain_name.pp host
      Fmt.(list ~sep:(unit ", ") Ipaddr.V6.pp)
      (Ipaddr.V6.Set.elements ips)
  | Resolved_a_failed host ->
    Fmt.pf ppf "resolve A failed for %a" Domain_name.pp host
  | Resolved_aaaa_failed host ->
    Fmt.pf ppf "resolve AAAA failed for %a" Domain_name.pp host
  | Connection_failed (host, id, (ip, port)) ->
    Fmt.pf ppf "%u connection to %a failed %a:%d" id Domain_name.pp host
      Ipaddr.pp ip port
  | Connected (host, id, (ip, port)) ->
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

let resolve_timeout = Duration.of_sec 1

let expand_list ips ports =
  List.concat_map (fun ip -> List.map (fun p -> (ip, p)) ports) ips

(* all input has been verified that ips and ports are non-empty. *)
let expand_list_split ips ports =
  match expand_list ips ports with
  | hd :: tl -> hd, tl
  | _ -> failwith "ips or ports are empty"

let tick now host id conn =
  match
    match conn.state with
    | Resolving when Int64.sub now conn.created > resolve_timeout ->
      Error () (* TODO retry resolution *)
    | Waiting_for_aaaa (started, ips) when Int64.sub now started > aaaa_timeout ->
      let ips = List.map (fun ip -> Ipaddr.V4 ip) (Ipaddr.V4.Set.elements ips) in
      let dst, dsts = expand_list_split ips conn.ports in
      Ok (Connecting (now, dst, dsts), [ Connect (host, id, dst) ])
    | Connecting (started, _dst, dsts) when Int64.sub now started > connect_timeout->
      (* TODO cancel previous connection attempt *)
      (match dsts with
       | [] -> Error ()
       | dst :: dsts -> Ok (Connecting (now, dst, dsts), [ Connect (host, id, dst) ]))
    | x -> Ok (x, [])
  with
  | Ok (state, actions) -> Ok ({ conn with state }, actions)
  | Error () -> Error ()

let timer t now =
  Log.debug (fun m -> m "timer");
  let conns, actions =
    Domain_name.Host_map.fold (fun host v (t, actions) ->
        let v, actions = IM.fold (fun id conn (acc, actions) ->
            match tick now host id conn with
            | Ok (conn, action) -> IM.add id conn acc, actions @ action
            | Error () -> acc, actions @ [ Connect_failed (host, id) ]
          ) v (IM.empty, actions)
        in
        let t =
          if IM.cardinal v = 0 then t else Domain_name.Host_map.add host v t
        in
        t, actions) t.conns (Domain_name.Host_map.empty, [])
  in
  Log.debug (fun m -> m "timer %d actions" (List.length actions));
  { t with conns },
  if Domain_name.Host_map.is_empty conns then
    (assert (actions = []); `Suspend)
  else
    `Act actions

let connect t now ~id host ports =
  if ports = [] then failwith "empty port list not supported";
  let conn = { created = now ; ports ; state = Resolving ; resolved = `none } in
  { t with conns = add_conn host id conn t.conns },
  [ Resolve_aaaa host ; Resolve_a host ]

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
  let v4_present, v6_present = merge (List.map fst (dst :: dsts)) in
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
  shuffle ~first:(fst dst) (List.rev v4_dsts @ v4s) (List.rev v6_dsts @ v6s)

let connect_ip t now ~id dsts =
  let dst, dsts = match dsts with
    | dst :: dsts -> dst, dsts
    | [] -> failwith "addresses are empty"
  in
  let state = Connecting (now, dst, dsts) in
  let conn = { created = now ; ports = [] ; state ; resolved = `both } in
  let host = Ipaddr.to_domain_name (fst dst) in
  { t with conns = add_conn host id conn t.conns },
  [ Connect (host, id, dst) ]

let event t now e =
  Log.debug (fun m -> m "received event %a" pp_event e);
  match e with
  | Resolved_a (name, ips) ->
    let conns, actions =
      match Domain_name.Host_map.find name t.conns with
      | None ->
        Log.warn (fun m -> m "resolved %a, but no entry in conns"
                     Domain_name.pp name);
        t.conns, []
      | Some cs ->
        let cs, actions = IM.fold (fun id c (cs, actions) ->
            let resolved = resolve c.resolved `v4 in
            let state, actions = match c.state with
              | Resolving when resolved = `both ->
                let ips =
                  List.map (fun ip -> Ipaddr.V4 ip) (Ipaddr.V4.Set.elements ips)
                in
                let dst, dsts = expand_list_split ips c.ports in
                Connecting (now, dst, dsts), Connect (name, id, dst) :: actions
              | Resolving -> Waiting_for_aaaa (now, ips), actions
              | Waiting_for_aaaa (ts, ips') ->
                Logs.warn (fun m -> m "already waiting for AAAA with %a"
                              Fmt.(list ~sep:(unit ", ") Ipaddr.V4.pp)
                              (Ipaddr.V4.Set.elements ips'));
                Waiting_for_aaaa (ts, Ipaddr.V4.Set.union ips' ips), actions
              | Connecting (ts, dst, dsts) ->
                let dsts = mix_dsts ~ipv4:ips c.ports dst dsts in
                Connecting (ts, dst, dsts), actions
            in
            IM.add id { c with state ; resolved } cs, actions)
            cs (IM.empty, [])
        in
        Domain_name.Host_map.add name cs t.conns, actions
    in
    { t with conns }, actions
  | Resolved_a_failed name ->
    let conns, actions =
      match Domain_name.Host_map.find name t.conns with
      | None ->
        Log.warn (fun m -> m "resolve A %a failed, but no entry in conns"
                     Domain_name.pp name);
        t.conns, []
      | Some cs ->
        let cs, actions = IM.fold (fun id c (cs, actions) ->
            let resolved = resolve c.resolved `v4 in
            match c.state with
            | Resolving when resolved = `both ->
              cs, Connect_failed (name, id) :: actions
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
      | None ->
        Log.warn (fun m -> m "resolved %a, but no entry in conns"
                     Domain_name.pp name);
        t.conns, []
      | Some cs ->
        let cs, actions = IM.fold (fun id c (cs, actions) ->
            let resolved = resolve c.resolved `v6 in
            let state, actions' = match c.state with
              | Resolving ->
                let ips = mix ~ipv6:ips [] in
                let dst, dsts = expand_list_split ips c.ports in
                Connecting (now, dst, dsts), [ Connect (name, id, dst) ]
              | Waiting_for_aaaa (_ts, ips') ->
                let ips = mix ~ipv4:ips' ~ipv6:ips [] in
                let dst, dsts = expand_list_split ips c.ports in
                Connecting (now, dst, dsts), [ Connect (name, id, dst) ]
              | Connecting (ts, dst, dsts) ->
                let dsts = mix_dsts ~ipv6:ips c.ports dst dsts in
                Connecting (ts, dst, dsts), []
            in
            IM.add id { c with state ; resolved } cs, actions @ actions')
            cs (IM.empty, [])
        in
        Domain_name.Host_map.add name cs t.conns, actions
    in
    { t with conns }, actions
  | Resolved_aaaa_failed name ->
    let conns, actions =
      match Domain_name.Host_map.find name t.conns with
      | None ->
        Log.warn (fun m -> m "resolve AAAA %a failed, but no entry in conns"
                     Domain_name.pp name);
        t.conns, []
      | Some cs ->
        let cs, actions = IM.fold (fun id c (cs, actions) ->
            let resolved = resolve c.resolved `v6 in
            match c.state with
            | Resolving when resolved = `both ->
              cs, Connect_failed (name, id) :: actions
            | Waiting_for_aaaa (_ts, ips) ->
              let ips =
                List.map (fun ip -> Ipaddr.V4 ip) (Ipaddr.V4.Set.elements ips)
              in
              let dst, dsts = expand_list_split ips c.ports in
              let state = Connecting (now, dst, dsts) in
              IM.add id { c with state ; resolved } cs,
              Connect (name, id, dst) :: actions
            | _ -> IM.add id { c with resolved } cs, actions)
            cs (IM.empty, [])
        in
        (if IM.is_empty cs then
           Domain_name.Host_map.remove name t.conns
         else
           Domain_name.Host_map.add name cs t.conns), actions
    in
    { t with conns }, actions
  | Connection_failed (name, id, (ip, port)) ->
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
          let is_dst (ip', port') = Ipaddr.compare ip ip' = 0 && port = port' in
          match c.state with
          | Connecting (_ts, dst, []) when is_dst dst && c.resolved = `both ->
            let cs = IM.remove id cs in
            Domain_name.Host_map.add name cs t.conns,
            [ Connect_failed (name, id) ]
          | Connecting (_ts, dst, []) when is_dst dst ->
            let state = Resolving in
            let cs = IM.add id { c with state } cs in
            Domain_name.Host_map.add name cs t.conns, []
          | Connecting (_ts, dst, ndst :: dsts) when is_dst dst ->
            let state = Connecting (now, ndst, dsts) in
            let cs = IM.add id { c with state } cs in
            Domain_name.Host_map.add name cs t.conns,
            [ Connect (name, id, ndst) ]
          | _ -> t.conns, []
    in
    { t with conns }, actions
  | Connected (name, id, (_ip, _port)) ->
    let conns =
      Domain_name.Host_map.update name (function
          | None -> None
          | Some xs ->
            let m = IM.remove id xs in
            if IM.cardinal m = 0 then None else Some m)
        t.conns
    in
    { t with conns }, []

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
