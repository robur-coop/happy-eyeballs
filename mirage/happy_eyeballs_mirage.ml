
(* Lwt tasks are spawned:
 - create starts an asynchronous timer task
 - the actions resulting from timer are scheduled in one separate task
 - the actions returned from Happy_eyeballs.connect/event are scheduled in
   respective separate tasks
*)

(* TODO - cancellation of connection attempts *)

let src = Logs.Src.create "happy-eyeballs.mirage" ~doc:"Happy Eyeballs Mirage"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (R : Mirage_random.S) (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (P : Mirage_clock.PCLOCK) (S : Mirage_stack.V4V6) = struct
  module DNS = Dns_client_mirage.Make(R)(T)(C)(S)

  type t = {
    dns : DNS.t ;
    stack : S.t ;
    mutable waiters : ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.u Happy_eyeballs.Waiter_map.t ;
    mutable he : Happy_eyeballs.t ;
    timer_condition : unit Lwt_condition.t ;
  }

  let he_timer = Duration.of_ms 10

  let try_connect stack ip port =
    let open Lwt.Infix in
    S.TCP.create_connection (S.tcp stack) (ip, port) >|= fun r ->
    Rresult.R.reword_error
      (fun err -> `Msg (Fmt.to_to_string S.TCP.pp_error err)) r

  let rec act t action =
    let open Lwt.Infix in
    Log.debug (fun m -> m "action %a" Happy_eyeballs.pp_action action);
    begin
      match action with
      | Happy_eyeballs.Resolve_a host ->
        begin
          DNS.getaddrinfo t.dns Dns.Rr_map.A host >|= function
          | Ok (_, res) ->
            let r =
              Dns.Rr_map.Ipv4_set.fold Ipaddr.V4.Set.add
                res Ipaddr.V4.Set.empty
            in
            Ok (Happy_eyeballs.Resolved_a (host, r))
          | Error _ -> Ok (Happy_eyeballs.Resolved_a_failed host)
        end
      | Happy_eyeballs.Resolve_aaaa host ->
        begin
          DNS.getaddrinfo t.dns Dns.Rr_map.Aaaa host >|= function
          | Ok (_, res) ->
            let r =
              Dns.Rr_map.Ipv6_set.fold Ipaddr.V6.Set.add
                res Ipaddr.V6.Set.empty
            in
            Ok (Happy_eyeballs.Resolved_aaaa (host, r))
          | Error _ -> Ok (Happy_eyeballs.Resolved_aaaa_failed host)
        end
      | Happy_eyeballs.Connect (host, id, (ip, port)) ->
        begin
          try_connect t.stack ip port >>= function
          | Ok flow ->
            let waiters, r = Happy_eyeballs.Waiter_map.find_and_remove id t.waiters in
            t.waiters <- waiters;
            begin match r with
              | Some waiter ->
                Lwt.wakeup_later waiter (Ok ((ip, port), flow));
                Lwt.return (Ok (Happy_eyeballs.Connected (host, id, (ip, port))))
              | None ->
                (* waiter already vanished *)
                S.TCP.close flow >>= fun () ->
                Lwt.return (Error ())
            end
          | Error _ ->
            Lwt.return (Ok (Happy_eyeballs.Connection_failed (host, id, (ip, port))))
        end
      | Happy_eyeballs.Connect_failed (_host, id) ->
        let waiters, r = Happy_eyeballs.Waiter_map.find_and_remove id t.waiters in
        t.waiters <- waiters;
        begin match r with
          | Some waiter ->
            Lwt.wakeup_later waiter (Error (`Msg "connection failed"));
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
      let he, actions = Happy_eyeballs.timer t.he (C.elapsed_ns ()) in
      t.he <- he ;
      match actions with
      | `Suspend ->
        timer t
      | `Act actions ->
        handle_timer_actions t actions ;
        T.sleep_ns he_timer >>= fun () ->
        loop ()
    in
    Lwt_condition.wait t.timer_condition >>= fun () ->
    loop ()

  let create stack =
    let dns = DNS.create stack
    and he = Happy_eyeballs.create (C.elapsed_ns ())
    and timer_condition = Lwt_condition.create ()
    in
    let t = { dns ; stack ; waiters = Happy_eyeballs.Waiter_map.empty ; he ; timer_condition } in
    Lwt.async (fun () -> timer t);
    t

  let handle_actions t actions =
    List.iter (fun a -> Lwt.async (fun () -> act t a)) actions

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
    r

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
                  Fmt.(list ~sep:(unit ", ") (pair ~sep:(unit ":") Ipaddr.pp int))
                  addresses
                  Duration.pp (Int64.sub (C.elapsed_ns ()) ts));
    r

  let connect t host ports =
    match Ipaddr.of_string host with
    | Ok ip -> connect_ip t (List.map (fun p -> (ip, p)) ports)
    | Error _ ->
      let open Lwt_result.Infix in
      Lwt_result.lift
        (let open Rresult.R.Infix in
         Domain_name.of_string host >>= fun dn ->
         Domain_name.host dn) >>= fun host ->
      connect_host t host ports
end
