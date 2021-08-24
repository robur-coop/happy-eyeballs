
(* TODO
- rethink where & when to start tasks
- cancellation of connection attempts
*)

let he_timer = Duration.of_ms 10

let now = Mtime_clock.elapsed_ns

module IM = Map.Make(Int)

type t = {
  mutable waiters : ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.u IM.t ;
  mutable he : Happy_eyeballs.t ;
  dns : Dns_client_lwt.t ;
}

let _id = ref 0

let register_waiter t w =
  incr _id;
  let id = !_id in
  t.waiters <- IM.add id w t.waiters;
  id

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
  Logs.debug (fun m -> m "action %a" Happy_eyeballs.pp_action action);
  begin
    match action with
    | Happy_eyeballs.Resolve_a host ->
      begin
        Dns_client_lwt.getaddrinfo t.dns Dns.Rr_map.A host >|= function
        | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_a (host, res))
        | Error _ -> Ok (Happy_eyeballs.Resolved_a_failed host)
      end
    | Happy_eyeballs.Resolve_aaaa host ->
      begin
        Dns_client_lwt.getaddrinfo t.dns Dns.Rr_map.Aaaa host >|= function
        | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_aaaa (host, res))
        | Error _ -> Ok (Happy_eyeballs.Resolved_aaaa_failed host)
      end
    | Happy_eyeballs.Connect (host, id, (ip, port)) ->
      begin
        try_connect ip port >>= function
        | Ok fd ->
          begin match IM.find_opt id t.waiters with
            | Some waiter ->
              t.waiters <- IM.remove id t.waiters;
              Lwt.wakeup_later waiter (Ok ((ip, port), fd));
              Lwt.return (Ok (Happy_eyeballs.Connected (host, id, (ip, port))))
            | None ->
              (* waiter already vanished *)
              safe_close fd >>= fun () ->
              Lwt.return (Error ())
          end
        | Error _ ->
          Lwt.return (Ok (Happy_eyeballs.Connection_failed (host, id, (ip, port))))
      end
    | Happy_eyeballs.Connect_failed (_host, id) ->
      begin match IM.find_opt id t.waiters with
        | Some waiter ->
          t.waiters <- IM.remove id t.waiters;
          Lwt.wakeup_later waiter (Error (`Msg "connection failed"));
          Lwt.return (Error ())
        | None ->
          (* waiter already vanished *)
          Lwt.return (Error ())
      end
  end >>= function
  | Error _ -> Lwt.return_unit
  | Ok ev ->
    let he, actions = Happy_eyeballs.event t.he (now ()) ev in
    t.he <- he;
    Lwt_list.iter_p (act t) actions

let handle_timer_actions t actions =
  Lwt.async (fun () -> Lwt_list.iter_p (fun a -> act t a) actions)

let rec timer t =
  let open Lwt.Infix in
  Lwt.join [
    let he, actions = Happy_eyeballs.timer t.he (now ()) in
    t.he <- he ;
    handle_timer_actions t actions ;
    Lwt_unix.sleep (Duration.to_f he_timer)
  ] >>= fun () ->
  timer t

let create () =
  let waiters = IM.empty
  and he = Happy_eyeballs.create (now ())
  and dns = Dns_client_lwt.create ()
  in
  let t = { waiters ; he ; dns } in
  Lwt.async (fun () -> timer t);
  t

let handle_actions t actions =
  List.iter (fun a -> Lwt.async (fun () -> act t a)) actions

let connect_host t host ports =
  let waiter, notify = Lwt.task () in
  let id = register_waiter t notify in
  let ts = now () in
  let he, actions = Happy_eyeballs.connect t.he ts ~id host ports in
  t.he <- he;
  handle_actions t actions;
  let open Lwt.Infix in
  waiter >|= fun r ->
  Logs.debug (fun m -> m "connection %s to %a after %a"
                 (match r with Ok _ -> "ok" | Error _ -> "failed")
                 Domain_name.pp host Duration.pp (Int64.sub (now ()) ts));
  r

let connect_ip t ips ports =
  let waiter, notify = Lwt.task () in
  let id = register_waiter t notify in
  let ts = now () in
  let he, actions = Happy_eyeballs.connect_ip t.he ts ~id ips ports in
  t.he <- he;
  handle_actions t actions;
  let open Lwt.Infix in
  waiter >|= fun r ->
  Logs.debug (fun m -> m "connection %s to %a after %a"
                 (match r with Ok _ -> "ok" | Error _ -> "failed")
                 Fmt.(list ~sep:(unit ", ") Ipaddr.pp)
                 (Happy_eyeballs.Ip_set.elements ips)
                 Duration.pp (Int64.sub (now ()) ts));
  r

let connect t host ports =
  match Ipaddr.of_string host with
  | Ok ip -> connect_ip t (Happy_eyeballs.Ip_set.singleton ip) ports
  | Error _ ->
    let open Lwt_result.Infix in
    Lwt_result.lift
      (let open Rresult.R.Infix in
       Domain_name.of_string host >>= fun dn ->
       Domain_name.host dn) >>= fun host ->
    connect_host t host ports
