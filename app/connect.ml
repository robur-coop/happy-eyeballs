let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Cyan int) (Stdlib.Domain.self () :> int)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Logs_threaded.enable ()

let run quiet domains host port =
  let host, port = match host, port with
    | `Domain_name (host, Some port), _port' -> Domain_name.to_string host, port
    | `Domain_name (host, None), port -> Domain_name.to_string host, port
    | `Ipaddr (ipaddr, Some port), _port' -> Ipaddr.to_string ipaddr, port
    | `Ipaddr (ipaddr, None), port -> Ipaddr.to_string ipaddr, port in
  Miou_unix.run ~domains @@ fun () ->
  let daemon, t = Happy_eyeballs_miou_unix.create () in
  begin match Happy_eyeballs_miou_unix.connect t host [ port ] with
    | Ok ((ipaddr, port), fd) ->
        Logs.info (fun m -> m "Connected to %a:%d" Ipaddr.pp ipaddr port);
        if not quiet then Fmt.pr "%a:%d\n%!" Ipaddr.pp ipaddr port;
        Miou_unix.close fd
    | Error (`Msg err) ->
        Logs.err (fun m -> m "%s" err)
  end;
  Happy_eyeballs_miou_unix.kill daemon;
  `Ok 0

open Cmdliner

let verbosity =
  let env = Cmd.Env.info "CONNECT_LOGS" in
  Logs_cli.level ~env ()

let renderer =
  let env = Cmd.Env.info "CONNECT_FMT" in
  Fmt_cli.style_renderer ~env ()

let utf_8 =
  let doc = "Allow us to emit UTF-8 characters." in
  let env = Cmd.Env.info "CONNECT_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  Logs.set_level level;
  let reporter = reporter Fmt.stderr in
  Logs.set_reporter reporter;
  Option.is_none level

let term_setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let is_digit = function '0' .. '9' -> true | _ -> false
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let host =
  let doc = "The host (it can be an IP address or a domain name)." in
  let host str =
    let ( let* ) = Result.bind in
    let domain_name =
      let sstr = String.split_on_char ':' str in
      match List.rev sstr with
      | port :: domain_name when String.for_all is_digit port ->
        let str = String.concat ":" (List.rev domain_name) in
        let* domain_name = Domain_name.of_string str in
        let* domain_name = Domain_name.host domain_name in
        Ok (domain_name, Some (int_of_string port))
      | _ ->
        let* domain_name = Domain_name.of_string str in
        let* domain_name = Domain_name.host domain_name in
        Ok (domain_name, None) in
    let ipaddr =
      let* ipaddr, port = Ipaddr.with_port_of_string ~default:(-1) str in
      if port = (-1) then Ok (ipaddr, None) else Ok (ipaddr, Some port) in
    match domain_name, ipaddr with
    | Ok a, Error _ -> Ok (`Domain_name a)
    | Error _, Ok a -> Ok (`Ipaddr a)
    | Ok a, _ -> Ok (`Domain_name a)
    | Error _, Error _ -> error_msgf "Invalid host: %S" str in
  let pp ppf = function
    | `Domain_name (v, None) -> Domain_name.pp ppf v
    | `Domain_name (v, Some port) -> Fmt.pf ppf "%a:%d" Domain_name.pp v port
    | `Ipaddr (v, None) -> Ipaddr.pp ppf v
    | `Ipaddr (v, Some port) -> Fmt.pf ppf "%a:%d" Ipaddr.pp v port in
  let host = Arg.conv (host, pp) in
  Arg.(required & pos 0 (some host) None & info [] ~doc ~docv:"<host>")

let domains =
  let doc = "The number of domains that we can use." in
  Arg.(value & opt int 1 & info [ "d"; "domains" ] ~doc)

let port =
  let doc = "The port where we want to connect." in
  Arg.(value & opt int 80 & info [ "p"; "port" ] ~doc)

let term = Term.(ret (const run $ term_setup_logs $ domains $ host $ port))

let cmd =
  let doc = "A simple program which try to connect you to the given service." in
  let man = [] in
  Cmd.v (Cmd.info "connect" ~doc ~man) term

let () = exit (Cmd.eval' cmd)


