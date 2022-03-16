open Lwt.Infix

let jump () host ports =
  let t = Happy_eyeballs_lwt.create () in
  Lwt_main.run (
    Logs.app (fun m -> m "connecting to %s (on ports %a)" host
                 Fmt.(list ~sep:(any ", ") int) ports);
    Happy_eyeballs_lwt.connect t host ports >>= function
    | Ok ((ip, port), fd) ->
      Logs.app (fun m -> m "connected to %a:%d" Ipaddr.pp ip port);
      Lwt_unix.close fd >|= fun () ->
      Ok ()
    | Error `Msg msg as e ->
      Logs.app (fun m -> m "failed to connect %s" msg);
      Lwt.return e)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let host =
  let doc = "Host to connect to" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"HOST")

let port =
  let doc = "Ports to connect to" in
  Arg.(value & opt_all int [443;80] & info [ "port" ] ~doc ~docv:"PORT")

let cmd =
  let term = Term.(term_result (const jump $ setup_log $ host $ port))
  and info = Cmd.info "test" ~version:"%%VERSION_NUM%%"
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
