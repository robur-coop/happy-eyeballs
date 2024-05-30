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

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs_threaded.enable ()

let () = Miou_unix.run @@ fun () ->
  let daemon, t = Happy_eyeballs_miou_unix.create () in
  for _ = 0 to 5 do
    match Happy_eyeballs_miou_unix.connect t Sys.argv.(1) [ 80 ] with
    | Ok ((ipaddr, port), fd) ->
        Logs.info (fun m -> m "Connected to %a:%d" Ipaddr.pp ipaddr port);
        Miou_unix.close fd
    | Error (`Msg err) ->
        Logs.err (fun m -> m "%s" err)
  done;
  Happy_eyeballs_miou_unix.kill daemon
