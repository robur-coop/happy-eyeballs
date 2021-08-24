type t

val create : unit -> t

val connect : t -> string -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
