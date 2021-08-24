(** The type of the abstract state of happy eyeballs. *)
type t

val create : unit -> t
(** [create ()] creates an initial state of happy eyeballs. *)

val connect_host : t -> [`host] Domain_name.t -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect_host t host ports] establishes a connection to [host] on [ports]
    (tried in sequence). *)

val connect_ip : t -> Happy_eyeballs.Ip_set.t -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect_ip t ips ports] establishes a connection to [ips] on [ports]. *)

val connect : t -> string -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect t host ports] establishes a connection to [host] on [ports], which
    may be a host name, or an IP address. *)
