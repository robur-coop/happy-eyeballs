(** The type of the abstract state of happy eyeballs. *)
type t

val create : unit -> t
(** [create ()] creates an initial state of happy eyeballs. *)

val connect_host : t -> [`host] Domain_name.t -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect_host t host ports] establishes a connection to [host] on [ports]
    (tried in sequence).

    @raise Failure if [ports] is empty. *)

val connect_ip : t -> (Ipaddr.t * int) list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect_ip t addresses] establishes a connection to [addresses].

    @raise Failure if [addresses] is the empty list. *)

val connect : t -> string -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect t host ports] establishes a connection to [host] on [ports], which
    may be a host name, or an IP address.

    @raise Failure if [ports] is the empty list. *)
