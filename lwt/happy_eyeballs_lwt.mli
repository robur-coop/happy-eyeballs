(** The type of the abstract state of happy eyeballs. *)
type t

(** The type of receiving an IPv4 or IPv6 address via DNS by providing a
    hostname. This is a pair of functions to avoid complexity in types or being
    too vague (and defer to run-time checking). *)
type getaddrinfo =
  ([ `host ] Domain_name.t -> (Ipaddr.V4.Set.t, [ `Msg of string ]) result Lwt.t) *
  ([ `host ] Domain_name.t -> (Ipaddr.V6.Set.t, [ `Msg of string ]) result Lwt.t)

val create : ?happy_eyeballs:Happy_eyeballs.t ->
  ?getaddrinfo:getaddrinfo ->
  ?timer_interval:int64 -> unit -> t
(** [create ~happy_eyeballs ~getaddrinfo ~timer_interval ()] creates an initial
    state of happy eyeballs with the specified timeouts in nanoseconds - the
    default for [timer_interval] is [Duration.of_ms 10]. *)

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
