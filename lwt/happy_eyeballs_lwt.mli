(** The type of the abstract state of happy eyeballs. *)
type t

type getaddrinfo = [ `A | `AAAA ] -> [ `host ] Domain_name.t -> (Ipaddr.Set.t, [ `Msg of string ]) result Lwt.t

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

val inject : getaddrinfo -> t -> unit
(** [inject getaddrinfo t] injects a {i new} domain-name resolver into the given
    happy-eyeballs instance. By default, the happy-eyeballs instance use
    {!val:Unix.getaddrinfo} to be able to resolve domain-name. However, the user
    can choose to use its own implementation of a DNS resolver (like
    [ocaml-dns]).

    So, the {i ceremony} for using happy-eyeballs with your own DNS resolver is
    to create a happy-eyeballs instance, obtain an instance that can resolve
    domain names (such as [ocaml-dns]) and inject the latter's implementation
    into our first happy-eyeballs instance:

    {[
      let _ =
        let happy_eyeballs = Happy_eyeballs_lwt.create () in
        let dns = Dns_client_lwt.create () in
        Happy_eyeballs_lwt.inject (Dns_client_lwt.getaddrinfo dns) happy_eyeballs;
        Happy_eyeballs_lwt.connect happy_eyeballs "robur.coop" [ 443 ]
        >>= function
        | Ok (_, fd) -> ...
        | Error _ -> ...
    ]} *)
