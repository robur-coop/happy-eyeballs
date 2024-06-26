(** The type of the abstract state of happy eyeballs. *)
type t

type getaddrinfo = [ `A | `AAAA ] -> [ `host ] Domain_name.t -> (Ipaddr.Set.t, [ `Msg of string ]) result Lwt.t

val create : ?happy_eyeballs:Happy_eyeballs.t ->
  ?getaddrinfo:getaddrinfo ->
  ?timer_interval:int64 -> unit -> t
(** [create ~happy_eyeballs ~getaddrinfo ~timer_interval ()] creates an initial
    state of happy eyeballs with the specified timeouts in nanoseconds - the
    default for [timer_interval] is [Duration.of_ms 10]. *)

val connect_host : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
  ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
  [`host] Domain_name.t -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect_host t host ports] establishes a connection to [host] on [ports]
    (tried in sequence). The timeouts and delays are specified in nanoseconds,
    and are by default the values defined when constructing [t].

    @raise Failure if [ports] is empty. *)

val connect_ip : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
  ?connect_timeout:int64 -> (Ipaddr.t * int) list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect_ip t addresses] establishes a connection to [addresses]. The
    timeouts and delays are specified in nanoseconds, and are by default the
    values defined when constructing [t].

    @raise Failure if [addresses] is the empty list. *)

val connect : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
  ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
  string -> int list ->
  ((Ipaddr.t * int) * Lwt_unix.file_descr, [ `Msg of string ]) result Lwt.t
(** [connect t host ports] establishes a connection to [host] on [ports], which
    may be a host name, or an IP address. The timeouts and delays are specified
    in nanoseconds, and are by default the values defined when constructing [t].

    @raise Failure if [ports] is the empty list. *)

val inject : t -> getaddrinfo -> unit
(** [inject t getaddrinfo] injects a {i new} domain-name resolver into the given
    happy-eyeballs instance. By default, the happy-eyeballs instance use
    {!val:Lwt_unix.getaddrinfo} to be able to resolve domain-name. However, the
    user can choose to use its own implementation of a DNS resolver (like
    [ocaml-dns]).

    So, the {i ceremony} for using happy-eyeballs with your own DNS resolver is
    to create a happy-eyeballs instance, obtain an instance that can resolve
    domain names (such as [ocaml-dns]) and inject the latter's implementation
    into our first happy-eyeballs instance:

    {[
      let _ =
        let dns = Dns_client_lwt.create () in
        let happy_eyeballs = Dns_client_lwt.create_happy_eyeballs dns in
        Happy_eyeballs_lwt.connect happy_eyeballs "robur.coop" [ 443 ]
        >>= function
        | Ok (_, fd) -> ...
        | Error _ -> ...
    ]} *)
