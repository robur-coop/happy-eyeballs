module type S = sig
  type t

  type stack

  type flow

  type getaddrinfo = [ `A | `AAAA ] -> [ `host ] Domain_name.t -> (Ipaddr.Set.t, [ `Msg of string ]) result Lwt.t

  val create : ?happy_eyeballs:Happy_eyeballs.t ->
    ?getaddrinfo:getaddrinfo -> ?timer_interval:int64 -> stack -> t

  val inject : t -> getaddrinfo -> unit
  (** [inject t getaddrinfo] injects a {i new} domain-name resolver into the
      given happy-eyeballs instance. By default, the happy-eyeballs instance is
      not able to resolve hostnames. Use a [dns-client-mirage] instance at your
      convenience. *)

  val connect_host : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    [`host] Domain_name.t -> int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t
  (** [connect_host t host ports] establishes a connection to [host] on [ports]
      (tried in sequence). The timeouts and delays are specified in nanoseconds,
      and are by default the values defined when constructing [t].

      @raise Failure if [ports] is empty. *)

  val connect_ip : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> (Ipaddr.t * int) list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t
  (** [connect_ip t addresses] establishes a connection to [addresses]. The
      timeouts and delays are specified in nanoseconds, and are by default the
      values defined when constructing [t].

      @raise Failure if [addresses] is the empty list. *)

  val connect : t -> ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    string -> int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t
    (** [connect t host ports] establishes a connection to [host] on [ports],
        which may be a host name, or an IP address. The timeouts and delays are
        specified in nanoseconds, and are by default the values defined when
        constructing [t].

        @raise Failure if [ports] is the empty list. *)
end

module Make (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (S : Tcpip.Stack.V4V6) : sig
  include S
    with type flow = S.TCP.flow
     and type stack = S.t

  val connect_device : ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    ?timer_interval:int64 -> ?getaddrinfo:getaddrinfo -> stack -> t Lwt.t
end
