module Make (R : Mirage_random.S) (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (P : Mirage_clock.PCLOCK) (S : Tcpip.Stack.V4V6) : sig
  module DNS : module type of Dns_client_mirage.Make(R)(T)(C)(P)(S)

  type t

  val create : ?happy_eyeballs:Happy_eyeballs.t ->
    ?dns:DNS.t -> ?timer_interval:int64 -> S.t -> t

  val connect_host : t -> [`host] Domain_name.t -> int list ->
    ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.t

  val connect_ip : t -> (Ipaddr.t * int) list ->
    ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.t

  val connect : t -> string -> int list ->
    ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.t
end
