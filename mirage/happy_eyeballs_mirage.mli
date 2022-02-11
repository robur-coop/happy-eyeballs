module type S = sig
  module DNS : Dns_client_mirage.S

  type t
  type flow

  val create : ?happy_eyeballs:Happy_eyeballs.t ->
    ?dns:DNS.t -> ?timer_interval:int64 -> DNS.Transport.stack -> t

  val connect_host : t -> [`host] Domain_name.t -> int list ->
    ((Ipaddr.t * int) * flow, [ `Msg of string ]) result Lwt.t

  val connect_ip : t -> (Ipaddr.t * int) list ->
    ((Ipaddr.t * int) * flow, [ `Msg of string ]) result Lwt.t

  val connect : t -> string -> int list ->
    ((Ipaddr.t * int) * flow, [ `Msg of string ]) result Lwt.t

  val connect_device :
    ?aaaa_timeout:int64 ->
    ?v6_connect_timeout:int64 ->
    ?connect_timeout:int64 ->
    ?resolve_timeout:int64 ->
    ?resolve_retries:int ->
    int64 ->
    DNS.t ->
    ?timer_interval:int64 -> DNS.Transport.stack -> t Lwt.t
end

module Make (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (S : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S with type Transport.stack = S.t)
  : S with type DNS.Transport.stack = S.t
       and type flow = S.TCP.flow
