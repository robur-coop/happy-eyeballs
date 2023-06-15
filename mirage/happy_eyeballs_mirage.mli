module type S = sig
  module Transport : Dns_client.S
    with type io_addr = [ `Plaintext of Ipaddr.t * int  | `Tls of Tls.Config.client * Ipaddr.t * int ]
     and type +'a io = 'a Lwt.t

  type t
  type dns
  type flow

  val create : ?happy_eyeballs:Happy_eyeballs.t ->
    ?dns:dns -> ?timer_interval:int64 -> Transport.stack -> t

  val connect_host : t -> [`host] Domain_name.t -> int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t

  val connect_ip : t -> (Ipaddr.t * int) list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t

  val connect : t -> string -> int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t
end

module Make (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (S : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S with type Transport.stack = S.t) : sig
  include S
   with module Transport = DNS.Transport
    and type dns = DNS.t
    and type flow = S.TCP.flow

  (* note: the v6_connect_timeout is kept until 1.0.0 since it is referenced in mirage *)
  val connect_device : ?aaaa_timeout:int64 -> ?connect_delay:int64 ->
    ?v6_connect_timeout:int64 -> ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
    ?timer_interval:int64 -> dns -> Transport.stack -> t Lwt.t
end
