module Make (R : Mirage_random.S) (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (P : Mirage_clock.PCLOCK) (S : Mirage_stack.V4V6) : sig
  type t

  val create : ?aaaa_timeout:int64 -> ?connect_timeout:int64 ->
    ?resolve_timeout:int64 -> ?timer_interval:int64 -> S.t -> t

  val connect_host : t -> [`host] Domain_name.t -> int list ->
    ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.t

  val connect_ip : t -> (Ipaddr.t * int) list ->
    ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.t

  val connect : t -> string -> int list ->
    ((Ipaddr.t * int) * S.TCP.flow, [ `Msg of string ]) result Lwt.t
end
