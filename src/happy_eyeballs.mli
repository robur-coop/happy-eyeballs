
type t

type action =
  | Resolve_a of [`host] Domain_name.t
  | Resolve_aaaa of [`host] Domain_name.t
  | Connect of [`host] Domain_name.t * int * Ipaddr.t * int
  | Connect_failed of [`host] Domain_name.t * int * int

val pp_action : action Fmt.t

type event =
  | Resolved_a of [`host] Domain_name.t * Dns.Rr_map.Ipv4_set.t
  | Resolved_aaaa of [`host] Domain_name.t * Dns.Rr_map.Ipv6_set.t
  | Connection_failed of [`host] Domain_name.t * int * Ipaddr.t * int
  | Connected of [`host] Domain_name.t * int * Ipaddr.t * int

val pp_event : event Fmt.t

val create : int64 -> t

val timer : t -> int64 -> t * action list

val connect : t -> int64 -> id:int -> [`host] Domain_name.t -> int -> t * action list

val event : t -> int64 -> event -> t * action list
