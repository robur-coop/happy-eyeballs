
(** The internal state of happy eyeballs. *)
type t

(** The variant of actions to be performed by the effectful layer. *)
type action =
  | Resolve_a of [`host] Domain_name.t
  | Resolve_aaaa of [`host] Domain_name.t
  | Connect of [`host] Domain_name.t * int * (Ipaddr.t * int)
  | Connect_failed of [`host] Domain_name.t * int

val pp_action : action Fmt.t
(** [pp_action ppf a] pretty-prints the action [a] on [ppf]. *)

(** The variant of events. *)
type event =
  | Resolved_a of [`host] Domain_name.t * Dns.Rr_map.Ipv4_set.t
  | Resolved_aaaa of [`host] Domain_name.t * Dns.Rr_map.Ipv6_set.t
  | Resolved_a_failed of [`host] Domain_name.t
  | Resolved_aaaa_failed of [`host] Domain_name.t
  | Connection_failed of [`host] Domain_name.t * int * (Ipaddr.t * int)
  | Connected of [`host] Domain_name.t * int * (Ipaddr.t * int)

val pp_event : event Fmt.t
(** [pp_event ppf e] pretty-prints event [e] on [ppf]. *)

val create : int64 -> t
(** [create ts] creates the internal state, initialized with the timestamp
    [ts] (an arbitrary number that must be monotonically increasing). *)

val timer : t -> int64 -> t * action list
(** [timer t ts] is a timer function that results in an updated [t] and a list
    of actions that need to be performed (connection to be retried, connection
    failures to be repored, ...) *)

val connect : t -> int64 -> id:int -> [`host] Domain_name.t -> int list ->
  t * action list
(** [connect t ts ~id host ports] attempts a connection to [host], where the
    [ports] are attempted in sequence. It results in an updated [t] and a list
    of actions to be performed. *)

module Ip_set : Set.S with type elt = Ipaddr.t

val connect_ip : t -> int64 -> id:int -> Ip_set.t -> int list ->
  t * action list
(** [connect_ip t ts ~id ips ports] attempts a connection to [ips, ports]. The
    list of ips will be sorted (mixing IPv6 and IPv4 addresses). The ports will
    be tried in sequence. The result is an updated [t] and a list of actions to
    be performed. *)

val event : t -> int64 -> event -> t * action list
(** [event t ts ev] results in an updated [t] and a list of actions to be
    performed. *)
