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
  | Resolved_a of [`host] Domain_name.t * Ipaddr.V4.Set.t
  | Resolved_aaaa of [`host] Domain_name.t * Ipaddr.V6.Set.t
  | Resolved_a_failed of [`host] Domain_name.t
  | Resolved_aaaa_failed of [`host] Domain_name.t
  | Connection_failed of [`host] Domain_name.t * int * (Ipaddr.t * int)
  | Connected of [`host] Domain_name.t * int * (Ipaddr.t * int)

val pp_event : event Fmt.t
(** [pp_event ppf e] pretty-prints event [e] on [ppf]. *)

val create : ?aaaa_timeout:int64 -> ?v6_connect_timeout:int64 ->
  ?connect_timeout:int64 -> ?resolve_timeout:int64 -> ?resolve_retries:int ->
  int64 -> t
(** [create ~aaaa_timeout ~v6_connect_timeout ~connect_timeout ~resolve_timeout ~resolve_retries ts]
    creates the internal state, initialized with the timestamp [ts] (an
    arbitrary number that must be monotonically increasing). The timeouts are
    specified in nanoseconds: the default of [aaaa_timeout] is
    [Duration.of_ms 50], [v6_connect_timeout] is [Duration.of_ms 200],
    [connect_timeout] is [Duration.of_sec 10], and [resolve_timeout] is
    [Duration.of_sec 1]. The [resolve_retries] defaults to 3. *)

val timer : t -> int64 -> t * [ `Suspend | `Act ] * action list
(** [timer t ts] is a timer function that results in an updated [t] and either
    [`Suspend] signalling the timer thread can sleep or [`Act] that the timer
    should be called again. In addition, a list of actions that need to be
    performed (connection to be retried, connection failures to be reported,
    ...) is provided.
    The timer thread should be signalled to resume after calling [connect] or
    [connect_ip]. *)

val connect : t -> int64 -> id:int -> [`host] Domain_name.t -> int list ->
  t * action list
(** [connect t ts ~id host ports] attempts a connection to [host], where the
    [ports] are attempted in sequence. It results in an updated [t] and a list
    of actions to be performed.

    @raise Failure if [ports] is the empty list. *)

val connect_ip : t -> int64 -> id:int -> (Ipaddr.t * int) list ->
  t * action list
(** [connect_ip t ts ~id addresses] attempts a connection to [addresses]. By
    default, the list will be tried in sequence. The ports will be tried in
    sequence.  The result is an updated [t] and a list of actions to be
    performed.

    @raise Failure if [addresses] is the empty list. *)

val event : t -> int64 -> event -> t * action list
(** [event t ts ev] results in an updated [t] and a list of actions to be
    performed.

    @raise Failure if [ev] contains an empty set of IP addresses. *)

(** A map for waiters and internal id. *)
module Waiter_map : sig
  include Map.S with type key = Int.t

  val register : 'a -> 'a t -> 'a t * int
  (** [register v t] registers [v] in [t], and returns the updated map and
      the key that was used. *)

  val find_and_remove : int -> 'a t -> 'a t * 'a option
  (** [find_and_remove id t] looks up [id] in [t], and removes [id] from [t]. *)
end
