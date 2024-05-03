include
  Dns_client.S
    with type io_addr =
      [ `Plaintext of Ipaddr.t * int
      | `Tls of Tls.Config.client * Ipaddr.t * int ]
     and type +'a io = 'a

type daemon
type happy = stack

type getaddrinfo = {
    getaddrinfo:
      'response 'a.
         'response Dns.Rr_map.key
      -> 'a Domain_name.t
      -> ('response, [ `Msg of string ]) result
}
[@@unboxed]

val make :
     ?aaaa_timeout:int64
  -> ?connect_delay:int64
  -> ?connect_timeout:int64
  -> ?resolve_timeout:int64
  -> ?resolve_retries:int
  -> unit
  -> daemon * happy
(** [make ()] allocates a new happy-eyeballs {i daemon} in parallel which must
    be de-allocated with {!val:kill} at the end of your whole process.
    Otherwise, Miou will complain that some tasks still exist. You can check
    {!val:Happy_eyeballs.create} for more informations about optional arguments.

    The happy-eyeball stack is able to give a connected socket only from IP
    addresses. It does not (yet) resolve domain-names. Only {!val:connect_ip} is
    usable. If you have a [getaddrinfo] function, you can then inject it into
    the happy-eyeball daemon to enable it to resolve domain names. *)

val inject_resolver : getaddrinfo:getaddrinfo -> happy -> unit
(** [inject_resolver ~getaddrinfo happy] injects a DNS resolver into the given
    {i happy-eyeballs} [happy] instance. Initially, the {i happy-eyeballs}
    instance (created by {!val:create}) can not resolve domain-name. When the
    user is able to resolve a domain-name (via the DNS protocol for example),
    he/she can {i inject} its resolver into the {i happy-eyeballs} instance.

    Only after injection the user can use {!val:connect_host} and
    {!val:connect_endpoint}.

    @raise Invalid_argument if you try to use more than once this function. *)

val kill : daemon -> unit
(** [kill daemon] kills properly the happy-eyeball daemon. *)

val connect_ip :
     happy
  -> (Ipaddr.t * int) list
  -> ((Ipaddr.t * int) * Miou_unix.file_descr, [> `Msg of string ]) result
(** [connect_ip t addresses] establishes a connection to [addresses]. *)

val connect_host :
     happy
  -> [ `host ] Domain_name.t
  -> int list
  -> ((Ipaddr.t * int) * Miou_unix.file_descr, [> `Msg of string ]) result
(** [connect_host t host ports] establishes a connection to [host] on [ports]
    (tried in sequence).

    @raise Failure if [ports] is an empty list. *)

val connect_endpoint :
     happy
  -> string
  -> int list
  -> ((Ipaddr.t * int) * Miou_unix.file_descr, [> `Msg of string ]) result
(** [connect_endpoint t host ports] establishes a connection to [host] on
    [ports], which may be a host name or an IP address.

    @raise Failure if [ports] is an empty list. *)
