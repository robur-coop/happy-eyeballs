## v2.0.1 (2025-05-14)

* mirage & lwt: provide the appropriate event, Resolved_aaaa_failed, when IPv6
  resolution failed. This fixes happy-eyeballs being stuck trying a connection
  (#48 @hannesm)

## v2.0.0 (2025-02-05)

* Use mirage-sleep and mirage-mtime (dune variants) instead of functorising
  over TIME and MCLOCK

## v1.2.2 (2024-10-10)

* Demote log message for Waiting_for_AAAA and Resolve_a (@reynir, #46)

## v1.2.1 (2024-09-04)

* happy-eyeballs-miou-unix: fix AAAA record (@dinosaure, #45)
* happy-eyeballs-miou-unix: demote log levels (@dinosaure, #45)

## v1.2.0 (2024-08-23)

* Add the miou implementation of happy-eyeballs (@dinosaure, @hannesm, #41)
* Fix the CirrusCI (@hannesm, #43)

## v1.1.0 (2024-05-27)

* Allow timeouts and delays in `connect`, `connect_ip`, `connect_host` to
  overwrite the default from `create` (#42 @hannesm)

## v1.0.0 (2024-05-19)

* Reverse dependency between dns-client-lwt and happy-eyeballs-lwt,
  dns-client-mirage and happy-eyeballs-mirage (#38 @dinosaure)
  This now has a new function `inject` to put a name resolver `getaddrinfo`
  into action. The default for happy-eyeballs-lwt is Lwt_unix.getaddrinfo.
  For happy-eyeballs-mirage, there is no default.
* Update timestamp when a fresh connection attempt is done (#37 @hannesm)
* Log message: prepend with counter to distinguish multiple happy-eyeballs
  instaces (#36 @hannesm)

## v0.6.0 (2023-06-15)

* Fix connection establishment: parallelize connection attempts, deal with
  servers dropping packets (such as the Azure DNS resolver)
* Introduce connect_delay (when to start the next connection attempt)
* Deal with connect_timeout properly
* Revise log output (IP vs domain name, fewer logs on debug level)

All in #34, @hannesm, reviewed by @reynir -- sponsored by Semgrep Inc

## v0.5.0 (2023-02-16)

* Adapt to dns-client 7.0.0 packaging changes (#31 @dinosaure)

## v0.4.0 (2022-12-02)

* Cancellation of connection attempts (#30 @reynir @hannesm, fixes #27)
* Make the type id abstract (suggested by @reynir in #30)
* Add reason for failures to the variants, improves log output (#30, fixes #7, @hannesm @reynir)
* Remove log messages about DNS resolution success/failure when there is no
  awaiting connection (#30, @hannesm)
* Use the domain name of the fist IP address in connect_ip as identifier (this
  provides more useful information than the hardcoded "host.invalid") (#29, @hannesm)

## v0.3.1 (2022-11-21)

* Improve documentation for `Happy_eyeballs.timer` (#24, #25 @reynir, review by @hannesm and @bikallem)
* Demote log levels that are (likely) caused by missing cancellation (#26 @reynir)

## v0.3.0 (2022-03-21)

* Happy_eyeballs_mirage.connect_device: remove int64 argument (timestamp), use
  monotonic clocks `C.elapsed_ns ()` instead (noticed by @dinosaure)

## v0.2.0 (2022-03-16)

* Happy_eyeballs_mirage: add a module type signature to allow creation of
  a MirageOS device (#22 @dinosaure)
* happy-eyeballs-lwt: update to cmdliner 1.1.0 (@hannesm)

## v0.1.3 (2022-01-12)

* Happy_eyeballs.create: add v6_connect_timeout parameter - the amount of
  nanoseconds (default: 200ms) after which to attempt IPv4 connection
  establishment. (#21 @hannesm, review by @reynir, issue reported at
  robur-coop/http-lwt-client#8 by @kit-ty-kate)
* Happy_eyeballs.create: add resolve_retries - the amount of resolve attempts
  when a (resolve) timeout occurs (default: 3). (#21 @hannesm, review by
  @reynir, issue reported at robur-coop/http-lwt-client#8 by @kit-ty-kate)

## v0.1.2 (2021-12-17)

* Happy_eyeballs_{lwt,mirage}: update to tcpip 7.0.0, remove mirage-stack
  dependency (#20 @dinosaure)

## v0.1.1 (2021-11-18)

* Happy_eyeballs_lwt.create and Happy_eyeballs_mirage.create now take an
  optional ?happy_eyeballs:Happy_eyeballs.t argument, and also an optional
  ?dns:Dns_client_lwt.t/DNS.t argument. This avoids the need to forward all
  potential creation arguments of Happy_eyeballs and DNS. (#19 @hannesm)
* Fix state machine: if Connecting fails, and resolved is not yet both, return
  to Resolving (instead of Error) (#13 @hannesm,
  similar to d0d4ef5ea2aaf2de407ba84742c5648489c47e1f #9)
* Add a state machine diagram (happy_eyeballs.dot) (#13 @hannesm)

## v0.1.0 (2021-10-27)

* Remove assertion in timer, and fix the code (reported by @dinosaure in #17,
  fix in #18 by @hannesm, reviewed by @reynir)
* Support OCaml 4.08 (#18 hannesm, requested in #16 by @smorimoto)

## v0.0.8 (2021-10-20)

* Adapt to dns 6.0.0 API (#15 @hannesm)
* Drop rresult dependency
* Avoid deprecated fmt functions, require fmt 0.8.7

## v0.0.7 (2021-09-28)

* Document changes of the return value of timer (#11 @reynir)
* Pass timeouts as duration into the create functions (#12 @hannesm)

## v0.0.6 (2021-09-21)

* return a variant from timer to indicate whether there are connections pending
  or the timer can be suspended -- this avoids unnecessary busy work (@reynir)
* connect_ip: take list of Ipaddr.t and int pairs instead of separate lists.
  The reason for this change is the dns-client. (@hannesm @reynir)

## v0.0.5 (2021-09-13)

* connect_ip: take an optional shuffle argument and an ordered list of ips to
  attempt connections to. The reason for this change is that /etc/resolv.conf
  specifies an ordering. (@hannesm)

## v0.0.4 (2021-09-11)

* Use set from ipaddr (>= 5.2.0) instead providing these (@hannesm)

## v0.0.3 (2021-09-07)

* BUGFIX: Avoid exception if expand_list is called with an empty list (@hannesm)

## v0.0.2 (2021-09-06)

* BUGFIX: Delay connect failure if v6 resolution and connection attempt fails
  before v4 resolution had a chance to succeed or fail (issue #9). (@hannesm)

## v0.0.1 (2021-08-24)

* Initial release (@hannesm)
