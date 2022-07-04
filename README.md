## happy eyeballs -- connecting to a remote host

[RFC 8305](https://datatracker.ietf.org/doc/html/rfc8305) describes how to
connect to a remote host, given that the host may have multiple addresses (IPv4
and IPv6, via DNS A and AAAA resource records), and the local host may have
connectivity via IPv4 only, IPv6 only, or both. The preference is to use IPv6.

This package implements the given RFC in its core (Happy_eyeballs module).
In addition, the Happy_eyeballs_lwt is a Lwt implementation using Unix.

The implemented [state machine](https://data.robur.coop/happy_eyeballs.pdf) is
kept in-sync with the code (source: happy_eyeballs.dot at the root of this
repository).

It is licensed under the ISC license.

### Installation

`opam install happy-eyeballs happy-eyeballs-lwt happy-eyeballs-mirage`

### Development

Since dns-client depends on happy-eyeballs and happy-eyeballs-lwt,
happy-eyeballs-mirage depend on dns-client, the dune build system is easily
confused. A workaround is after a `git clone` of happy-eyeballs, do an `opam
source dns-client`. Then dune will use the local dns-client package, and there
will be no circularity issue.
