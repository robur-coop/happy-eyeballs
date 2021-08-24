## happy eyeballs -- connecting to a remote host

[RFC 8305](https://datatracker.ietf.org/doc/html/rfc8305) describes how to
connect to a remote host, given that the host may have multiple addresses (IPv4
and IPv6, via DNS A and AAAA resource records), and the local host may have
connectivity via IPv4 only, IPv6 only, or both. The preference is to use IPv6.

This package implements the given RFC in its core (Happy_eyeballs module).
In addition, the Happy_eyeballs_lwt is a Lwt implementation using Unix.

It is licensed under the ISC license.

Installation:

`opam install happy-eyeballs happy-eyeballs-lwt happy-eyeballs-mirage`
