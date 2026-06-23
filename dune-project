(lang dune 3.13)

(name ws-direct)
(version 0.1.0)

(generate_opam_files true)

(license MIT)
(authors "Jeong-Sik Yun")
(maintainers "Jeong-Sik Yun")
(source
 (github jeong-sik/ws-direct))

(package
 (name ws-direct-core)
 (synopsis
  "RFC 6455 WebSocket frame codec and connection state machine (runtime-agnostic)")
 (description
  "Pure OCaml WebSocket framing: frame parse/serialize over bigstrings, masking, fragmentation reassembly, and a connection state machine exposing gluten-RUNTIME-shaped read/write operations. No Eio/Lwt/Async dependency.")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.13))
  bigstringaf
  faraday
  (alcotest
   (and
    :with-test
    (>= 1.7)))
  (qcheck-core
   (and
    :with-test
    (>= 0.21)))
  (qcheck-alcotest
   (and
    :with-test
    (>= 0.21)))))

(package
 (name ws-direct-gluten)
 (allow_empty)
 (synopsis "Gluten server adapter for ws-direct")
 (description
  "Plugs ws-direct's connection state machine into gluten as a server-side protocol, for HTTP/1.1 WebSocket upgrades on the httpun stack.")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.13))
  (ws-direct-core
   (= :version))
  gluten))

(package
 (name ws-direct-eio)
 (synopsis "Eio client/server driver for ws-direct")
 (description
  "Drives ws-direct's connection over an Eio.Flow.two_way, including the client-side HTTP upgrade handshake. Works over any flow, including TLS flows (Tls_eio.t).")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.13))
  (ws-direct-core
   (= :version))
  eio
  digestif
  base64
  mirage-crypto-rng
  cstruct
  (eio_main
   (and
    :with-test
    (>= 1.0)))))
