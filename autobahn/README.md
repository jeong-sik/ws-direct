# Autobahn conformance

The Autobahn `fuzzingclient` drives ~500 protocol cases against a server. It is
the authoritative external check that ws-direct frames, fragments, validates
UTF-8, and closes per RFC 6455 — and that it never stalls or disconnects on
malformed or large input.

## Run

The `echo_server` executable answers every data message with the same opcode;
ping/pong and close are handled by the core endpoint.

```sh
# 1. start the echo server (binds 0.0.0.0:9001)
dune exec bin/echo_server.exe --root .

# 2. run the suite (Docker reaches the host via host.docker.internal)
mkdir -p reports
docker run --rm -v "$PWD/autobahn":/mnt -v "$PWD/autobahn/reports":/mnt/reports \
  -w /mnt crossbario/autobahn-testsuite \
  wstest -m fuzzingclient -s /mnt/fuzzingclient.json

# 3. open reports/index.html, or check reports/index.json for "behavior"
```

Sections 12 and 13 (permessage-deflate) are excluded: ws-direct does not
negotiate the compression extension (RFC 6455 core only), so those cases are out
of scope rather than failing.

## Result (2026-06-23, ws-direct @ b55d4b5 core + eio server driver)

301 cases run (sections 1-7, 9, 10), **0 FAILED**.

| section | cases | result |
|---|---|---|
| 1-7, 10 (framing, ping/pong, reserved, opcodes, fragmentation, UTF-8, close) | 247 | 241 OK, 3 NON-STRICT, 3 INFORMATIONAL |
| 9 (limits / large & fragmented messages) | 54 | 54 OK |

- The 3 NON-STRICT cases are 6.4.2-6.4.4 (UTF-8 fail-fast timing): the
  connection is closed with the correct 1007, just not at the exact octet that
  first makes the stream invalid. `behaviorClose` is OK for all three.
- The 3 INFORMATIONAL cases (7.1.6, 7.13.1, 7.13.2) carry no pass/fail verdict.
- Section 9 confirms large and fragmented messages echo without stalling — the
  failure mode that affected the previous one-frame-per-read server.
