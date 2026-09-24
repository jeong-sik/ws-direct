# Changelog

## 0.2.0 (2026-09-24)

### Added

- `Endpoint.Wsd.send_text_bigstring` sends a text frame from a bigstring
  without copying it into a string first (#4).

### Fixed

- The Eio writer loop stops on a dead transport instead of spinning (#3).
- The Eio opening handshake is bounded by a wall-clock deadline (#2).
- Inbound parsing stops at the first terminal event, Fail or Close (#1).

## 0.1.0

- RFC 6455 frame codec and connection state machine (`ws-direct-core`), an
  Eio client/server driver (`ws-direct-eio`) and a gluten server adapter
  (`ws-direct-gluten`).
