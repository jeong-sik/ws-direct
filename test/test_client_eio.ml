module E = Ws_direct_core.Endpoint
module F = Ws_direct_core.Frame
module C = Ws_direct_core.Connection
module H = Ws_direct_eio.Handshake

(* deterministic "randomness" so the test is reproducible; the real driver
   injects a CSPRNG *)
let test_random n = String.make n '\x2a'

let read_head flow =
  let b = Buffer.create 256 in
  let one = Cstruct.create 1 in
  let rec loop () =
    let _ = Eio.Flow.single_read flow one in
    Buffer.add_char b (Cstruct.get_char one 0);
    let s = Buffer.contents b in
    let l = String.length s in
    if l >= 4 && String.sub s (l - 4) 4 = "\r\n\r\n" then s else loop ()
  in
  loop ()

let header_value head name =
  String.split_on_char '\n' head
  |> List.find_map (fun line ->
         match String.index_opt line ':' with
         | None -> None
         | Some i ->
           let n =
             String.sub line 0 i |> String.trim |> String.lowercase_ascii
           in
           if String.equal n name then
             Some
               (String.trim (String.sub line (i + 1) (String.length line - i - 1)))
           else None)

(* read bytes until a single frame parses *)
let read_frame flow =
  let buf = Buffer.create 64 in
  let chunk = Cstruct.create 256 in
  let rec loop () =
    let n = Eio.Flow.single_read flow chunk in
    Buffer.add_string buf (Cstruct.to_string ~off:0 ~len:n chunk);
    let s = Buffer.contents buf in
    let bs = Bigstringaf.of_string s ~off:0 ~len:(String.length s) in
    match F.parse bs ~off:0 ~len:(String.length s) with
    | F.Frame (p, _) -> p
    | F.Incomplete -> loop ()
    | F.Protocol_error m -> Alcotest.failf "server frame parse: %s" m
  in
  loop ()

(* A minimal hand-written server: validates the handshake against the real key,
   checks the client frame is masked and intact, then replies unmasked. *)
let server flow =
  let head = read_head flow in
  let key =
    match header_value head "sec-websocket-key" with
    | Some k -> k
    | None -> Alcotest.fail "server: no Sec-WebSocket-Key in request"
  in
  let resp =
    String.concat "\r\n"
      [ "HTTP/1.1 101 Switching Protocols"
      ; "Upgrade: websocket"
      ; "Connection: Upgrade"
      ; "Sec-WebSocket-Accept: " ^ H.accept_token key
      ; ""
      ; ""
      ]
  in
  Eio.Flow.copy_string resp flow;
  let p = read_frame flow in
  Alcotest.(check bool) "client frame is masked" true p.F.masked;
  Alcotest.(check bool) "client opcode is Text" true
    (p.F.frame.F.opcode = F.Opcode.Text);
  Alcotest.(check string) "client payload" "hello" (F.payload_string p.F.frame);
  Eio.Flow.copy_string (F.to_string (F.of_string F.Opcode.Text "pong")) flow

let test_roundtrip () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let reply, set_reply = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () -> server b)
    (fun () ->
      let wsd =
        Ws_direct_eio.Client.connect ~sw ~random:test_random ~clock
          ~host:"localhost" ~resource:"/" a (fun _ ->
            E.handlers
              ~on_message:(fun m ->
                Eio.Promise.resolve set_reply
                  (Bigstringaf.to_string m.C.Message.payload))
              ())
      in
      E.Wsd.send_text wsd "hello";
      Alcotest.(check string) "server reply round-trips" "pong"
        (Eio.Promise.await reply);
      Eio.Flow.shutdown a `All)

(* Server that completes the handshake then sends one unmasked Text frame, used
   to drive the client's reader into a handler that raises. *)
let server_send_trigger flow =
  let head = read_head flow in
  let key =
    match header_value head "sec-websocket-key" with
    | Some k -> k
    | None -> Alcotest.fail "server: no Sec-WebSocket-Key in request"
  in
  let resp =
    String.concat "\r\n"
      [ "HTTP/1.1 101 Switching Protocols"
      ; "Upgrade: websocket"
      ; "Connection: Upgrade"
      ; "Sec-WebSocket-Accept: " ^ H.accept_token key
      ; ""
      ; ""
      ]
  in
  Eio.Flow.copy_string resp flow;
  Eio.Flow.copy_string (F.to_string (F.of_string F.Opcode.Text "trigger")) flow

(* P0 (RFC-0287 review): an exception raised inside the reader path — here an
   [on_message] handler that throws, the same propagation route as a mid-session
   [Tls_failure] — must NOT escape the driver to fail the parent switch (that
   would take down every sibling fiber). The driver must contain it, deliver it
   as [on_error] (so a callback->stream bridge unblocks), and exit cleanly. *)
let test_reader_exception_isolated () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let errored, set_errored = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () -> server_send_trigger b)
    (fun () ->
      let _wsd =
        Ws_direct_eio.Client.connect ~sw ~random:test_random ~clock
          ~host:"localhost" ~resource:"/" a (fun _ ->
            E.handlers
              ~on_message:(fun _ -> failwith "handler boom")
              ~on_error:(fun msg -> Eio.Promise.resolve set_errored msg)
              ())
      in
      (* If the driver re-raised instead of containing, this await would never
         resolve — the fork would have failed the switch first. *)
      let msg = Eio.Promise.await errored in
      Alcotest.(check bool)
        "on_error carries the cause" true
        (let n = String.length "boom" in
         let rec has i =
           i + n <= String.length msg
           && (String.sub msg i n = "boom" || has (i + 1))
         in
         has 0);
      Eio.Flow.shutdown a `All)

(* RFC-0287 review #2: a peer that never sends CRLFCRLF must not grow the
   handshake buffer without limit. Feed >16 KiB with no terminator and assert
   read_head fails instead of looping forever. *)
let test_read_head_caps_oversized () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  (* A concurrent writer floods non-terminator bytes; read_head drains until it
     hits the cap and raises, at which point Fiber.both cancels the writer. *)
  let raised =
    try
      Eio.Fiber.both
        (fun () -> Eio.Flow.copy_string (String.make (32 * 1024) 'x') b)
        (fun () -> ignore (Ws_direct_eio.Driver.read_head ~clock a));
      false
    with
    | Failure _ -> true
  in
  Alcotest.(check bool) "read_head caps an oversized handshake head" true raised

(* P1-2 (review): the byte cap bounds memory, not lifetime. A peer that sends
   bytes below the CRLFCRLF terminator — here, nothing at all — must be cut off
   by a wall-clock deadline rather than holding the fiber+fd forever. With no
   deadline this call would block indefinitely; the 50ms timeout must raise. *)
let test_read_head_times_out () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  (* the peer (b) never sends a terminator *)
  ignore b;
  let raised =
    try
      ignore (Ws_direct_eio.Driver.read_head ~clock ~timeout:0.05 a);
      false
    with
    | Failure _ -> true
  in
  Alcotest.(check bool) "read_head times out on a silent peer" true raised

let () =
  Alcotest.run "ws-direct-eio client"
    [ ( "loopback"
      , [ Alcotest.test_case "handshake + masked round-trip over socketpair"
            `Quick test_roundtrip
        ; Alcotest.test_case "reader exception is isolated and surfaced as on_error"
            `Quick test_reader_exception_isolated
        ; Alcotest.test_case "read_head caps oversized handshake head" `Quick
            test_read_head_caps_oversized
        ; Alcotest.test_case "read_head times out on a silent peer (slowloris)"
            `Quick test_read_head_times_out
        ] )
    ]
