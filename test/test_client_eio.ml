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
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let a, b = Eio_unix.Net.socketpair_stream ~sw () in
  let reply, set_reply = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () -> server b)
    (fun () ->
      let wsd =
        Ws_direct_eio.Client.connect ~sw ~random:test_random ~host:"localhost"
          ~resource:"/" a (fun _ ->
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

let () =
  Alcotest.run "ws-direct-eio client"
    [ ( "loopback"
      , [ Alcotest.test_case "handshake + masked round-trip over socketpair"
            `Quick test_roundtrip
        ] )
    ]
