module F = Ws_direct_core.Frame
module C = Ws_direct_core.Connection
module E = Ws_direct_core.Endpoint

let bs_of_string s = Bigstringaf.of_string s ~off:0 ~len:(String.length s)
let mask = "\x01\x02\x03\x04"

(* client -> server wire frame: masked *)
let cf ?(fin = true) opcode s = F.to_string ~mask (F.of_string ~fin opcode s)

(* Pump all currently-buffered output out of an endpoint as a byte string. *)
let drain_output t =
  let buf = Buffer.create 256 in
  let rec loop () =
    match E.next_write_operation t with
    | `Write iovecs ->
      let n =
        List.fold_left
          (fun acc (iov : Bigstringaf.t Faraday.iovec) ->
            Buffer.add_string buf
              (Bigstringaf.substring iov.Faraday.buffer ~off:iov.Faraday.off
                 ~len:iov.Faraday.len);
            acc + iov.Faraday.len)
          0 iovecs
      in
      E.report_write_result t (`Ok n);
      loop ()
    | `Yield | `Close _ -> ()
  in
  loop ();
  Buffer.contents buf

let parse1 s =
  match F.parse (bs_of_string s) ~off:0 ~len:(String.length s) with
  | F.Frame (p, _) -> p
  | _ -> Alcotest.failf "expected a parseable frame, got %d bytes" (String.length s)

let test_auto_pong () =
  let t = E.create E.Server (fun _ -> E.handlers ()) in
  let ping = cf F.Opcode.Ping "hi" in
  ignore (E.read t (bs_of_string ping) ~off:0 ~len:(String.length ping));
  let { F.frame; masked } = parse1 (drain_output t) in
  Alcotest.(check bool) "pong opcode" true (frame.F.opcode = F.Opcode.Pong);
  Alcotest.(check bool) "server pong unmasked" false masked;
  Alcotest.(check string) "pong echoes payload" "hi" (F.payload_string frame)

let test_server_send_unmasked () =
  let t = E.create E.Server (fun _ -> E.handlers ()) in
  E.Wsd.send_text (E.wsd t) "hello";
  let { F.frame; masked } = parse1 (drain_output t) in
  Alcotest.(check bool) "unmasked" false masked;
  Alcotest.(check bool) "text" true (frame.F.opcode = F.Opcode.Text);
  Alcotest.(check string) "payload" "hello" (F.payload_string frame)

let test_client_send_masked () =
  let t =
    E.create E.Client ~random:(fun () -> "\x09\x0a\x0b\x0c") (fun _ ->
        E.handlers ())
  in
  E.Wsd.send_text (E.wsd t) "hi";
  let { F.frame; masked } = parse1 (drain_output t) in
  Alcotest.(check bool) "client masks" true masked;
  Alcotest.(check string) "payload" "hi" (F.payload_string frame)

(* Two endpoints exchanging through their output buffers: a client message must
   round-trip into the server's [on_message]. *)
let test_client_to_server () =
  let received = ref None in
  let server =
    E.create E.Server (fun _ ->
        E.handlers
          ~on_message:(fun m ->
            received := Some (Bigstringaf.to_string m.C.Message.payload))
          ())
  in
  let client =
    E.create E.Client ~random:(fun () -> "\x09\x0a\x0b\x0c") (fun _ ->
        E.handlers ())
  in
  E.Wsd.send_text (E.wsd client) "ping!";
  let wire = drain_output client in
  ignore (E.read server (bs_of_string wire) ~off:0 ~len:(String.length wire));
  Alcotest.(check (option string)) "server received" (Some "ping!") !received

(* A fragmented client message coalesced into one buffer round-trips whole. *)
let test_fragmented_round_trip () =
  let received = ref None in
  let server =
    E.create E.Server (fun _ ->
        E.handlers
          ~on_message:(fun m ->
            received := Some (Bigstringaf.to_string m.C.Message.payload))
          ())
  in
  let wire =
    cf ~fin:false F.Opcode.Text "AA"
    ^ cf ~fin:false F.Opcode.Continuation "BB"
    ^ cf ~fin:true F.Opcode.Continuation "CC"
  in
  ignore (E.read server (bs_of_string wire) ~off:0 ~len:(String.length wire));
  Alcotest.(check (option string)) "reassembled" (Some "AABBCC") !received

let test_close_echo_and_closed () =
  let closed_with = ref None in
  let t =
    E.create E.Server (fun _ ->
        E.handlers
          ~on_close:(fun ~code ~reason -> closed_with := Some (code, reason))
          ())
  in
  let close = cf F.Opcode.Close "\x03\xe8" in
  ignore (E.read t (bs_of_string close) ~off:0 ~len:(String.length close));
  Alcotest.(check bool) "endpoint closed" true (E.is_closed t);
  Alcotest.(check (option (pair (option int) string)))
    "on_close fired" (Some (Some 1000, "")) !closed_with;
  let { F.frame; _ } = parse1 (drain_output t) in
  Alcotest.(check bool) "echoed a Close" true (frame.F.opcode = F.Opcode.Close)

let test_protocol_error_closes () =
  let errored = ref false in
  let t =
    E.create E.Server (fun _ ->
        E.handlers ~on_error:(fun _ -> errored := true) ())
  in
  (* unmasked client frame -> protocol error *)
  let bad = F.to_string (F.of_string F.Opcode.Text "x") in
  ignore (E.read t (bs_of_string bad) ~off:0 ~len:(String.length bad));
  Alcotest.(check bool) "on_error fired" true !errored;
  Alcotest.(check bool) "closed after error" true (E.is_closed t);
  let { F.frame; _ } = parse1 (drain_output t) in
  Alcotest.(check bool) "sent Close 1002" true (frame.F.opcode = F.Opcode.Close)

let () =
  Alcotest.run "ws-direct-core endpoint"
    [ ( "control automation"
      , [ Alcotest.test_case "auto-pong on ping" `Quick test_auto_pong
        ; Alcotest.test_case "close echo + is_closed" `Quick
            test_close_echo_and_closed
        ; Alcotest.test_case "protocol error sends Close 1002" `Quick
            test_protocol_error_closes
        ] )
    ; ( "send path"
      , [ Alcotest.test_case "server sends unmasked" `Quick
            test_server_send_unmasked
        ; Alcotest.test_case "client sends masked" `Quick test_client_send_masked
        ] )
    ; ( "round trip"
      , [ Alcotest.test_case "client -> server message" `Quick
            test_client_to_server
        ; Alcotest.test_case "fragmented message reassembled" `Quick
            test_fragmented_round_trip
        ] )
    ]
