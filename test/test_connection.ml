module F = Ws_direct_core.Frame
module C = Ws_direct_core.Connection

let bs_of_string s = Bigstringaf.of_string s ~off:0 ~len:(String.length s)
let mask = "\x01\x02\x03\x04"

(* client -> server wire frame: masked *)
let cf ?(fin = true) opcode s = F.to_string ~mask (F.of_string ~fin opcode s)

(* server -> client wire frame: unmasked *)
let sf ?(fin = true) opcode s = F.to_string (F.of_string ~fin opcode s)

let drain role wire =
  C.read_bytes (C.create role) (bs_of_string wire) ~off:0
    ~len:(String.length wire)

(* HEADLINE: three coalesced fragments in ONE read must drain fully into one
   reassembled message. A one-frame-per-read drainer (the httpun-ws 0.2.0 bug)
   stalls here after the first frame. *)
let test_coalesced_fragments () =
  let wire =
    cf ~fin:false F.Opcode.Text "AA"
    ^ cf ~fin:false F.Opcode.Continuation "BB"
    ^ cf ~fin:true F.Opcode.Continuation "CC"
  in
  let events, consumed = drain C.Server wire in
  Alcotest.(check int) "consumed all" (String.length wire) consumed;
  match events with
  | [ C.Message { C.Message.kind = C.Message.Text; payload } ] ->
    Alcotest.(check string) "reassembled" "AABBCC"
      (Bigstringaf.to_string payload)
  | _ ->
    Alcotest.failf "expected 1 reassembled Text, got %d events"
      (List.length events)

let test_interleaved_control () =
  let wire =
    cf ~fin:false F.Opcode.Text "AA"
    ^ cf ~fin:true F.Opcode.Ping "hi"
    ^ cf ~fin:true F.Opcode.Continuation "BB"
  in
  let events, _ = drain C.Server wire in
  match events with
  | [ C.Ping p; C.Message { payload; _ } ] ->
    Alcotest.(check string) "ping" "hi" (Bigstringaf.to_string p);
    Alcotest.(check string) "msg" "AABB" (Bigstringaf.to_string payload)
  | _ -> Alcotest.failf "expected [Ping; Message], got %d" (List.length events)

let test_single_message () =
  let events, consumed = drain C.Server (cf F.Opcode.Text "hello") in
  Alcotest.(check bool) "consumed > 0" true (consumed > 0);
  match events with
  | [ C.Message { payload; kind = C.Message.Text } ] ->
    Alcotest.(check string) "payload" "hello" (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected single Text message"

let test_partial_tail () =
  let full = cf F.Opcode.Text "first" in
  let second = cf F.Opcode.Text "second" in
  let half = String.sub second 0 (String.length second / 2) in
  let wire = full ^ half in
  let events, consumed = drain C.Server wire in
  Alcotest.(check int) "consumed only first frame" (String.length full) consumed;
  match events with
  | [ C.Message { payload; _ } ] ->
    Alcotest.(check string) "first" "first" (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected only the first message"

let test_reassembly_across_reads () =
  let c = C.create C.Server in
  let w1 = cf ~fin:false F.Opcode.Text "AA" in
  let w2 = cf ~fin:true F.Opcode.Continuation "BB" in
  let e1, _ = C.read_bytes c (bs_of_string w1) ~off:0 ~len:(String.length w1) in
  Alcotest.(check int) "no event mid-fragment" 0 (List.length e1);
  let e2, _ = C.read_bytes c (bs_of_string w2) ~off:0 ~len:(String.length w2) in
  match e2 with
  | [ C.Message { payload; _ } ] ->
    Alcotest.(check string) "joined across reads" "AABB"
      (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected completed message on second read"

let test_server_requires_mask () =
  let events, _ = drain C.Server (sf F.Opcode.Text "x") in
  match events with
  | [ C.Protocol_error _ ] -> ()
  | _ -> Alcotest.fail "expected Protocol_error for unmasked client frame"

let test_client_accepts_unmasked () =
  let events, _ = drain C.Client (sf F.Opcode.Binary "data") in
  match events with
  | [ C.Message { kind = C.Message.Binary; payload } ] ->
    Alcotest.(check string) "payload" "data" (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected Binary message"

let test_client_rejects_masked () =
  let events, _ = drain C.Client (cf F.Opcode.Text "x") in
  match events with
  | [ C.Protocol_error _ ] -> ()
  | _ -> Alcotest.fail "expected Protocol_error for masked server frame"

let test_orphan_continuation () =
  let events, _ = drain C.Server (cf ~fin:true F.Opcode.Continuation "x") in
  match events with
  | [ C.Protocol_error _ ] -> ()
  | _ -> Alcotest.fail "expected Protocol_error for orphan continuation"

let test_interrupted_fragmentation () =
  let wire =
    cf ~fin:false F.Opcode.Text "AA" ^ cf ~fin:true F.Opcode.Text "BB"
  in
  let events, _ = drain C.Server wire in
  match events with
  | [ C.Protocol_error _ ] -> ()
  | _ ->
    Alcotest.failf "expected Protocol_error, got %d events" (List.length events)

let test_close_with_code () =
  let events, _ = drain C.Server (cf F.Opcode.Close "\x03\xe8bye") in
  match events with
  | [ C.Close { code = Some 1000; reason = "bye" } ] -> ()
  | _ -> Alcotest.fail "expected Close code=1000 reason=bye"

let test_empty_close () =
  let events, _ = drain C.Server (cf F.Opcode.Close "") in
  match events with
  | [ C.Close { code = None; reason = "" } ] -> ()
  | _ -> Alcotest.fail "expected Close with no code"

let () =
  Alcotest.run "ws-direct-core connection"
    [ ( "drain + reassembly"
      , [ Alcotest.test_case "coalesced 3 fragments (headline)" `Quick
            test_coalesced_fragments
        ; Alcotest.test_case "interleaved control frame" `Quick
            test_interleaved_control
        ; Alcotest.test_case "single unfragmented message" `Quick
            test_single_message
        ; Alcotest.test_case "partial trailing frame" `Quick test_partial_tail
        ; Alcotest.test_case "reassembly across reads" `Quick
            test_reassembly_across_reads
        ] )
    ; ( "masking direction"
      , [ Alcotest.test_case "server requires masked" `Quick
            test_server_requires_mask
        ; Alcotest.test_case "client accepts unmasked" `Quick
            test_client_accepts_unmasked
        ; Alcotest.test_case "client rejects masked" `Quick
            test_client_rejects_masked
        ] )
    ; ( "protocol errors"
      , [ Alcotest.test_case "orphan continuation" `Quick
            test_orphan_continuation
        ; Alcotest.test_case "interrupted fragmentation" `Quick
            test_interrupted_fragmentation
        ] )
    ; ( "close frame"
      , [ Alcotest.test_case "close with code+reason" `Quick test_close_with_code
        ; Alcotest.test_case "empty close" `Quick test_empty_close
        ] )
    ]
