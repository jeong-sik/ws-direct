module F = Ws_direct_core.Frame
module C = Ws_direct_core.Connection
module CC = Ws_direct_core.Close_code

let bs_of_string s = Bigstringaf.of_string s ~off:0 ~len:(String.length s)
let mask = "\x01\x02\x03\x04"

(* client -> server wire frame: masked *)
let cf ?(fin = true) opcode s = F.to_string ~mask (F.of_string ~fin opcode s)

(* server -> client wire frame: unmasked *)
let sf ?(fin = true) opcode s = F.to_string (F.of_string ~fin opcode s)

let drain role wire =
  C.read_bytes (C.create role) (bs_of_string wire) ~off:0
    ~len:(String.length wire)

(* Assert the events are exactly one [Fail] carrying [code]. *)
let check_fail ~code label events =
  match events with
  | [ C.Fail { code = got; _ } ] ->
    Alcotest.(check int) label code (CC.to_int got)
  | _ -> Alcotest.failf "%s: expected a single Fail event" label

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

(* --- masking direction (RFC 6455 §5.3) ---------------------------------- *)

let test_server_requires_mask () =
  drain C.Server (sf F.Opcode.Text "x") |> fst
  |> check_fail ~code:1002 "unmasked client frame"

let test_client_accepts_unmasked () =
  let events, _ = drain C.Client (sf F.Opcode.Binary "data") in
  match events with
  | [ C.Message { kind = C.Message.Binary; payload } ] ->
    Alcotest.(check string) "payload" "data" (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected Binary message"

let test_client_rejects_masked () =
  drain C.Client (cf F.Opcode.Text "x") |> fst
  |> check_fail ~code:1002 "masked server frame"

(* --- fragmentation errors ----------------------------------------------- *)

let test_orphan_continuation () =
  drain C.Server (cf ~fin:true F.Opcode.Continuation "x") |> fst
  |> check_fail ~code:1002 "orphan continuation"

let test_interrupted_fragmentation () =
  let wire =
    cf ~fin:false F.Opcode.Text "AA" ^ cf ~fin:true F.Opcode.Text "BB"
  in
  drain C.Server wire |> fst |> check_fail ~code:1002 "interrupted fragmentation"

(* --- UTF-8 (RFC 6455 §8.1) ---------------------------------------------- *)

let test_text_invalid_utf8 () =
  drain C.Server (cf F.Opcode.Text "\xff") |> fst
  |> check_fail ~code:1007 "invalid UTF-8 text"

let test_text_valid_utf8 () =
  let events, _ = drain C.Server (cf F.Opcode.Text "\xe2\x82\xac") in
  match events with
  | [ C.Message { kind = C.Message.Text; payload } ] ->
    Alcotest.(check string) "euro" "\xe2\x82\xac" (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected valid UTF-8 text message"

(* a 3-byte code point split across two text fragments must reassemble *)
let test_utf8_split_valid () =
  let wire =
    cf ~fin:false F.Opcode.Text "\xe2\x82"
    ^ cf ~fin:true F.Opcode.Continuation "\xac"
  in
  let events, _ = drain C.Server wire in
  match events with
  | [ C.Message { payload; _ } ] ->
    Alcotest.(check string) "joined euro" "\xe2\x82\xac"
      (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected reassembled valid UTF-8"

(* a multi-byte sequence broken by a non-continuation byte across fragments *)
let test_utf8_split_invalid () =
  let wire =
    cf ~fin:false F.Opcode.Text "\xe2\x82"
    ^ cf ~fin:true F.Opcode.Continuation "\x28"
  in
  drain C.Server wire |> fst |> check_fail ~code:1007 "invalid UTF-8 across fragments"

(* a Text message that ends mid-codepoint is incomplete, hence invalid *)
let test_utf8_truncated_at_fin () =
  let wire =
    cf ~fin:false F.Opcode.Text "ab"
    ^ cf ~fin:true F.Opcode.Continuation "\xe2\x82"
  in
  drain C.Server wire |> fst |> check_fail ~code:1007 "truncated UTF-8 at FIN"

(* --- close frames (RFC 6455 §5.5.1 / §7.4.1) ---------------------------- *)

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

let test_close_app_code () =
  (* 0x0bb8 = 3000, a valid application close code *)
  let events, _ = drain C.Server (cf F.Opcode.Close "\x0b\xb8ok") in
  match events with
  | [ C.Close { code = Some 3000; reason = "ok" } ] -> ()
  | _ -> Alcotest.fail "expected Close code=3000 reason=ok"

let test_close_one_byte () =
  drain C.Server (cf F.Opcode.Close "\x03") |> fst
  |> check_fail ~code:1002 "1-byte close payload"

let test_close_reserved_code () =
  (* 0x03ed = 1005, reserved, MUST NOT appear on the wire *)
  drain C.Server (cf F.Opcode.Close "\x03\xed") |> fst
  |> check_fail ~code:1002 "reserved close code 1005"

let test_close_bad_reason_utf8 () =
  (* 0x03e8 = 1000 with an invalid-UTF-8 reason byte *)
  drain C.Server (cf F.Opcode.Close "\x03\xe8\xff") |> fst
  |> check_fail ~code:1007 "non-UTF-8 close reason"

(* --- no processing after a terminal event (RFC 6455 §7.1.7, §5.5.1) ------ *)

(* A frame that fails the connection stops the drain: a later frame coalesced
   into the same buffer must not be delivered (RFC 6455 §7.1.7). *)
let test_no_processing_after_fail () =
  let bad = sf F.Opcode.Text "x" (* unmasked from a client -> Fail 1002 *) in
  let wire = bad ^ cf F.Opcode.Text "after" in
  let events, consumed = drain C.Server wire in
  Alcotest.(check int) "consumes only up to the failing frame"
    (String.length bad) consumed;
  check_fail ~code:1002 "fail then stop" events

(* A data frame received after a Close is not delivered (RFC 6455 §5.5.1). *)
let test_no_data_after_close () =
  let close = cf F.Opcode.Close "\x03\xe8bye" in
  let wire = close ^ cf F.Opcode.Text "after" in
  let events, consumed = drain C.Server wire in
  Alcotest.(check int) "consumes only up to the Close frame"
    (String.length close) consumed;
  match events with
  | [ C.Close { code = Some 1000; reason = "bye" } ] -> ()
  | _ ->
    Alcotest.failf "expected exactly [Close], got %d events"
      (List.length events)

(* --- size caps (RFC 6455 §7.4.1 1009) ----------------------------------- *)

(* a single (unfragmented) frame is a complete message and must obey max_message *)
let test_single_frame_over_max_message () =
  let c = C.create ~max_message:4 C.Server in
  let wire = cf F.Opcode.Binary "abcdef" in
  let events, _ = C.read_bytes c (bs_of_string wire) ~off:0 ~len:(String.length wire) in
  check_fail ~code:1009 "single frame over max_message" events

let test_single_frame_within_max_message () =
  let c = C.create ~max_message:8 C.Server in
  let wire = cf F.Opcode.Binary "abcd" in
  let events, _ = C.read_bytes c (bs_of_string wire) ~off:0 ~len:(String.length wire) in
  match events with
  | [ C.Message { payload; _ } ] ->
    Alcotest.(check string) "delivered" "abcd" (Bigstringaf.to_string payload)
  | _ -> Alcotest.fail "expected a delivered message within the cap"

(* a frame whose payload exceeds max_frame is rejected at parse (1002) *)
let test_frame_over_max_frame () =
  let c = C.create ~max_frame:4 C.Server in
  let wire = cf F.Opcode.Binary "abcdef" in
  let events, _ = C.read_bytes c (bs_of_string wire) ~off:0 ~len:(String.length wire) in
  check_fail ~code:1002 "frame over max_frame" events

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
    ; ( "fragmentation errors"
      , [ Alcotest.test_case "orphan continuation" `Quick
            test_orphan_continuation
        ; Alcotest.test_case "interrupted fragmentation" `Quick
            test_interrupted_fragmentation
        ] )
    ; ( "utf-8 validation"
      , [ Alcotest.test_case "invalid single-frame text" `Quick
            test_text_invalid_utf8
        ; Alcotest.test_case "valid single-frame text" `Quick
            test_text_valid_utf8
        ; Alcotest.test_case "valid split across fragments" `Quick
            test_utf8_split_valid
        ; Alcotest.test_case "invalid split across fragments" `Quick
            test_utf8_split_invalid
        ; Alcotest.test_case "truncated at FIN" `Quick
            test_utf8_truncated_at_fin
        ] )
    ; ( "close frame"
      , [ Alcotest.test_case "close with code+reason" `Quick test_close_with_code
        ; Alcotest.test_case "empty close" `Quick test_empty_close
        ; Alcotest.test_case "application close code" `Quick test_close_app_code
        ; Alcotest.test_case "1-byte close -> 1002" `Quick test_close_one_byte
        ; Alcotest.test_case "reserved close code -> 1002" `Quick
            test_close_reserved_code
        ; Alcotest.test_case "non-UTF-8 reason -> 1007" `Quick
            test_close_bad_reason_utf8
        ] )
    ; ( "terminal stop"
      , [ Alcotest.test_case "no frame processed after Fail" `Quick
            test_no_processing_after_fail
        ; Alcotest.test_case "no data frame delivered after Close" `Quick
            test_no_data_after_close
        ] )
    ; ( "size caps"
      , [ Alcotest.test_case "single frame over max_message -> 1009" `Quick
            test_single_frame_over_max_message
        ; Alcotest.test_case "single frame within max_message" `Quick
            test_single_frame_within_max_message
        ; Alcotest.test_case "frame over max_frame -> 1002" `Quick
            test_frame_over_max_frame
        ] )
    ]
