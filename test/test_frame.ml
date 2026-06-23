module F = Ws_direct_core.Frame

let bs_of_string s = Bigstringaf.of_string s ~off:0 ~len:(String.length s)

(* --- Golden vectors (RFC 6455 §5.7) ------------------------------------- *)

let test_parse_unmasked_text () =
  let wire = "\x81\x05Hello" in
  match F.parse (bs_of_string wire) ~off:0 ~len:(String.length wire) with
  | F.Frame ({ frame; masked }, n) ->
    Alcotest.(check bool) "fin" true frame.F.fin;
    Alcotest.(check bool) "masked" false masked;
    Alcotest.(check int) "consumed" 7 n;
    Alcotest.(check bool) "opcode" true (frame.F.opcode = F.Opcode.Text);
    Alcotest.(check string) "payload" "Hello" (F.payload_string frame)
  | _ -> Alcotest.fail "expected Frame"

let test_serialize_unmasked_text () =
  let fr = F.of_string F.Opcode.Text "Hello" in
  Alcotest.(check string) "wire" "\x81\x05Hello" (F.to_string fr)

(* RFC 6455 §5.7: masked "Hello" with key 0x37fa213d. *)
let masked_hello = "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58"

let test_parse_masked_text () =
  match
    F.parse (bs_of_string masked_hello) ~off:0
      ~len:(String.length masked_hello)
  with
  | F.Frame ({ frame; masked }, n) ->
    Alcotest.(check bool) "masked" true masked;
    Alcotest.(check int) "consumed" 11 n;
    Alcotest.(check string) "payload" "Hello" (F.payload_string frame)
  | _ -> Alcotest.fail "expected Frame"

let test_serialize_masked_text () =
  let fr = F.of_string F.Opcode.Text "Hello" in
  Alcotest.(check string) "wire" masked_hello
    (F.to_string ~mask:"\x37\xfa\x21\x3d" fr)

let test_ping_golden () =
  let fr = F.of_string F.Opcode.Ping "Hello" in
  Alcotest.(check string) "wire" "\x89\x05Hello" (F.to_string fr)

(* --- Incomplete / error paths ------------------------------------------- *)

let test_incomplete () =
  let wire = "\x81\x05Hel" in
  (* header claims 5 payload bytes, only 3 present *)
  match F.parse (bs_of_string wire) ~off:0 ~len:(String.length wire) with
  | F.Incomplete -> ()
  | _ -> Alcotest.fail "expected Incomplete"

let test_incomplete_header () =
  let wire = "\x81" in
  match F.parse (bs_of_string wire) ~off:0 ~len:(String.length wire) with
  | F.Incomplete -> ()
  | _ -> Alcotest.fail "expected Incomplete (short header)"

let test_reserved_opcode () =
  let wire = "\x83\x00" in
  (* fin, opcode 0x3 (reserved non-control), len 0 *)
  match F.parse (bs_of_string wire) ~off:0 ~len:(String.length wire) with
  | F.Protocol_error _ -> ()
  | _ -> Alcotest.fail "expected Protocol_error for reserved opcode"

let test_control_too_large () =
  let fr = F.of_string F.Opcode.Ping (String.make 126 'x') in
  let wire = F.to_string fr in
  match F.parse (bs_of_string wire) ~off:0 ~len:(String.length wire) with
  | F.Protocol_error _ -> ()
  | _ -> Alcotest.fail "expected Protocol_error for oversized control frame"

let test_max_payload_guard () =
  (* a header announcing 1000 bytes, parsed with max_payload=10 *)
  let fr = F.of_string F.Opcode.Binary (String.make 1000 'y') in
  let wire = F.to_string fr in
  match
    F.parse ~max_payload:10 (bs_of_string wire) ~off:0
      ~len:(String.length wire)
  with
  | F.Protocol_error _ -> ()
  | _ -> Alcotest.fail "expected Protocol_error when payload exceeds max"

(* --- Round-trip --------------------------------------------------------- *)

let roundtrip ?mask opcode s =
  let wire = F.to_string ?mask (F.of_string opcode s) in
  match F.parse (bs_of_string wire) ~off:0 ~len:(String.length wire) with
  | F.Frame ({ frame; masked }, n) ->
    n = String.length wire
    && masked = (mask <> None)
    && frame.F.opcode = opcode
    && F.payload_string frame = s
  | _ -> false

let test_roundtrip_lengths () =
  List.iter
    (fun len ->
      let s = String.make len 'x' in
      Alcotest.(check bool)
        (Printf.sprintf "rt %d unmasked" len)
        true
        (roundtrip F.Opcode.Binary s);
      Alcotest.(check bool)
        (Printf.sprintf "rt %d masked" len)
        true
        (roundtrip ~mask:"\x01\x02\x03\x04" F.Opcode.Binary s))
    [ 0; 1; 125; 126; 127; 255; 65535; 65536; 70000 ]

(* a frame embedded between other bytes parses with the right consumed count *)
let test_consumed_offset () =
  let wire = "\x81\x05Hello" ^ "TRAILING" in
  match F.parse (bs_of_string wire) ~off:0 ~len:(String.length wire) with
  | F.Frame (_, n) -> Alcotest.(check int) "consumed only first frame" 7 n
  | _ -> Alcotest.fail "expected Frame"

(* --- QCheck properties -------------------------------------------------- *)

let frame_input_gen =
  QCheck.Gen.(
    pair
      (oneof_list F.Opcode.[ Text; Binary; Continuation ])
      (string_size (int_range 0 3000)))

let frame_input_arb = QCheck.make frame_input_gen

let qcheck_roundtrip_unmasked =
  QCheck.Test.make ~count:2000 ~name:"roundtrip unmasked (decode . encode = id)"
    frame_input_arb
    (fun (opcode, s) -> roundtrip opcode s)

let qcheck_roundtrip_masked =
  QCheck.Test.make ~count:2000 ~name:"roundtrip masked (unmask recovers payload)"
    frame_input_arb
    (fun (opcode, s) -> roundtrip ~mask:"\x37\xfa\x21\x3d" opcode s)

let () =
  Alcotest.run "ws-direct-core"
    [ ( "frame golden"
      , [ Alcotest.test_case "parse unmasked text" `Quick
            test_parse_unmasked_text
        ; Alcotest.test_case "serialize unmasked text" `Quick
            test_serialize_unmasked_text
        ; Alcotest.test_case "parse masked text" `Quick test_parse_masked_text
        ; Alcotest.test_case "serialize masked text" `Quick
            test_serialize_masked_text
        ; Alcotest.test_case "ping golden" `Quick test_ping_golden
        ] )
    ; ( "frame errors"
      , [ Alcotest.test_case "incomplete payload" `Quick test_incomplete
        ; Alcotest.test_case "incomplete header" `Quick test_incomplete_header
        ; Alcotest.test_case "reserved opcode" `Quick test_reserved_opcode
        ; Alcotest.test_case "oversized control" `Quick test_control_too_large
        ; Alcotest.test_case "max payload guard" `Quick test_max_payload_guard
        ] )
    ; ( "frame roundtrip"
      , [ Alcotest.test_case "lengths" `Quick test_roundtrip_lengths
        ; Alcotest.test_case "consumed offset" `Quick test_consumed_offset
        ; QCheck_alcotest.to_alcotest qcheck_roundtrip_unmasked
        ; QCheck_alcotest.to_alcotest qcheck_roundtrip_masked
        ] )
    ]
