module H = Ws_direct_eio.Handshake

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* RFC 6455 §1.3 worked example: this exact key must yield this exact accept. *)
let test_accept_golden () =
  Alcotest.(check string) "accept token" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (H.accept_token "dGhlIHNhbXBsZSBub25jZQ==")

let test_make_key_length () =
  (* 16 raw bytes base64-encode to 24 padded characters *)
  let key = H.make_key (fun n -> String.make n '\x00') in
  Alcotest.(check int) "key chars" 24 (String.length key);
  Alcotest.(check string) "16 zero bytes" "AAAAAAAAAAAAAAAAAAAAAA==" key

let test_request_format () =
  let req = H.request ~host:"example.com" ~resource:"/chat" ~key:"the-key" in
  List.iter
    (fun line ->
      Alcotest.(check bool) (Printf.sprintf "contains %S" line) true
        (contains req line))
    [ "GET /chat HTTP/1.1\r\n"
    ; "Host: example.com\r\n"
    ; "Upgrade: websocket\r\n"
    ; "Connection: Upgrade\r\n"
    ; "Sec-WebSocket-Key: the-key\r\n"
    ; "Sec-WebSocket-Version: 13\r\n"
    ];
  let n = String.length req in
  Alcotest.(check bool) "ends with blank line" true
    (n >= 4 && String.sub req (n - 4) 4 = "\r\n\r\n")

let key = "dGhlIHNhbXBsZSBub25jZQ=="
let good_accept = H.accept_token key

let response ?(status = "101 Switching Protocols") ?(upgrade = "websocket")
    ?(connection = "Upgrade") ~accept () =
  String.concat "\r\n"
    [ "HTTP/1.1 " ^ status
    ; "Upgrade: " ^ upgrade
    ; "Connection: " ^ connection
    ; "Sec-WebSocket-Accept: " ^ accept
    ; ""
    ; ""
    ]

let ok = function Ok () -> true | Error _ -> false

let test_response_ok () =
  Alcotest.(check bool) "accepts valid 101" true
    (ok (H.check_response ~key (response ~accept:good_accept ())))

let test_response_bad_accept () =
  Alcotest.(check bool) "rejects wrong accept" false
    (ok (H.check_response ~key (response ~accept:"wrongtoken" ())))

let test_response_non_101 () =
  Alcotest.(check bool) "rejects non-101" false
    (ok
       (H.check_response ~key
          (response ~status:"400 Bad Request" ~accept:good_accept ())))

let test_response_missing_upgrade () =
  let r =
    String.concat "\r\n"
      [ "HTTP/1.1 101 Switching Protocols"
      ; "Connection: Upgrade"
      ; "Sec-WebSocket-Accept: " ^ good_accept
      ; ""
      ; ""
      ]
  in
  Alcotest.(check bool) "rejects missing Upgrade" false (ok (H.check_response ~key r))

(* Header names are case-insensitive and Connection may be a token list. *)
let test_response_case_insensitive () =
  let r =
    String.concat "\r\n"
      [ "HTTP/1.1 101 Switching Protocols"
      ; "upgrade: websocket"
      ; "CONNECTION: keep-alive, Upgrade"
      ; "sec-websocket-accept: " ^ good_accept
      ; ""
      ; ""
      ]
  in
  Alcotest.(check bool) "accepts case-insensitive headers" true
    (ok (H.check_response ~key r))

let () =
  Alcotest.run "ws-direct-eio handshake"
    [ ( "accept + request"
      , [ Alcotest.test_case "accept token golden (RFC §1.3)" `Quick
            test_accept_golden
        ; Alcotest.test_case "make_key length" `Quick test_make_key_length
        ; Alcotest.test_case "request format" `Quick test_request_format
        ] )
    ; ( "response validation"
      , [ Alcotest.test_case "valid 101 accepted" `Quick test_response_ok
        ; Alcotest.test_case "wrong accept rejected" `Quick
            test_response_bad_accept
        ; Alcotest.test_case "non-101 rejected" `Quick test_response_non_101
        ; Alcotest.test_case "missing Upgrade rejected" `Quick
            test_response_missing_upgrade
        ; Alcotest.test_case "case-insensitive headers" `Quick
            test_response_case_insensitive
        ] )
    ]
