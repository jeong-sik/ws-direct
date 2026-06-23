module U = Ws_direct_core.Utf8

let valid name s = Alcotest.test_case name `Quick (fun () ->
  Alcotest.(check bool) name true (U.valid_string s))

let invalid name s = Alcotest.test_case name `Quick (fun () ->
  Alcotest.(check bool) name false (U.valid_string s))

(* RFC 3629 well-formed sequences across all four lengths, including the
   boundary code points. *)
let valid_cases =
  [ valid "empty" ""
  ; valid "ascii" "hello world"
  ; valid "2-byte min U+0080" "\xc2\x80"
  ; valid "2-byte copyright" "\xc2\xa9"
  ; valid "3-byte min U+0800" "\xe0\xa0\x80"
  ; valid "3-byte euro U+20AC" "\xe2\x82\xac"
  ; valid "4-byte min U+10000" "\xf0\x90\x80\x80"
  ; valid "4-byte max U+10FFFF" "\xf4\x8f\xbf\xbf"
  ; valid "mixed" "a\xc2\xa9b\xe2\x82\xacc\xf0\x90\x80\x80"
  ]

(* The classic UTF-8 attack/edge vectors that a naive validator lets through. *)
let invalid_cases =
  [ invalid "stray continuation 0x80" "\x80"
  ; invalid "continuation after ascii" "a\x80"
  ; invalid "lead 0xC0 (overlong)" "\xc0\x80"
  ; invalid "lead 0xC1 (overlong)" "\xc1\xbf"
  ; invalid "overlong 3-byte NUL" "\xe0\x80\x80"
  ; invalid "overlong 4-byte" "\xf0\x80\x80\x80"
  ; invalid "surrogate U+D800" "\xed\xa0\x80"
  ; invalid "surrogate U+DFFF" "\xed\xbf\xbf"
  ; invalid "above U+10FFFF" "\xf4\x90\x80\x80"
  ; invalid "lead 0xF5" "\xf5\x80\x80\x80"
  ; invalid "lead 0xFF" "\xff"
  ; invalid "truncated 2-byte" "\xc2"
  ; invalid "truncated 3-byte" "\xe2\x82"
  ; invalid "missing continuation" "\xe2\x82\x28"
  ]

(* A multi-byte code point split across two [feed] calls must validate as one
   stream — this is exactly the WebSocket fragmented-text case. *)
let test_split_valid () =
  let t = U.create () in
  let _ = U.feed_string t "\xe2\x82" in
  Alcotest.(check bool) "incomplete mid-codepoint" false (U.is_complete t);
  let _ = U.feed_string t "\xac" in
  Alcotest.(check bool) "complete after final byte" true (U.is_complete t)

let test_split_invalid () =
  let t = U.create () in
  let _ = U.feed_string t "\xe2\x82" in
  let still_ok = U.feed_string t "\x28" in
  Alcotest.(check bool) "rejected on bad continuation" false still_ok

let () =
  Alcotest.run "ws-direct-core utf8"
    [ ("valid", valid_cases)
    ; ("invalid", invalid_cases)
    ; ( "incremental"
      , [ Alcotest.test_case "valid split across feeds" `Quick test_split_valid
        ; Alcotest.test_case "invalid across feeds" `Quick test_split_invalid
        ] )
    ]
