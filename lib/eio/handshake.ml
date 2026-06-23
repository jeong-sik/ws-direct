(* RFC 6455 §1.3: the fixed GUID concatenated with the client key before
   hashing for Sec-WebSocket-Accept. *)
let websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let accept_token key =
  Digestif.SHA1.digest_string (key ^ websocket_guid)
  |> Digestif.SHA1.to_raw_string
  |> Base64.encode_string

(* §4.1: the key is base64 of a fresh 16-byte nonce. *)
let nonce_bytes = 16

let make_key random = Base64.encode_string (random nonce_bytes)

let request ~host ~resource ~key =
  String.concat "\r\n"
    [ "GET " ^ resource ^ " HTTP/1.1"
    ; "Host: " ^ host
    ; "Upgrade: websocket"
    ; "Connection: Upgrade"
    ; "Sec-WebSocket-Key: " ^ key
    ; "Sec-WebSocket-Version: 13"
    ; ""
    ; ""
    ]

(* Split a header block on CRLF (tolerating bare LF) and drop the trailing
   empty line(s). *)
let split_lines head =
  String.split_on_char '\n' head
  |> List.map (fun s ->
         let n = String.length s in
         if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s)

let parse_status line =
  match String.split_on_char ' ' line with
  | _proto :: code :: _ -> ( try Some (int_of_string code) with _ -> None)
  | _ -> None

let parse_header line =
  match String.index_opt line ':' with
  | None -> None
  | Some i ->
    let name = String.sub line 0 i |> String.trim |> String.lowercase_ascii in
    let value =
      String.sub line (i + 1) (String.length line - i - 1) |> String.trim
    in
    Some (name, value)

(* A [Connection] header may be a comma-separated list; §4.2.2 requires it to
   contain the "upgrade" token (case-insensitively). *)
let connection_lists_upgrade value =
  String.split_on_char ',' value
  |> List.exists (fun tok ->
         String.lowercase_ascii (String.trim tok) = "upgrade")

let check_response ~key head =
  match split_lines head with
  | [] -> Error "empty response"
  | status_line :: header_lines -> (
    match parse_status status_line with
    | Some 101 -> (
      let headers = List.filter_map parse_header header_lines in
      let find n = List.assoc_opt n headers in
      let upgrade_ok =
        match find "upgrade" with
        | Some v -> String.lowercase_ascii v = "websocket"
        | None -> false
      in
      let connection_ok =
        match find "connection" with
        | Some v -> connection_lists_upgrade v
        | None -> false
      in
      if not upgrade_ok then Error "missing or invalid Upgrade header"
      else if not connection_ok then Error "missing or invalid Connection header"
      else
        match find "sec-websocket-accept" with
        | None -> Error "missing Sec-WebSocket-Accept"
        | Some got ->
          if String.equal got (accept_token key) then Ok ()
          else Error "Sec-WebSocket-Accept mismatch")
    | Some code -> Error (Printf.sprintf "expected HTTP 101, got %d" code)
    | None -> Error (Printf.sprintf "malformed status line: %s" status_line))

let request_key head =
  match split_lines head with
  | [] -> Error "empty request"
  | request_line :: header_lines ->
    let is_get =
      match String.split_on_char ' ' request_line with
      | meth :: _ -> String.uppercase_ascii meth = "GET"
      | [] -> false
    in
    if not is_get then Error "not a GET request"
    else
      let headers = List.filter_map parse_header header_lines in
      let find n = List.assoc_opt n headers in
      let upgrade_ok =
        match find "upgrade" with
        | Some v -> String.lowercase_ascii v = "websocket"
        | None -> false
      in
      let connection_ok =
        match find "connection" with
        | Some v -> connection_lists_upgrade v
        | None -> false
      in
      let version_ok =
        match find "sec-websocket-version" with
        | Some v -> String.trim v = "13"
        | None -> false
      in
      if not upgrade_ok then Error "missing or invalid Upgrade header"
      else if not connection_ok then Error "missing or invalid Connection header"
      else if not version_ok then Error "unsupported Sec-WebSocket-Version"
      else (
        match find "sec-websocket-key" with
        | Some k -> Ok k
        | None -> Error "missing Sec-WebSocket-Key")

let server_response ~key =
  String.concat "\r\n"
    [ "HTTP/1.1 101 Switching Protocols"
    ; "Upgrade: websocket"
    ; "Connection: Upgrade"
    ; "Sec-WebSocket-Accept: " ^ accept_token key
    ; ""
    ; ""
    ]
