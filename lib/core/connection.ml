module Message = struct
  type kind =
    | Text
    | Binary

  type t =
    { kind : kind
    ; payload : Bigstringaf.t
    }
end

type role =
  | Server
  | Client

type event =
  | Message of Message.t
  | Ping of Bigstringaf.t
  | Pong of Bigstringaf.t
  | Close of
      { code : int option
      ; reason : string
      }
  | Fail of
      { code : Close_code.t
      ; reason : string
      }

type t =
  { role : role
  ; max_message : int (* cap on a complete message, single-frame or reassembled *)
  ; max_frame : int (* cap on one frame's payload (DoS guard before reassembly) *)
  ; mutable frag_kind : Message.kind option
      (* the data kind of an in-progress fragmented message, if any *)
  ; mutable frag_chunks : Bigstringaf.t list (* reversed: most recent first *)
  ; mutable frag_len : int
  ; mutable frag_utf8 : Utf8.t option
      (* incremental UTF-8 decoder for an in-progress fragmented Text message;
         [None] for Binary (RFC 6455 §8.1 only constrains Text) *)
  }

let create ?(max_message = Frame.default_max_payload)
    ?(max_frame = Frame.default_max_payload) role =
  { role
  ; max_message
  ; max_frame
  ; frag_kind = None
  ; frag_chunks = []
  ; frag_len = 0
  ; frag_utf8 = None
  }

let reset_frag t =
  t.frag_kind <- None;
  t.frag_chunks <- [];
  t.frag_len <- 0;
  t.frag_utf8 <- None

let kind_of_data_opcode : Frame.Opcode.t -> Message.kind = function
  | Frame.Opcode.Text -> Message.Text
  | Frame.Opcode.Binary -> Message.Binary
  | _ -> assert false (* only called for Text/Binary *)

(* Concatenate reversed chunks (most-recent-first) into one bigstring in
   original order. *)
let concat_rev chunks total =
  let out = Bigstringaf.create total in
  let pos = ref total in
  List.iter
    (fun c ->
      let n = Bigstringaf.length c in
      pos := !pos - n;
      Bigstringaf.blit c ~src_off:0 out ~dst_off:!pos ~len:n)
    chunks;
  out

let fail_protocol reason = Fail { code = Close_code.protocol_error; reason }
let fail_utf8 reason = Fail { code = Close_code.invalid_payload; reason }

let bs_len = Bigstringaf.length

(* RFC 6455 §5.5.1 / §7.4.1: a Close body is empty, or a 2-byte status code
   followed by a UTF-8 reason. A 1-byte body is malformed; the status code must
   be one an endpoint is permitted to send; the reason must be valid UTF-8.
   Returns the user-visible (code, reason) on success, or a failure event. *)
let parse_close payload =
  let len = bs_len payload in
  if len = 0 then Ok (None, "")
  else if len = 1 then Error (fail_protocol "1-byte close payload")
  else
    let code =
      (Char.code (Bigstringaf.get payload 0) lsl 8)
      lor Char.code (Bigstringaf.get payload 1)
    in
    match Close_code.of_wire code with
    | Error msg -> Error (fail_protocol msg)
    | Ok _ ->
      let reason = Bigstringaf.substring payload ~off:2 ~len:(len - 2) in
      if Utf8.valid_string reason then Ok (Some code, reason)
      else Error (fail_utf8 "close reason is not valid UTF-8")

let masking_ok t (p : Frame.parsed) =
  match t.role with Server -> p.masked | Client -> not p.masked

(* Start (or refuse to start) a fragmented data message from its first,
   non-final frame. For Text, seed the incremental UTF-8 decoder and fail fast
   if the opening bytes are already invalid. *)
let begin_fragment t kind payload =
  match kind with
  | Message.Binary ->
    t.frag_kind <- Some kind;
    t.frag_chunks <- [ payload ];
    t.frag_len <- bs_len payload;
    t.frag_utf8 <- None;
    None
  | Message.Text ->
    let u = Utf8.create () in
    if not (Utf8.feed u payload ~off:0 ~len:(bs_len payload)) then
      Some (fail_utf8 "text message is not valid UTF-8")
    else begin
      t.frag_kind <- Some kind;
      t.frag_chunks <- [ payload ];
      t.frag_len <- bs_len payload;
      t.frag_utf8 <- Some u;
      None
    end

let handle_frame t (p : Frame.parsed) : event option =
  let f = p.Frame.frame in
  if not (masking_ok t p) then
    Some
      (fail_protocol
         (match t.role with
         | Server -> "client frame must be masked"
         | Client -> "server frame must not be masked"))
  else if f.Frame.rsv1 || f.Frame.rsv2 || f.Frame.rsv3 then
    Some (fail_protocol "RSV bit set without a negotiated extension")
  else
    match f.Frame.opcode with
    | Frame.Opcode.Ping -> Some (Ping f.Frame.payload)
    | Frame.Opcode.Pong -> Some (Pong f.Frame.payload)
    | Frame.Opcode.Close -> (
      match parse_close f.Frame.payload with
      | Ok (code, reason) -> Some (Close { code; reason })
      | Error ev -> Some ev)
    | (Frame.Opcode.Text | Frame.Opcode.Binary) as op ->
      if t.frag_kind <> None then
        Some
          (fail_protocol "expected a continuation frame, got a new data frame")
      else begin
        let kind = kind_of_data_opcode op in
        if not f.Frame.fin then begin_fragment t kind f.Frame.payload
        else if bs_len f.Frame.payload > t.max_message then
          (* a single frame is itself a complete message, so it must obey the
             message cap — the fragmented path is not the only way to exceed it *)
          Some
            (Fail
               { code = Close_code.message_too_big
               ; reason = "message exceeds maximum size"
               })
        else begin
          (* a complete, unfragmented message *)
          match kind with
          | Message.Text
            when not
                   (Utf8.valid_bigstring f.Frame.payload ~off:0
                      ~len:(bs_len f.Frame.payload)) ->
            Some (fail_utf8 "text message is not valid UTF-8")
          | _ -> Some (Message { Message.kind; payload = f.Frame.payload })
        end
      end
    | Frame.Opcode.Continuation -> (
      match t.frag_kind with
      | None -> Some (fail_protocol "unexpected continuation frame")
      | Some kind ->
        let new_len = t.frag_len + bs_len f.Frame.payload in
        if new_len > t.max_message then begin
          reset_frag t;
          Some
            (Fail
               { code = Close_code.message_too_big
               ; reason = "reassembled message exceeds maximum size"
               })
        end
        else begin
          let utf8_ok =
            match t.frag_utf8 with
            | Some u -> Utf8.feed u f.Frame.payload ~off:0 ~len:(bs_len f.Frame.payload)
            | None -> true
          in
          if not utf8_ok then begin
            reset_frag t;
            Some (fail_utf8 "text message is not valid UTF-8")
          end
          else begin
            t.frag_chunks <- f.Frame.payload :: t.frag_chunks;
            t.frag_len <- new_len;
            if not f.Frame.fin then None
            else
              match t.frag_utf8 with
              | Some u when not (Utf8.is_complete u) ->
                reset_frag t;
                Some (fail_utf8 "text message ends mid-sequence")
              | _ ->
                let payload = concat_rev t.frag_chunks t.frag_len in
                reset_frag t;
                Some (Message { Message.kind; payload })
          end
        end)

(* A terminal event ends inbound processing for this connection: a [Fail] (RFC
   6455 §7.1.7 — once the connection is failed, no further data may be processed)
   or a peer [Close] (§5.5.1/§7.1.4 — a data frame received after a Close is not
   delivered). [Message]/[Ping]/[Pong] are non-terminal. *)
let is_terminal_event = function
  | Close _ | Fail _ -> true
  | Message _ | Ping _ | Pong _ -> false

let read_bytes t bs ~off ~len =
  let rec loop acc consumed =
    if consumed >= len then (List.rev acc, consumed)
    else
      match
        Frame.parse ~max_payload:t.max_frame bs ~off:(off + consumed)
          ~len:(len - consumed)
      with
      | Frame.Incomplete -> (List.rev acc, consumed)
      | Frame.Protocol_error msg ->
        (List.rev (fail_protocol msg :: acc), consumed)
      | Frame.Frame (p, n) -> (
        match handle_frame t p with
        | Some ev when is_terminal_event ev ->
          (* Stop at the first terminal event, included as the final event
             (mirroring the Protocol_error arm). The terminal frame's [n] bytes
             are consumed; any trailing bytes in this buffer are deliberately
             left unparsed — the driver shuts the connection down on this
             event, so a later frame must never reach the application. *)
          (List.rev (ev :: acc), consumed + n)
        | Some ev -> loop (ev :: acc) (consumed + n)
        | None -> loop acc (consumed + n))
  in
  loop [] 0
