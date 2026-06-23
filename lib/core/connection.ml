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
  | Protocol_error of string

type t =
  { role : role
  ; max_message : int
  ; mutable frag_kind : Message.kind option
      (* the data kind of an in-progress fragmented message, if any *)
  ; mutable frag_chunks : Bigstringaf.t list (* reversed: most recent first *)
  ; mutable frag_len : int
  }

let create ?(max_message = Frame.default_max_payload) role =
  { role; max_message; frag_kind = None; frag_chunks = []; frag_len = 0 }

let reset_frag t =
  t.frag_kind <- None;
  t.frag_chunks <- [];
  t.frag_len <- 0

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

(* RFC 6455 §5.5.1: close payload is empty, or a 2-byte code + UTF-8 reason. A
   1-byte payload is malformed (reported as no code). *)
let parse_close payload =
  let len = Bigstringaf.length payload in
  if len < 2 then (None, "")
  else
    let code =
      (Char.code (Bigstringaf.get payload 0) lsl 8)
      lor Char.code (Bigstringaf.get payload 1)
    in
    let reason = Bigstringaf.substring payload ~off:2 ~len:(len - 2) in
    (Some code, reason)

let masking_ok t (p : Frame.parsed) =
  match t.role with Server -> p.masked | Client -> not p.masked

let handle_frame t (p : Frame.parsed) : event option =
  let f = p.Frame.frame in
  if not (masking_ok t p) then
    Some
      (Protocol_error
         (match t.role with
         | Server -> "client frame must be masked"
         | Client -> "server frame must not be masked"))
  else if f.Frame.rsv1 || f.Frame.rsv2 || f.Frame.rsv3 then
    Some (Protocol_error "RSV bit set without a negotiated extension")
  else
    match f.Frame.opcode with
    | Frame.Opcode.Ping -> Some (Ping f.Frame.payload)
    | Frame.Opcode.Pong -> Some (Pong f.Frame.payload)
    | Frame.Opcode.Close ->
      let code, reason = parse_close f.Frame.payload in
      Some (Close { code; reason })
    | (Frame.Opcode.Text | Frame.Opcode.Binary) as op ->
      if t.frag_kind <> None then
        Some (Protocol_error "expected a continuation frame, got a new data frame")
      else if f.Frame.fin then
        Some
          (Message
             { Message.kind = kind_of_data_opcode op; payload = f.Frame.payload })
      else begin
        t.frag_kind <- Some (kind_of_data_opcode op);
        t.frag_chunks <- [ f.Frame.payload ];
        t.frag_len <- Bigstringaf.length f.Frame.payload;
        None
      end
    | Frame.Opcode.Continuation -> (
      match t.frag_kind with
      | None -> Some (Protocol_error "unexpected continuation frame")
      | Some kind ->
        let new_len = t.frag_len + Bigstringaf.length f.Frame.payload in
        if new_len > t.max_message then begin
          reset_frag t;
          Some (Protocol_error "reassembled message exceeds maximum size")
        end
        else begin
          t.frag_chunks <- f.Frame.payload :: t.frag_chunks;
          t.frag_len <- new_len;
          if f.Frame.fin then begin
            let payload = concat_rev t.frag_chunks t.frag_len in
            reset_frag t;
            Some (Message { Message.kind; payload })
          end
          else None
        end)

let read_bytes t bs ~off ~len =
  let rec loop acc consumed =
    if consumed >= len then (List.rev acc, consumed)
    else
      match Frame.parse bs ~off:(off + consumed) ~len:(len - consumed) with
      | Frame.Incomplete -> (List.rev acc, consumed)
      | Frame.Protocol_error msg -> (List.rev (Protocol_error msg :: acc), consumed)
      | Frame.Frame (p, n) ->
        let acc =
          match handle_frame t p with Some ev -> ev :: acc | None -> acc
        in
        loop acc (consumed + n)
  in
  loop [] 0
