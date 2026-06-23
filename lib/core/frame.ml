module Opcode = struct
  type t =
    | Continuation
    | Text
    | Binary
    | Close
    | Ping
    | Pong

  let to_int = function
    | Continuation -> 0x0
    | Text -> 0x1
    | Binary -> 0x2
    | Close -> 0x8
    | Ping -> 0x9
    | Pong -> 0xA

  let of_int = function
    | 0x0 -> Ok Continuation
    | 0x1 -> Ok Text
    | 0x2 -> Ok Binary
    | 0x8 -> Ok Close
    | 0x9 -> Ok Ping
    | 0xA -> Ok Pong
    | n -> Error n

  let is_control = function Close | Ping | Pong -> true | _ -> false
end

type t =
  { fin : bool
  ; rsv1 : bool
  ; rsv2 : bool
  ; rsv3 : bool
  ; opcode : Opcode.t
  ; payload : Bigstringaf.t
  }

let create ?(fin = true) ?(rsv1 = false) ?(rsv2 = false) ?(rsv3 = false) opcode
    payload =
  { fin; rsv1; rsv2; rsv3; opcode; payload }

let of_string ?fin ?rsv1 ?rsv2 ?rsv3 opcode s =
  create ?fin ?rsv1 ?rsv2 ?rsv3 opcode
    (Bigstringaf.of_string s ~off:0 ~len:(String.length s))

let payload_string t = Bigstringaf.to_string t.payload

(* Masking is its own inverse: each payload byte is XORed with key[i mod 4]
   (RFC 6455 §5.3). [src] and [dst] may be the same bigstring. *)
let mask_into ~key ~src ~src_off ~dst ~dst_off ~len =
  for i = 0 to len - 1 do
    let k = Char.code (String.unsafe_get key (i land 3)) in
    let b = Char.code (Bigstringaf.unsafe_get src (src_off + i)) in
    Bigstringaf.unsafe_set dst (dst_off + i) (Char.unsafe_chr (b lxor k))
  done

let serialize ?mask t f =
  let payload = t.payload in
  let len = Bigstringaf.length payload in
  let b0 =
    (if t.fin then 0x80 else 0)
    lor (if t.rsv1 then 0x40 else 0)
    lor (if t.rsv2 then 0x20 else 0)
    lor (if t.rsv3 then 0x10 else 0)
    lor Opcode.to_int t.opcode
  in
  Faraday.write_uint8 f b0;
  let mask_bit = match mask with Some _ -> 0x80 | None -> 0x00 in
  if len <= 125 then Faraday.write_uint8 f (mask_bit lor len)
  else if len <= 0xFFFF then begin
    Faraday.write_uint8 f (mask_bit lor 126);
    Faraday.BE.write_uint16 f len
  end
  else begin
    Faraday.write_uint8 f (mask_bit lor 127);
    Faraday.BE.write_uint64 f (Int64.of_int len)
  end;
  match mask with
  | None -> Faraday.write_bigstring f payload
  | Some key ->
    if String.length key <> 4 then
      invalid_arg "Frame.serialize: mask key must be 4 bytes";
    Faraday.write_string f key;
    let masked = Bigstringaf.create len in
    mask_into ~key ~src:payload ~src_off:0 ~dst:masked ~dst_off:0 ~len;
    Faraday.write_bigstring f masked

let to_string ?mask t =
  let f = Faraday.create 256 in
  serialize ?mask t f;
  Faraday.serialize_to_string f

type parsed =
  { frame : t
  ; masked : bool
  }

type parse =
  | Frame of parsed * int
  | Incomplete
  | Protocol_error of string

let default_max_payload = 64 * 1024 * 1024

let parse ?(max_payload = default_max_payload) bs ~off ~len =
  if len < 2 then Incomplete
  else begin
    let b0 = Char.code (Bigstringaf.get bs off) in
    let b1 = Char.code (Bigstringaf.get bs (off + 1)) in
    let fin = b0 land 0x80 <> 0 in
    let rsv1 = b0 land 0x40 <> 0 in
    let rsv2 = b0 land 0x20 <> 0 in
    let rsv3 = b0 land 0x10 <> 0 in
    let opcode_int = b0 land 0x0F in
    let masked = b1 land 0x80 <> 0 in
    let len7 = b1 land 0x7F in
    let ext_len_bytes = if len7 = 126 then 2 else if len7 = 127 then 8 else 0 in
    let header_len = 2 + ext_len_bytes + (if masked then 4 else 0) in
    if len < header_len then Incomplete
    else begin
      match Opcode.of_int opcode_int with
      | Error n -> Protocol_error (Printf.sprintf "reserved opcode 0x%x" n)
      | Ok opcode ->
        let payload_len =
          if len7 < 126 then len7
          else if len7 = 126 then
            (Char.code (Bigstringaf.get bs (off + 2)) lsl 8)
            lor Char.code (Bigstringaf.get bs (off + 3))
          else begin
            let v = Bigstringaf.get_int64_be bs (off + 2) in
            if Int64.compare v 0L < 0
               || Int64.compare v (Int64.of_int max_int) > 0
            then -1
            else Int64.to_int v
          end
        in
        if payload_len < 0 then
          Protocol_error "payload length exceeds addressable range"
        else if payload_len > max_payload then
          Protocol_error
            (Printf.sprintf "payload length %d exceeds max %d" payload_len
               max_payload)
        else if Opcode.is_control opcode && (payload_len > 125 || not fin) then
          Protocol_error
            "control frame must have payload <= 125 bytes and FIN set"
        else begin
          let mask_off = off + 2 + ext_len_bytes in
          let payload_off = mask_off + if masked then 4 else 0 in
          let total = payload_off - off + payload_len in
          if len < total then Incomplete
          else begin
            let payload = Bigstringaf.create payload_len in
            (if masked then begin
               let key = Bigstringaf.substring bs ~off:mask_off ~len:4 in
               mask_into ~key ~src:bs ~src_off:payload_off ~dst:payload
                 ~dst_off:0 ~len:payload_len
             end
             else
               Bigstringaf.blit bs ~src_off:payload_off payload ~dst_off:0
                 ~len:payload_len);
            let frame = { fin; rsv1; rsv2; rsv3; opcode; payload } in
            Frame ({ frame; masked }, total)
          end
        end
    end
  end
