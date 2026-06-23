(** RFC 6455 WebSocket frame codec.

    Runtime-agnostic: depends only on [bigstringaf] and [faraday]. A frame is
    the logical unit (FIN, RSV bits, opcode, unmasked payload); masking is a
    wire concern handled at (de)serialization, not stored on the frame. *)

module Opcode : sig
  type t =
    | Continuation
    | Text
    | Binary
    | Close
    | Ping
    | Pong

  val to_int : t -> int

  (** [of_int n] is [Ok op] for the six defined opcodes, or [Error n] for any
      reserved code (0x3-0x7 non-control, 0xB-0xF control). Reserved opcodes
      are a protocol error per RFC 6455 §5.2. *)
  val of_int : int -> (t, int) result

  val is_control : t -> bool
end

type t =
  { fin : bool
  ; rsv1 : bool
  ; rsv2 : bool
  ; rsv3 : bool
  ; opcode : Opcode.t
  ; payload : Bigstringaf.t  (** unmasked application payload *)
  }

val create :
   ?fin:bool
  -> ?rsv1:bool
  -> ?rsv2:bool
  -> ?rsv3:bool
  -> Opcode.t
  -> Bigstringaf.t
  -> t

(** Build a frame from a [string] payload (copies into a bigstring). *)
val of_string :
   ?fin:bool
  -> ?rsv1:bool
  -> ?rsv2:bool
  -> ?rsv3:bool
  -> Opcode.t
  -> string
  -> t

val payload_string : t -> string

(** [serialize ?mask t f] writes the wire encoding of [t] to faraday [f].

    If [mask] is given (it must be exactly 4 bytes) the MASK bit is set and the
    payload is masked with it. Per RFC 6455, client→server frames MUST be
    masked and server→client frames MUST NOT be; that direction policy is
    enforced by the connection layer, not here.

    @raise Invalid_argument if [mask] is not 4 bytes. *)
val serialize : ?mask:string -> t -> Faraday.t -> unit

(** Convenience wrapper over {!serialize} returning a fresh string. *)
val to_string : ?mask:string -> t -> string

type parsed =
  { frame : t
  ; masked : bool  (** whether the wire frame had the MASK bit set *)
  }

type parse =
  | Frame of parsed * int
      (** a complete frame and the total number of bytes it consumed *)
  | Incomplete  (** not enough bytes buffered yet; retry after more arrive *)
  | Protocol_error of string  (** a malformed frame; the connection must fail *)

(** Default DoS guard for a single frame's payload (64 MiB). *)
val default_max_payload : int

(** [parse ?max_payload bs ~off ~len] attempts to decode one frame from
    [bs.\[off .. off+len-1\]].

    - {!Incomplete} when fewer than the full frame's bytes are present.
    - {!Protocol_error} for reserved opcodes, control frames that are
      fragmented or exceed 125 bytes, or payloads exceeding [max_payload] /
      [max_int].
    - {!Frame} with the payload already unmasked. *)
val parse : ?max_payload:int -> Bigstringaf.t -> off:int -> len:int -> parse
