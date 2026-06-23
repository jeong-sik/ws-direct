(** Inbound WebSocket message assembly (RFC 6455 §5.4).

    Sits above {!Frame}: it drains every buffered frame in one pass, reassembles
    fragmented data messages, enforces the masking direction for the role, and
    surfaces control frames as events. This is the layer where the coalesced-
    frame stall that affects a one-frame-per-read drainer is structurally
    impossible: {!read_bytes} loops until the buffer holds no more complete
    frames. *)

module Message : sig
  type kind =
    | Text
    | Binary

  type t =
    { kind : kind
    ; payload : Bigstringaf.t  (** full, reassembled application payload *)
    }
end

type role =
  | Server  (** peer is a client: inbound frames MUST be masked *)
  | Client  (** peer is a server: inbound frames MUST NOT be masked *)

type event =
  | Message of Message.t  (** a complete, reassembled data message *)
  | Ping of Bigstringaf.t
  | Pong of Bigstringaf.t
  | Close of
      { code : int option
      ; reason : string
      }  (** a valid Close frame from the peer (code already validated) *)
  | Fail of
      { code : Close_code.t
      ; reason : string
      }
      (** the connection must be failed (RFC 6455 §7.1.7): the peer violated the
          protocol ([Close_code.protocol_error], 1002) or sent invalid payload
          data such as non-UTF-8 text ([Close_code.invalid_payload], 1007). The
          driver should send a Close with this code and then shut down. *)

type t

(** [create ?max_message ?max_frame role] is a fresh assembler. [max_message]
    (default 64 MiB) bounds a complete message — single-frame or reassembled.
    [max_frame] (default 64 MiB) bounds one frame's payload, rejecting an
    oversized frame before it is buffered. *)
val create : ?max_message:int -> ?max_frame:int -> role -> t

(** Feed one already-parsed frame. Returns [None] while a fragmented message is
    still in progress, [Some event] otherwise. Enforces masking direction,
    fragmentation ordering, and rejects set RSV bits (no extensions). *)
val handle_frame : t -> Frame.parsed -> event option

(** [read_bytes t bs ~off ~len] parses and drains {e every} complete frame in
    [bs.\[off .. off+len-1\]] in a single call, returning the resulting events
    in order and the number of bytes consumed. An incomplete trailing frame is
    left unconsumed (its bytes are reported as not consumed) so the caller can
    re-present them once more data arrives. Parsing stops at the first
    {!Fail}, which is included as the final event. *)
val read_bytes : t -> Bigstringaf.t -> off:int -> len:int -> event list * int
