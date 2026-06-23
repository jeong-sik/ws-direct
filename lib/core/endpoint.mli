(** A driven WebSocket endpoint.

    Wraps a {!Connection} (inbound assembly) with an outbound {!Faraday} buffer
    and the mandatory automatic control-frame responses (Pong for every Ping,
    Close echo on Close, Close 1002 on protocol error). Outgoing frames are
    masked iff the role is [Client] (RFC 6455 §5.3).

    The read/write operation functions intentionally match [Gluten.RUNTIME]
    byte for byte, so the [ws-direct-gluten] server adapter is a zero-logic
    re-export and the [ws-direct-eio] driver runs the same endpoint over an
    [Eio.Flow.two_way]. *)

type role =
  | Server
  | Client

module Wsd : sig
  (** Write descriptor handed to the handler builder; the channel for sending. *)
  type t

  val send_text : t -> string -> unit
  val send_binary : t -> string -> unit
  val send_ping : t -> ?payload:string -> unit -> unit
  val send_pong : t -> ?payload:string -> unit -> unit

  (** Send a Close frame. After this the descriptor refuses further sends. *)
  val send_close : t -> ?code:int -> ?reason:string -> unit -> unit

  val is_closed : t -> bool
end

type handlers

(** Build a handler set; every callback defaults to a no-op. [on_ping]/[on_pong]
    are observational — the endpoint already auto-replies to pings. *)
val handlers :
   ?on_message:(Connection.Message.t -> unit)
  -> ?on_ping:(Bigstringaf.t -> unit)
  -> ?on_pong:(Bigstringaf.t -> unit)
  -> ?on_close:(code:int option -> reason:string -> unit)
  -> ?on_error:(string -> unit)
  -> ?on_eof:(unit -> unit)
  -> unit
  -> handlers

type t

(** [create role ?max_message ?max_frame ?random builder] builds an endpoint.
    [builder] receives the {!Wsd.t} and returns the inbound handlers.
    [max_message] caps a complete message (single-frame or reassembled) and
    [max_frame] caps one frame's payload — both default to 64 MiB. [random]
    yields a fresh 4-byte masking key per client frame; the default is
    non-cryptographic and the [Client] eio driver must inject a CSPRNG. *)
val create :
   role
  -> ?max_message:int
  -> ?max_frame:int
  -> ?random:(unit -> string)
  -> (Wsd.t -> handlers)
  -> t

val wsd : t -> Wsd.t

(** Deliver a fallback terminal callback so a driver can guarantee that exactly
    one of [on_close] / [on_error] / [on_eof] fires per connection — required
    by callback->blocking bridges that would otherwise hang on an abnormal exit
    (TLS error, cancellation). Both are idempotent: they fire only if no
    terminal handler has run yet, and never after a clean Close / Fail / eof. *)
val ensure_terminal_eof : t -> unit

val notify_error : t -> string -> unit

(* --- gluten RUNTIME-shaped operations ---------------------------------- *)

val next_read_operation : t -> [ `Read | `Yield | `Close ]
val read : t -> Bigstringaf.t -> off:int -> len:int -> int
val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
val yield_reader : t -> (unit -> unit) -> unit

val next_write_operation :
  t -> [ `Write of Bigstringaf.t Faraday.iovec list | `Yield | `Close of int ]

val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
val yield_writer : t -> (unit -> unit) -> unit
val report_exn : t -> exn -> unit
val is_closed : t -> bool
val shutdown : t -> unit
