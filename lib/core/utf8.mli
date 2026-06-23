(** Incremental UTF-8 validation (RFC 3629).

    Designed to validate a payload that arrives in chunks: a WebSocket text
    message may be fragmented (§5.4) with a multi-byte code point split across
    continuation frames, so the bytes must be validated as one stream rather than
    per frame. *)

type t

val create : unit -> t

(** [feed t bs ~off ~len] consumes bytes, updating the decoder state, and returns
    whether the stream is still valid. It returns [false] as soon as the bytes
    cannot be valid UTF-8 — a stray or missing continuation byte, an overlong
    encoding, a surrogate (U+D800..U+DFFF), or a code point above U+10FFFF. Once
    invalid, the validator stays invalid. *)
val feed : t -> Bigstringaf.t -> off:int -> len:int -> bool

(** [feed_string] is {!feed} over an OCaml string. *)
val feed_string : t -> string -> bool

(** [is_complete t] is true when the stream is valid {e and} ends on a code-point
    boundary. A stream whose bytes are each individually acceptable but that ends
    mid-sequence (a truncated multi-byte code point) is not valid UTF-8. *)
val is_complete : t -> bool

(** [valid_string s] is [true] iff [s] is wholly valid UTF-8. *)
val valid_string : string -> bool

(** [valid_bigstring bs ~off ~len] is [true] iff that slice is wholly valid
    UTF-8. *)
val valid_bigstring : Bigstringaf.t -> off:int -> len:int -> bool
