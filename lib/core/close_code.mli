(** RFC 6455 §7.4 WebSocket close status codes.

    A value of type {!t} is always safe to put on the wire. The codes reserved
    for internal use (1005 "No Status Rcvd", 1006 "Abnormal Closure", 1015 "TLS
    handshake") and every other non-sendable value cannot be constructed, so
    "send a reserved close code" — which §7.4.1 forbids — is unrepresentable. *)

type t

(** 1000 Normal Closure. *)
val normal : t

(** 1002 Protocol error. *)
val protocol_error : t

(** 1007 Invalid frame payload data (e.g. a Text frame that is not UTF-8). *)
val invalid_payload : t

(** 1009 Message Too Big. *)
val message_too_big : t

(** [of_wire n] validates a close code received in a peer's Close frame.

    [Error] for any value an endpoint must not have sent on the wire: 0-999, the
    reserved 1004/1005/1006/1015, the unassigned 1012-2999, and >=5000. The
    accepted set is the registered protocol codes (1000-1003, 1007-1011) and the
    library/application range 3000-4999. *)
val of_wire : int -> (t, string) result

val to_int : t -> int
