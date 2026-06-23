(** RFC 6455 §4 opening-handshake encoding and validation (pure: no I/O).

    The handshake is the security-critical boundary of a client driver, so it is
    a separate, fully testable module: it builds the upgrade request and decides
    whether a server's response is an acceptable Switching-Protocols reply, with
    no dependency on Eio or any transport. *)

(** [accept_token key] is the [Sec-WebSocket-Accept] value a server returns for
    the client's [Sec-WebSocket-Key]: base64(SHA-1(key ^ GUID)) where GUID is
    the §1.3 magic string. *)
val accept_token : string -> string

(** [make_key random] is a fresh [Sec-WebSocket-Key]: base64 of 16 bytes drawn
    from [random] (§4.1 requires a freshly chosen, unpredictable nonce). *)
val make_key : (int -> string) -> string

(** [request ~host ~resource ~key] is the byte string of the client's GET
    upgrade request, terminated by a blank line. *)
val request : host:string -> resource:string -> key:string -> string

(** [check_response ~key head] validates the server's response head (the bytes
    up to and including the terminating blank line). It returns [Ok ()] only for
    a 101 status with [Upgrade: websocket], a [Connection] header listing
    [upgrade], and a [Sec-WebSocket-Accept] equal to [accept_token key]. *)
val check_response : key:string -> string -> (unit, string) result

(** [request_key head] validates a client's upgrade request head and returns its
    [Sec-WebSocket-Key]. [Error] unless it is a GET with [Upgrade: websocket], a
    [Connection] header listing [upgrade], [Sec-WebSocket-Version: 13], and a
    [Sec-WebSocket-Key]. *)
val request_key : string -> (string, string) result

(** [server_response ~key] is the 101 Switching Protocols response a server
    sends for a client whose [Sec-WebSocket-Key] is [key]. *)
val server_response : key:string -> string
