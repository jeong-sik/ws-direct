(** WebSocket server driver over an {!Eio.Flow.two_way}.

    Reads and validates the RFC 6455 §4 client handshake, replies 101, then
    drives a {!Ws_direct_core.Endpoint} (role
    {!Ws_direct_core.Endpoint.Server}) over the flow. Like the client, it speaks
    a plain [two_way], so a TLS-terminated connection works unchanged. *)

(** [handle ?max_message ?handshake_timeout ~clock flow builder] performs the
    server handshake on [flow] and, on success, drives the connection to
    completion (this call returns when the connection closes). [builder] receives
    the writer and returns the inbound handlers, as for
    {!Ws_direct_core.Endpoint.create}. A server never masks its frames (§5.3).

    The opening handshake runs under a [handshake_timeout]-second deadline on
    [clock] (see {!Driver.read_head}) so a client that never completes the
    upgrade cannot hold the connection indefinitely.

    @raise Failure
      if the request is not a valid WebSocket upgrade, or the handshake deadline
      elapses. *)
val handle :
   ?max_message:int
  -> ?handshake_timeout:float
  -> clock:_ Eio.Time.clock
  -> _ Eio.Flow.two_way
  -> (Ws_direct_core.Endpoint.Wsd.t -> Ws_direct_core.Endpoint.handlers)
  -> unit
