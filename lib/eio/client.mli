(** WebSocket client driver over an {!Eio.Flow.two_way}.

    Performs the RFC 6455 §4 opening handshake, then drives a
    {!Ws_direct_core.Endpoint} (role {!Ws_direct_core.Endpoint.Client}) over the
    flow with a reader and a writer fiber. Because it speaks to a plain
    [two_way], it composes with a TLS flow ([Tls_eio.t]) for [wss://] without
    any change here. *)

(** [connect ~sw ?random ?max_message ~host ~resource flow builder] runs the
    handshake on [flow] and, on success, forks the read/write driver into [sw]
    and returns the connection's writer.

    [builder] receives that writer and returns the inbound handlers, exactly as
    for {!Ws_direct_core.Endpoint.create}. Outbound frames are masked with 4
    bytes from [random] (§5.3 requires an unpredictable key); [random] defaults
    to {!Mirage_crypto_rng.generate}, so the caller must have seeded the RNG
    (e.g. via [Mirage_crypto_rng_unix] or [Mirage_crypto_rng_eio.run]).

    [host] and [resource] populate the request line and [Host] header.

    @raise Failure if the server's response is not an acceptable 101 upgrade. *)
val connect :
   sw:Eio.Switch.t
  -> ?random:(int -> string)
  -> ?max_message:int
  -> host:string
  -> resource:string
  -> _ Eio.Flow.two_way
  -> (Ws_direct_core.Endpoint.Wsd.t -> Ws_direct_core.Endpoint.handlers)
  -> Ws_direct_core.Endpoint.Wsd.t
