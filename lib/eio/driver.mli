(** Shared transport loop driving a core {!Ws_direct_core.Endpoint} over an Eio
    flow. Used by both the client and server entry points; not the public API. *)

(** [read_head ?timeout ~clock flow] reads the HTTP handshake head one byte at a
    time, up to and including the terminating CRLFCRLF, leaving any bytes that
    follow in the flow (they belong to the WebSocket stream). The read is bound
    both in size ([max_handshake_head_bytes]) and in time: it runs under a
    [timeout]-second deadline on [clock] (default {!default_handshake_timeout})
    so a peer that never completes the head cannot hold the connection
    indefinitely (slowloss, RFC 6455 §10).

    @raise Failure if the head exceeds the byte cap, or the deadline elapses. *)
val read_head :
   ?timeout:float
  -> clock:_ Eio.Time.clock
  -> _ Eio.Flow.source
  -> string

(** Default handshake-head deadline in seconds, used by {!read_head} when no
    [timeout] is given. *)
val default_handshake_timeout : float

(** [drive flow endpoint] runs a reader and a writer fiber until the connection
    closes, then shuts the endpoint down. The reader retains an incomplete
    trailing frame across reads; the writer bridges the endpoint's faraday
    yield protocol to an {!Eio.Promise}. *)
val drive : _ Eio.Flow.two_way -> Ws_direct_core.Endpoint.t -> unit
