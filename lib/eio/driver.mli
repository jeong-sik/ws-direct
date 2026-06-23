(** Shared transport loop driving a core {!Ws_direct_core.Endpoint} over an Eio
    flow. Used by both the client and server entry points; not the public API. *)

(** [read_head flow] reads the HTTP handshake head one byte at a time, up to and
    including the terminating CRLFCRLF, leaving any bytes that follow in the
    flow (they belong to the WebSocket stream). *)
val read_head : _ Eio.Flow.source -> string

(** [drive flow endpoint] runs a reader and a writer fiber until the connection
    closes, then shuts the endpoint down. The reader retains an incomplete
    trailing frame across reads; the writer bridges the endpoint's faraday
    yield protocol to an {!Eio.Promise}. *)
val drive : _ Eio.Flow.two_way -> Ws_direct_core.Endpoint.t -> unit
