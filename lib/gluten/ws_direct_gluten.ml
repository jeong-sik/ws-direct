(** Server-side gluten adapter.

    {!Ws_direct_core.Endpoint} already exposes exactly the operations gluten's
    [RUNTIME] signature requires; this module asserts that match (the coercion
    below fails to compile if the signatures ever drift) and packages an
    endpoint as a [Gluten.impl] for an HTTP server's upgrade callback.

    Drop-in for [Httpun_ws.Server_connection]: an httpun upgrade handler can do
    [upgrade (Ws_direct_gluten.impl endpoint)] where it previously did
    [upgrade (Gluten.make (module Httpun_ws.Server_connection) ws_conn)]. *)

module Endpoint = Ws_direct_core.Endpoint

module Server_connection :
  Gluten.RUNTIME with type t = Endpoint.t = struct
  type t = Endpoint.t

  let next_read_operation = Endpoint.next_read_operation
  let read = Endpoint.read
  let read_eof = Endpoint.read_eof
  let yield_reader = Endpoint.yield_reader
  let next_write_operation = Endpoint.next_write_operation
  let report_write_result = Endpoint.report_write_result
  let yield_writer = Endpoint.yield_writer
  let report_exn = Endpoint.report_exn
  let is_closed = Endpoint.is_closed
  let shutdown = Endpoint.shutdown
end

(** Package a driven endpoint as a [Gluten.impl] ready to hand to an HTTP
    server's upgrade callback. *)
let impl (endpoint : Endpoint.t) : Gluten.impl =
  Gluten.make (module Server_connection) endpoint
