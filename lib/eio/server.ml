module Endpoint = Ws_direct_core.Endpoint

let handle ?(max_message = Ws_direct_core.Frame.default_max_payload) flow builder =
  let head = Driver.read_head flow in
  match Handshake.request_key head with
  | Error msg -> failwith ("ws-direct server handshake failed: " ^ msg)
  | Ok key ->
    Eio.Flow.copy_string (Handshake.server_response ~key) flow;
    let endpoint = Endpoint.create Endpoint.Server ~max_message builder in
    Driver.drive flow endpoint
