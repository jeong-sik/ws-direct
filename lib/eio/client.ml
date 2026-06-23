module Endpoint = Ws_direct_core.Endpoint

let default_random n = Mirage_crypto_rng.generate n

let connect ~sw ?(random = default_random)
    ?(max_message = Ws_direct_core.Frame.default_max_payload) ?handshake_timeout
    ~clock ~host ~resource flow builder =
  let key = Handshake.make_key random in
  Eio.Flow.copy_string (Handshake.request ~host ~resource ~key) flow;
  let head = Driver.read_head ?timeout:handshake_timeout ~clock flow in
  match Handshake.check_response ~key head with
  | Error msg -> failwith ("ws-direct handshake failed: " ^ msg)
  | Ok () ->
    let endpoint =
      Endpoint.create Endpoint.Client ~max_message
        ~random:(fun () -> random 4)
        builder
    in
    Eio.Fiber.fork ~sw (fun () -> Driver.drive flow endpoint);
    Endpoint.wsd endpoint
