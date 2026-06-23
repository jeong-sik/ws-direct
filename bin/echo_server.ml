(* A WebSocket echo server: every data message is sent straight back with the
   same opcode. Used as the target for the Autobahn fuzzingclient conformance
   run. Ping/pong and close are handled by the core Endpoint. *)

module Endpoint = Ws_direct_core.Endpoint
module Message = Ws_direct_core.Connection.Message

let port = 9001

let echo_handlers wsd =
  Endpoint.handlers
    ~on_message:(fun (m : Message.t) ->
      let s = Bigstringaf.to_string m.Message.payload in
      match m.Message.kind with
      | Message.Text -> Endpoint.Wsd.send_text wsd s
      | Message.Binary -> Endpoint.Wsd.send_binary wsd s)
    ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let sock = Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net addr in
  Printf.printf "ws-direct echo server listening on 0.0.0.0:%d\n%!" port;
  let handle flow _addr =
    try Ws_direct_eio.Server.handle ~clock flow echo_handlers with
    | Failure _ -> () (* invalid handshake: drop the connection *)
  in
  while true do
    Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) handle
  done
