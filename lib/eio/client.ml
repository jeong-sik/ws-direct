module Endpoint = Ws_direct_core.Endpoint

let default_random n = Mirage_crypto_rng.generate n

(* A growable byte accumulator holding inbound bytes that the endpoint has not
   yet consumed (an incomplete trailing frame is kept and re-presented with the
   next read). *)
module Acc = struct
  type t =
    { mutable buf : Bigstringaf.t
    ; mutable len : int
    }

  let create n = { buf = Bigstringaf.create n; len = 0 }

  let ensure t extra =
    let need = t.len + extra in
    let cap = Bigstringaf.length t.buf in
    if need > cap then begin
      let ncap = ref (max cap 1) in
      while !ncap < need do
        ncap := !ncap * 2
      done;
      let nb = Bigstringaf.create !ncap in
      Bigstringaf.blit t.buf ~src_off:0 nb ~dst_off:0 ~len:t.len;
      t.buf <- nb
    end

  let add t (cs : Cstruct.t) n =
    ensure t n;
    Bigstringaf.blit cs.Cstruct.buffer ~src_off:cs.Cstruct.off t.buf
      ~dst_off:t.len ~len:n;
    t.len <- t.len + n

  let drop t consumed =
    if consumed > 0 then begin
      let rem = t.len - consumed in
      if rem > 0 then
        Bigstringaf.blit t.buf ~src_off:consumed t.buf ~dst_off:0 ~len:rem;
      t.len <- rem
    end
end

(* Read the HTTP response head one byte at a time up to and including the
   terminating CRLFCRLF, so no WebSocket bytes are consumed past the boundary
   (any frames the server sends immediately stay in the socket for the reader
   fiber). The head is small and read once, so byte-at-a-time is acceptable. *)
let read_head flow =
  let b = Buffer.create 256 in
  let one = Cstruct.create 1 in
  let rec loop () =
    let n = Eio.Flow.single_read flow one in
    if n = 0 then loop ()
    else begin
      Buffer.add_char b (Cstruct.get_char one 0);
      let len = Buffer.length b in
      if
        len >= 4
        &&
        let s = Buffer.contents b in
        String.sub s (len - 4) 4 = "\r\n\r\n"
      then Buffer.contents b
      else loop ()
    end
  in
  loop ()

let iovec_cstruct (iov : Bigstringaf.t Faraday.iovec) =
  Cstruct.of_bigarray iov.Faraday.buffer ~off:iov.Faraday.off ~len:iov.Faraday.len

let writer flow endpoint =
  let rec loop () =
    match Endpoint.next_write_operation endpoint with
    | `Write iovecs ->
      let total =
        List.fold_left (fun a (iov : _ Faraday.iovec) -> a + iov.Faraday.len) 0
          iovecs
      in
      (match Eio.Flow.write flow (List.map iovec_cstruct iovecs) with
      | () -> Endpoint.report_write_result endpoint (`Ok total)
      | exception (End_of_file | Eio.Io _) ->
        Endpoint.report_write_result endpoint `Closed);
      loop ()
    | `Yield ->
      let p, r = Eio.Promise.create () in
      Endpoint.yield_writer endpoint (Eio.Promise.resolve r);
      Eio.Promise.await p;
      loop ()
    | `Close _ -> ()
  in
  loop ()

let reader flow endpoint =
  let acc = Acc.create 4096 in
  let chunk = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow chunk with
    | n ->
      Acc.add acc chunk n;
      let consumed = Endpoint.read endpoint acc.Acc.buf ~off:0 ~len:acc.Acc.len in
      Acc.drop acc consumed;
      if Endpoint.is_closed endpoint then () else loop ()
    | exception End_of_file ->
      ignore (Endpoint.read_eof endpoint acc.Acc.buf ~off:0 ~len:acc.Acc.len)
  in
  loop ()

let drive flow endpoint =
  Fun.protect
    ~finally:(fun () -> Endpoint.shutdown endpoint)
    (fun () ->
      try Eio.Fiber.both (fun () -> writer flow endpoint) (fun () -> reader flow endpoint)
      with End_of_file | Eio.Io _ -> ())

let connect ~sw ?(random = default_random)
    ?(max_message = Ws_direct_core.Frame.default_max_payload) ~host ~resource
    flow builder =
  let key = Handshake.make_key random in
  Eio.Flow.copy_string (Handshake.request ~host ~resource ~key) flow;
  let head = read_head flow in
  match Handshake.check_response ~key head with
  | Error msg -> failwith ("ws-direct handshake failed: " ^ msg)
  | Ok () ->
    let endpoint =
      Endpoint.create Endpoint.Client ~max_message
        ~random:(fun () -> random 4)
        builder
    in
    Eio.Fiber.fork ~sw (fun () -> drive flow endpoint);
    Endpoint.wsd endpoint
