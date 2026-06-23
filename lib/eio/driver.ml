module Endpoint = Ws_direct_core.Endpoint

(* A growable byte accumulator holding inbound bytes the endpoint has not yet
   consumed (an incomplete trailing frame is kept and re-presented next read). *)
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

(* Bound the opening-handshake head so a peer that never sends CRLFCRLF cannot
   grow the buffer without limit (a slowloris-style header flood). 16 KiB is far
   above any legitimate WebSocket upgrade head. *)
let max_handshake_head_bytes = 16 * 1024

(* The byte cap alone limits memory, not lifetime: a peer that dribbles bytes
   below the terminator (the classic slowloris) holds a fiber + fd until it
   reaches [max_handshake_head_bytes], which at one byte per second is over four
   hours. A wall-clock deadline caps the handshake regardless of byte rate
   (RFC 6455 §10). *)
let default_handshake_timeout = 10.0

let read_head ?(timeout = default_handshake_timeout) ~clock flow =
  let read_until_crlfcrlf () =
    let b = Buffer.create 256 in
    let one = Cstruct.create 1 in
    let rec loop () =
      if Buffer.length b > max_handshake_head_bytes then
        failwith
          (Printf.sprintf "handshake head exceeded %d bytes without CRLFCRLF"
             max_handshake_head_bytes);
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
  in
  try Eio.Time.with_timeout_exn clock timeout read_until_crlfcrlf
  with Eio.Time.Timeout ->
    failwith
      (Printf.sprintf "handshake head not completed within %.1fs" timeout)

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
      match
        Eio.Fiber.both
          (fun () -> writer flow endpoint)
          (fun () -> reader flow endpoint)
      with
      | () ->
        (* Clean exit (peer EOF or Close already routed a terminal handler);
           guarantee one for the writer-closed-first case too. *)
        Endpoint.ensure_terminal_eof endpoint
      | exception (Eio.Cancel.Cancelled _ as e) ->
        (* Cancellation MUST propagate (Eio), but deliver a terminal first so a
           blocking consumer unblocks before the switch tears down. *)
        Endpoint.ensure_terminal_eof endpoint;
        raise e
      | exception e ->
        (* Fault isolation: a per-connection transport error (e.g. a
           mid-session TLS [Tls_failure]/[Tls_alert]) must NOT escape to fail
           the parent switch — that would take down every sibling fiber (the
           whole gateway). Deliver it as [on_error] and exit cleanly so the
           caller reconnects via its own policy. *)
        Endpoint.notify_error endpoint (Printexc.to_string e))
