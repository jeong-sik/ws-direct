type role =
  | Server
  | Client

let noop () = ()
let default_random () = String.init 4 (fun _ -> Char.chr (Random.int 256))

module Wsd = struct
  type t =
    { writer : Faraday.t
    ; role : role
    ; random : unit -> string
    ; mutable closed : bool
    ; mutable on_write : unit -> unit
    }

  let create ~role ~random =
    { writer = Faraday.create 0x1000
    ; role
    ; random
    ; closed = false
    ; on_write = noop
    }

  let mask_for w = match w.role with Client -> Some (w.random ()) | Server -> None

  let send_frame_bs w ?(fin = true) opcode payload =
    if not w.closed then begin
      Frame.serialize ?mask:(mask_for w) (Frame.create ~fin opcode payload)
        w.writer;
      w.on_write ()
    end

  let send_string w ?fin opcode s =
    send_frame_bs w ?fin opcode
      (Bigstringaf.of_string s ~off:0 ~len:(String.length s))

  let send_text w s = send_string w Frame.Opcode.Text s
  let send_binary w s = send_string w Frame.Opcode.Binary s
  let send_ping w ?(payload = "") () = send_string w Frame.Opcode.Ping payload
  let send_pong w ?(payload = "") () = send_string w Frame.Opcode.Pong payload
  let send_pong_bs w bs = send_frame_bs w Frame.Opcode.Pong bs

  let close_payload code reason =
    match code with
    | None -> ""
    | Some c ->
      let b = Bytes.create (2 + String.length reason) in
      Bytes.set b 0 (Char.chr ((c lsr 8) land 0xff));
      Bytes.set b 1 (Char.chr (c land 0xff));
      Bytes.blit_string reason 0 b 2 (String.length reason);
      Bytes.unsafe_to_string b

  let send_close w ?code ?(reason = "") () =
    if not w.closed then begin
      send_string w Frame.Opcode.Close (close_payload code reason);
      w.closed <- true
    end

  let is_closed w = w.closed || Faraday.is_closed w.writer
end

type handlers =
  { on_message : Connection.Message.t -> unit
  ; on_ping : Bigstringaf.t -> unit
  ; on_pong : Bigstringaf.t -> unit
  ; on_close : code:int option -> reason:string -> unit
  ; on_error : string -> unit
  ; on_eof : unit -> unit
  }

let handlers ?(on_message = fun _ -> ()) ?(on_ping = fun _ -> ())
    ?(on_pong = fun _ -> ()) ?(on_close = fun ~code:_ ~reason:_ -> ())
    ?(on_error = fun _ -> ()) ?(on_eof = fun () -> ()) () =
  { on_message; on_ping; on_pong; on_close; on_error; on_eof }

let noop_handlers = handlers ()

type t =
  { wsd : Wsd.t
  ; inbound : Connection.t
  ; mutable handlers : handlers
  ; mutable closed : bool
  ; (* Set once any of on_close / on_error / on_eof has fired. A driver must
       deliver exactly one terminal handler per connection so a blocking
       consumer (e.g. a callback->stream bridge) always unblocks; this guard
       lets [ensure_terminal_eof] / [notify_error] fire a fallback terminal on
       an abnormal exit (TLS error, cancellation) without double-firing after a
       clean Close/Fail/eof. *)
    mutable terminal_delivered : bool
  ; mutable wakeup_writer : unit -> unit
  ; mutable wakeup_reader : unit -> unit
  }

let conn_role = function
  | Server -> Connection.Server
  | Client -> Connection.Client

let create role ?(max_message = Frame.default_max_payload)
    ?(max_frame = Frame.default_max_payload) ?(random = default_random) builder =
  let wsd = Wsd.create ~role ~random in
  let t =
    { wsd
    ; inbound = Connection.create ~max_message ~max_frame (conn_role role)
    ; handlers = noop_handlers
    ; closed = false
    ; terminal_delivered = false
    ; wakeup_writer = noop
    ; wakeup_reader = noop
    }
  in
  wsd.Wsd.on_write <-
    (fun () ->
      let k = t.wakeup_writer in
      t.wakeup_writer <- noop;
      k ());
  t.handlers <- builder wsd;
  t

let wsd t = t.wsd

let shutdown t =
  if not t.closed then begin
    t.closed <- true;
    t.wsd.Wsd.closed <- true;
    if not (Faraday.is_closed t.wsd.Wsd.writer) then
      Faraday.close t.wsd.Wsd.writer;
    let kw = t.wakeup_writer in
    t.wakeup_writer <- noop;
    kw ();
    let kr = t.wakeup_reader in
    t.wakeup_reader <- noop;
    kr ()
  end

(* Deliver a fallback terminal handler on an abnormal exit (no Close/Fail/eof
   was observed), e.g. a TLS error or cancellation in the driver. Idempotent
   via [terminal_delivered] so it never double-fires after a clean terminal. *)
let ensure_terminal_eof t =
  if not t.terminal_delivered then begin
    t.terminal_delivered <- true;
    t.handlers.on_eof ()
  end

let notify_error t msg =
  if not t.terminal_delivered then begin
    t.terminal_delivered <- true;
    t.handlers.on_error msg
  end

let handle_event t = function
  | Connection.Message m -> t.handlers.on_message m
  | Connection.Ping p ->
    Wsd.send_pong_bs t.wsd p;
    t.handlers.on_ping p
  | Connection.Pong p -> t.handlers.on_pong p
  | Connection.Close { code; reason } ->
    t.handlers.on_close ~code ~reason;
    t.terminal_delivered <- true;
    Wsd.send_close t.wsd ?code ();
    shutdown t
  | Connection.Fail { code; reason } ->
    t.handlers.on_error reason;
    t.terminal_delivered <- true;
    Wsd.send_close t.wsd ~code:(Close_code.to_int code) ~reason ();
    shutdown t

let process t bs ~off ~len =
  let events, consumed = Connection.read_bytes t.inbound bs ~off ~len in
  List.iter (handle_event t) events;
  consumed

let read t bs ~off ~len = process t bs ~off ~len

let read_eof t bs ~off ~len =
  let n = process t bs ~off ~len in
  t.handlers.on_eof ();
  t.terminal_delivered <- true;
  shutdown t;
  n

let next_read_operation t = if t.closed then `Close else `Read
let yield_reader t k = t.wakeup_reader <- k

let next_write_operation t =
  match Faraday.operation t.wsd.Wsd.writer with
  | `Writev iovecs -> `Write iovecs
  | `Close -> `Close 0
  | `Yield -> `Yield

let report_write_result t = function
  | `Ok n -> Faraday.shift t.wsd.Wsd.writer n
  | `Closed -> shutdown t

let yield_writer t k =
  if Faraday.is_closed t.wsd.Wsd.writer then k () else t.wakeup_writer <- k

let report_exn t exn =
  notify_error t (Printexc.to_string exn);
  shutdown t
let is_closed t = t.closed
