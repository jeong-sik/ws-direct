(* Regression: the writer fiber must STOP when the transport is dead, not
   busy-spin.

   A dead transport makes [Eio.Flow.write] raise on every attempt.
   [report_write_result `Closed] closes the faraday serializer, but
   [Faraday.close] cannot drop the bytes already buffered for the failed write,
   so [next_write_operation] keeps returning [`Write] for the SAME bytes
   (faraday flushes pending output before it will report [`Close]). If the
   writer re-loops it retries the dead transport forever at 100% CPU — observed
   live (main_eio.exe pid 26053, ~21h uptime: a [sample] showed
   next_write_operation -> Eio.Flow.write -> report_write_result plus
   caml_raise_exn dominating the main thread, with no thread parked in a wait).

   Failure is made deterministic with a mock sink that always raises: real
   sockets buffer in the kernel, so the first writes may succeed and the spin
   only starts later, which is racy. The spin also cannot be caught with a
   timeout — a busy loop never yields, so a timer fiber on the same domain would
   never be scheduled — so the test asserts on the WRITE-ATTEMPT COUNT. A
   self-guard caps attempts and raises, so a regressed (spinning) build fails
   fast instead of hanging the test binary. *)

module E = Ws_direct_core.Endpoint
module D = Ws_direct_eio.Driver

let spin_guard = 1000

(* A two-way flow whose every write fails (transport dead) and whose reads are
   at EOF. [attempts] counts write attempts, letting the test distinguish "one
   write, then stop" (fixed) from "writes forever" (spin). [End_of_file] is one
   of the exceptions the driver routes to [report_write_result `Closed], so it
   reproduces the [`Closed] path without constructing an [Eio.Io] value. *)
let dead_flow () =
  let attempts = ref 0 in
  let module M = struct
    type t = int ref

    let read_methods = []
    let single_read (_ : t) (_ : Cstruct.t) : int = raise End_of_file

    let single_write (a : t) (_ : Cstruct.t list) : int =
      incr a;
      if !a > spin_guard then
        failwith
          (Printf.sprintf
             "ws-direct writer spun: >%d write attempts on a dead transport"
             spin_guard);
      raise End_of_file

    let copy (a : t) ~src = Eio.Flow.Pi.simple_copy ~single_write a ~src
    let shutdown (_ : t) (_ : Eio.Flow.shutdown_command) = ()
  end in
  let flow = Eio.Resource.T (attempts, Eio.Flow.Pi.two_way (module M)) in
  (flow, attempts)

let writer_stops_on_dead_transport () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun _sw ->
  let terminal = ref "none" in
  let ep =
    E.create E.Server (fun wsd ->
        (* buffer one frame so the writer has output to flush *)
        E.Wsd.send_text wsd "hello";
        E.handlers
          ~on_eof:(fun () -> terminal := "eof")
          ~on_error:(fun m -> terminal := "error:" ^ m)
          ())
  in
  let flow, attempts = dead_flow () in
  D.drive flow ep;
  Alcotest.(check int)
    "writer made exactly one write attempt then stopped (no busy-spin)" 1
    !attempts;
  Alcotest.(check bool) "a terminal handler fired" true (!terminal <> "none")

let () =
  Alcotest.run "ws-direct-driver"
    [ ( "writer"
      , [ Alcotest.test_case "stops on dead transport (no spin)" `Quick
            writer_stops_on_dead_transport
        ] )
    ]
