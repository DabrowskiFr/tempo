open Tempo

let log tag instant message =
  Format.printf "[%s] instant %d: %s@.%!" tag instant message

let worker guard ack payload () =
  when_ guard (fun () ->
      log "scenario3/worker" 0 "guard true, waiting for ack";
      when_ ack (fun () ->
          log "scenario3/worker" 1
            "ack seen in the same instant, now awaiting payload";
          let value = await payload in
          log "scenario3/worker" 2
            (Printf.sprintf "payload delivered later (%d), worker done" value)))

let driver guard ack payload () =
  log "scenario3/driver" 0 "emit guard only";
  emit guard ();
  pause ();
  log "scenario3/driver" 1 "emit guard + ack (payload missing)";
  emit guard ();
  emit ack ();
  pause ();
  log "scenario3/driver" 2
    "re-emit guard + ack and emit payload to unblock worker";
  emit guard ();
  emit ack ();
  emit payload 84;
  log "scenario3/driver" 2 "done";
  pause ();
  emit guard ();
  emit ack ()
(* emit ack ();
  emit payload 84; *)

let scenario () =
  Format.printf "@.>>> nested_guard_with_payload <<<@.%!";
  let guard = new_signal () in
  let ack = new_signal () in
  let payload = new_signal () in
  parallel [ driver guard ack payload; worker guard ack payload ]

let () = execute (fun _ _ -> scenario ())
