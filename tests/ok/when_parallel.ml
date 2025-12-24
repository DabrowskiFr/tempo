open Tempo

let log tag instant message =
  Format.printf "[%s] instant %d: %s@.%!" tag instant message

let observe_guard ~name ~guard ~events () =
  let rec loop seen remaining =
    if remaining = 0 then
      log name seen "done observing guard"
    else begin
      when_ guard (fun () ->
          log name seen "guard present, branch resumes in parallel");
      pause ();
      loop (seen + 1) (remaining - 1)
    end
  in
  loop 0 events

let combined_listener guard_a guard_b () =
  when_ guard_a (fun () ->
      when_ guard_b (fun () ->
          log "combo" 1 "guard_a et guard_b présents dans le même instant"));
  pause ();
  log "combo" 2 "combo branch done"

let driver guard_a guard_b () =
  log "driver" 0 "emit guard_a to wake the first listener";
  emit guard_a ();
  pause ();
  log "driver" 1 "emit guard_a and guard_b in the same instant";
  emit guard_a ();
  emit guard_b ();
  pause ();
  log "driver" 2 "final emission of guard_a";
  emit guard_a ();
  pause ();
  log "driver" 3 "all signals emitted"

let scenario () =
  let guard_a = new_signal () in
  let guard_b = new_signal () in
  parallel
    [ (driver guard_a guard_b)
    ; (observe_guard ~name:"listener/A" ~guard:guard_a ~events:3)
    ; (observe_guard ~name:"listener/B" ~guard:guard_b ~events:1)
    ; (combined_listener guard_a guard_b)
    ];
  Format.printf "[scenario] all parallel branches completed@.%!"

let () = execute (fun _ _ -> scenario ())
