open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let scenario () =
  let guard = new_signal () in
  let cancel = new_signal () in
  let worker () =
    watch cancel (fun () ->
        when_ guard (fun () ->
            log "worker" "guard satisfied, running";
            pause ();
            log "worker" "no cancel yet, continue";
            pause ();
            log "worker" "finished without preemption"))
  in
  let driver () =
    log "driver" "emit guard to start worker";
    emit guard ();
    pause ();
    log "driver" "emit guard again to keep worker running";
    emit guard ();
    pause ();
    log "driver" "emit guard one last time, no cancel";
    emit guard ();
    pause ();
    log "driver" "one more to check";
    emit guard ()
  in
  parallel [ driver; worker ]

let _ = execute ~instants:10 (fun _ _ -> scenario ())
