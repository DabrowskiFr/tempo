open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let scenario () =
  let guard = new_signal () in
  let cancel = new_signal () in
  let worker () =
    watch cancel (fun () ->
        when_ guard (fun () ->
            log "worker" "guard satisfied, watch started";
            pause ();
            log "worker" "still running under guard";
            pause ();
            log "worker" "UNREACHABLE: watch should have killed me"))
  in
  let driver () =
    log "driver" "emit guard to start worker";
    emit guard ();
    pause ();
    log "driver" "re-emit guard and emit cancel => watch preempts";
    emit guard ();
    emit cancel ();
    pause ();
    log "driver" "worker aborted by watch"
  in
  parallel [ driver; worker ]

let () = execute (fun _ _ -> scenario ())
