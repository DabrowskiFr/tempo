open Tempo

let log tag message =
  Format.printf "[%s] %s@.%!" tag message

let scenario () =
  let a = new_signal () in
  let b = new_signal () in
  let body () =
    when_ a (fun () ->
      when_ b (fun () ->
        log "nested" "both guards satisfied"))
  in let driver () = 
    log "driver" "emit only a";
    emit a ();
    pause ();
    log "driver" "emit b (a from previous instant should not count)";
    emit b ();
    pause ();
    log "driver" "emit a and b together to satisfy nested guard";
    emit a ();
    emit b ();
    pause ();
    log "driver" "done"
  in parallel [body; driver]   

  let _ = execute (fun _ _ -> scenario ())
