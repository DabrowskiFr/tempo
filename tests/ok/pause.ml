open Tempo

let log name message =
  Format.printf "[%s] %s@.%!" name message

let scenario () =
  log "simple" "started";
  pause ();
  log "simple" "resumed";
  pause ();
  log "simple" "done"


let () = execute (fun _ _ -> scenario ())
