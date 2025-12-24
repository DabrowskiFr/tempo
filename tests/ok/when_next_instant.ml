open Tempo
 
 let log tag message =
   Format.printf "[%s] %s@.%!" tag message

let scenario () =
  let trigger = new_signal () in
  let driver () =
    log "driver" "do nothing";
    pause ();
    log "driver" "emit trigger";
    emit trigger ()
  in let body () =
    when_ trigger (fun () ->
    log "body" "guard satisfied")
  in parallel [driver ; body]
  
let _ = execute (fun _ _ -> scenario ())
