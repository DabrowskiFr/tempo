open Tempo

 let log tag message =
   Format.printf "[%s] %s@.%!" tag message

  let scenario () =
    let s = new_signal () in
    let driver () =
      begin
        pause ();
        log "driver" "emitting signal to complete when_";
        emit s ()
      end
    in let worker () =
      begin 
        when_ s (fun () -> ());
        log "when_complete" "when_ has completed"
      end   
    in parallel [driver ; worker]

  let () = execute (fun _ _ -> scenario ())
