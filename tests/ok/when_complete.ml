open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let scenario () =
  let s = new_signal () in
  let driver () =
    pause ();
    log "driver" "emitting signal to complete when_";
    emit s ()
  in
  let worker () =
    when_ s (fun () -> ());
    log "when_complete" "when_ has completed"
  in
  parallel [ driver; worker ]

let () = execute (fun _ _ -> scenario ())
