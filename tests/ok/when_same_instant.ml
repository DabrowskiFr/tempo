open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let driver trigger () =
  log "driver" "emit trigger before installing when_";
  emit trigger ()

let body trigger () =
  when_ trigger (fun () -> log "body" "guard satisfied immediately")

let scenario () =
  let trigger = new_signal () in
  parallel [ driver trigger; body trigger ]

let _ = execute (fun _ _ -> scenario ())
