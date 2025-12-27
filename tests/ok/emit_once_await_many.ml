open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let consumer name payload () =
  log name "await payload...";
  let value = await payload in
  log name (Printf.sprintf "received payload %d" value)

let producer payload () =
  pause ();
  log "producer" "emit payload";
  emit payload 42

let scenario () =
  let payload = new_signal () in
  parallel
    [
      consumer "consumer 1" payload
    ; consumer "consumer 2" payload
    ; producer payload
    ]

let () = execute (fun _ _ -> scenario ())
