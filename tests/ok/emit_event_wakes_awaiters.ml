open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let consumer id signal () =
  log id "waiting for payload...";
  let value = await signal in
  log id (Format.sprintf "received payload %d" value)

let producer signal () =
  log "producer" "emit payload";
  emit signal 7

let scenario () =
  let payload = new_signal () in
  parallel
    [
      consumer "consumer1" payload
    ; consumer "consumer2" payload
    ; producer payload
    ]

let () = execute (fun _ _ -> scenario ())
