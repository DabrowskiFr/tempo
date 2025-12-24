open Tempo

let log tag message =
  Format.printf "[%s] %s@.%!" tag message

let consumer : int signal -> unit -> unit =
  fun payload () ->
    log "consumer" "waiting for payload...";
    let value = await payload in
      log "consumer" (Format.sprintf "received payload %d" value)

let producer : int signal -> unit -> unit =
  fun payload () ->
    log "producer" "doing nothing";
    pause ();
    log "producer" "doing nothing";
    pause ();
    log "producer" "emit payload";
    emit payload 42

let scenario () =
    let payload = new_signal () in
      parallel [consumer payload; producer payload]

let () = execute (fun _ _ -> scenario ())
