open Tempo

let log tag instant message =
  Format.printf "[%s] instant %d: %s@.%!" tag instant message

let consumer guard payload () =
  when_ guard (fun () ->
      log "scenario1/consumer" 0 "guard satisfied, awaiting payload";
      let value = await_immediate payload in
      log "scenario1/consumer" 0
        (Printf.sprintf "payload received immediately (%d)" value))

let producer guard payload () =
  log "scenario1/driver" 0 "emit guard and payload in the same instant";
  emit guard ();
  emit payload 10;
  log "scenario1/driver" 0 "done"

let scenario () =
  let guard = new_signal () in
  let payload = new_signal () in
  parallel [ producer guard payload; consumer guard payload ]

let () = execute (fun _ _ -> scenario ())
