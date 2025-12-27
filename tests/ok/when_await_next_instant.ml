open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let consumer guard payload () =
  when_ guard (fun () ->
      log "consumer" "guard present but payload missing, await should block";
      let value = await payload in
      log "consumer"
        (Printf.sprintf "resumed when guard was re-emitted (value=%d)" value))

let producer guard payload () =
  log "producer" "emit guard only";
  emit guard ();
  pause ();
  log "producer"
    "re-emit guard and payload to release consumer (fails, awaits is delayed)";
  emit guard ();
  emit payload 21;
  log "producer" "done"

let scenario () =
  let guard = new_signal () in
  let payload = new_signal () in
  parallel [ producer guard payload; consumer guard payload ]

let () = execute ~instants:3 (fun _ _ -> scenario ())
