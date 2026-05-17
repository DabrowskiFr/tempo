open Tempo

let before_pause = ref false
let after_pause = ref false
let driver_done = ref false

let worker guard () =
  when_ guard (fun () ->
      before_pause := true;
      pause ();
      after_pause := true)

let driver guard () =
  emit guard ();
  pause ();
  driver_done := true

let () =
  execute ~instants:3 (fun _ _ ->
      let guard = new_signal () in
      parallel [ driver guard; worker guard ]);
  if not !before_pause then failwith "guarded body did not start";
  if !after_pause then failwith "guard was not preserved after pause";
  if not !driver_done then failwith "driver did not complete";
  Format.printf "before_pause=%b@.%!" !before_pause;
  Format.printf "after_pause=%b@.%!" !after_pause;
  Format.printf "driver_done=%b@.%!" !driver_done
