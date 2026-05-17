open Tempo

let scenario () =
  let guard = new_signal () in
  let cancel = new_signal () in
  let worker_started = ref false in
  let worker_mid = ref false in
  let worker_reached_unreachable = ref false in
  let cancel_emitted = ref false in
  let driver_done = ref false in
  let worker () =
    watch cancel (fun () ->
        when_ guard (fun () ->
            worker_started := true;
            pause ();
            worker_mid := true;
            pause ();
            worker_reached_unreachable := true))
  in
  let driver () =
    emit guard ();
    pause ();
    emit guard ();
    emit cancel ();
    cancel_emitted := true;
    pause ();
    driver_done := true
  in
  parallel [ driver; worker ];
  if not !worker_started then failwith "worker did not start";
  if not !worker_mid then failwith "worker did not run before cancellation";
  if not !cancel_emitted then failwith "cancel was not emitted";
  if not !driver_done then failwith "driver did not complete";
  if !worker_reached_unreachable then failwith "watch did not preempt worker";
  Format.printf "worker_started=%b@.%!" !worker_started;
  Format.printf "worker_mid=%b@.%!" !worker_mid;
  Format.printf "cancel_emitted=%b@.%!" !cancel_emitted;
  Format.printf "worker_preempted=%b@.%!" (not !worker_reached_unreachable);
  Format.printf "driver_done=%b@.%!" !driver_done

let () = execute (fun _ _ -> scenario ())
