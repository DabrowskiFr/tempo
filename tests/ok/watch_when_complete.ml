open Tempo

let scenario () =
  let guard = new_signal () in
  let cancel = new_signal () in
  let worker_started = ref false in
  let worker_continued = ref false in
  let worker_finished = ref false in
  let driver_done = ref false in
  let worker () =
    watch cancel (fun () ->
        when_ guard (fun () ->
            worker_started := true;
            pause ();
            worker_continued := true;
            pause ();
            worker_finished := true))
  in
  let driver () =
    emit guard ();
    pause ();
    emit guard ();
    pause ();
    emit guard ();
    pause ();
    driver_done := true
  in
  parallel [ driver; worker ];
  if not !worker_started then failwith "worker did not start under guard";
  if not !worker_continued then failwith "worker did not continue after first pause";
  if not !worker_finished then failwith "worker did not finish without preemption";
  if not !driver_done then failwith "driver did not complete";
  Format.printf "worker_started=%b@.%!" !worker_started;
  Format.printf "worker_continued=%b@.%!" !worker_continued;
  Format.printf "worker_finished=%b@.%!" !worker_finished;
  Format.printf "driver_done=%b@.%!" !driver_done

let () = execute ~instants:7 (fun _ _ -> scenario ())
