open Tempo

let scenario () =
  let trigger = new_signal () in
  let worker_steps = ref 0 in
  let worker_finished = ref false in
  let body_completed = ref false in
  let driver_done = ref false in
  let body () =
    watch trigger (fun () ->
        let rec run step () =
          if step >= 3 then worker_finished := true
          else (
            incr worker_steps;
            pause ();
            run (step + 1) ())
        in
        run 0 ());
    body_completed := true
  in
  let driver () =
    pause ();
    pause ();
    pause ();
    driver_done := true
  in
  parallel [ body; driver ];
  if !worker_steps <> 3 then failwith "worker did not perform exactly 3 steps";
  if not !worker_finished then failwith "worker did not finish";
  if not !body_completed then failwith "body did not complete";
  if not !driver_done then failwith "driver did not complete";
  Format.printf "worker_steps=%d@.%!" !worker_steps;
  Format.printf "worker_finished=%b@.%!" !worker_finished;
  Format.printf "body_completed=%b@.%!" !body_completed;
  Format.printf "driver_done=%b@.%!" !driver_done

let () = execute ~instants:10 (fun _ _ -> scenario ())
