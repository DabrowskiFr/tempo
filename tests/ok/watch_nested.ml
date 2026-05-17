open Tempo

let scenario () =
  let stop = new_signal () in
  let override = new_signal () in
  let outer_entered = ref false in
  let inner_entered = ref false in
  let inner_exited = ref false in
  let outer_exited = ref false in
  let override_emitted = ref false in
  let stop_emitted = ref false in
  let steps = ref 0 in
  let body () =
    watch stop (fun () ->
        outer_entered := true;
        watch override (fun () ->
            inner_entered := true;
            let rec loop step () =
              if step >= 10 then ()
              else (
                incr steps;
                pause ();
                loop (step + 1) ())
            in
            loop 0 ());
        inner_exited := true);
    outer_exited := true
  and driver () =
    pause ();
    emit override ();
    override_emitted := true;
    pause ();
    emit stop ();
    stop_emitted := true
  in
  parallel [ body; driver ];
  if not !outer_entered then failwith "outer watch did not start";
  if not !inner_entered then failwith "inner watch did not start";
  if not !inner_exited then failwith "inner watch did not exit";
  if not !outer_exited then failwith "outer watch did not exit";
  if not !override_emitted then failwith "override was not emitted";
  if not !stop_emitted then failwith "stop was not emitted";
  if !steps >= 10 then failwith "inner watch was not preempted";
  Format.printf "outer_entered=%b@.%!" !outer_entered;
  Format.printf "inner_entered=%b@.%!" !inner_entered;
  Format.printf "inner_exited=%b@.%!" !inner_exited;
  Format.printf "outer_exited=%b@.%!" !outer_exited;
  Format.printf "override_emitted=%b@.%!" !override_emitted;
  Format.printf "stop_emitted=%b@.%!" !stop_emitted;
  Format.printf "inner_steps=%d@.%!" !steps

let () = execute (fun _ _ -> scenario ())
