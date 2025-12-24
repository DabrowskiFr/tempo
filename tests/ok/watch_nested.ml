open Tempo

let rec workload label step limit () =
  if step >= limit then
    Format.printf "[%s] completed (%d steps)@.%!" label limit
  else begin
    Format.printf "[%s] step %d@.%!" label step;
    pause ();
    workload label (step + 1) limit ()
  end

let scenario () =
  let stop = new_signal () in
  let override = new_signal () in
  let body () =
    watch stop (fun () ->
        Format.printf "[body] outer watch entered@.%!";
        watch override (fun () ->
            Format.printf "[body] inner watch entered@.%!";
            workload "body" 0 10 ());
        Format.printf "[body] inner watch exited@.%!");
    Format.printf "[body] outer watch exited@.%!"
  and driver () =
    Format.printf "[driver] override will fire first@.%!";
    pause ();
    emit override ();
    Format.printf "[driver] override emitted@.%!";
    pause ();
    Format.printf "[driver] stop emitted@.%!";
    emit stop ();
    Format.printf "[driver] done@.%!"
  in
  parallel [body; driver]

let () = execute (fun _ _ -> scenario ())
