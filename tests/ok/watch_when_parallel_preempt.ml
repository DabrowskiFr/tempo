open Tempo

let () =
  let outputs =
    execute_trace ~instants:12 ~inputs:[ None ] (fun _input output ->
        let gate = new_signal () in
        let stop = new_signal () in
        let count = State.create 0 in
        parallel
          [
            (fun () ->
              for i = 1 to 6 do
                if i < 3 then emit gate ();
                if i = 3 then emit stop ();
                pause ()
              done);
            (fun () ->
              watch stop (fun () ->
                  for _ = 1 to 10 do
                    pause ();
                    when_ gate (fun () -> State.modify count (fun x -> x + 1))
                  done);
              emit output (State.get count));
          ])
  in
  match outputs with
  | [ n ] -> Printf.printf "count-before-stop: %d\n" n
  | _ -> Printf.printf "unexpected-outputs\n"
