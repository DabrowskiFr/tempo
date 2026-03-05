open Tempo

let () =
  let run () =
    execute_trace ~instants:8 ~inputs:[ None ] (fun _input output ->
        let kill = Low_level.new_kill () in
        let body_steps = State.create 0 in
        let continued = State.create false in
        let body () =
          Low_level.with_kill kill (fun () ->
              State.modify body_steps (fun x -> x + 1);
              pause ();
              State.modify body_steps (fun x -> x + 1000));
          State.set continued true
        in
        let driver () = Low_level.abort_kill kill in
        parallel [ body; driver ];
        emit output (State.get body_steps, State.get continued))
  in
  let a = run () in
  let b = run () in
  match (a, b) with
  | [ (s1, c1) ], [ (s2, c2) ] ->
      Printf.printf "tag=low_level;a=(%d,%b);b=(%d,%b);eq=%b\n" s1 c1 s2 c2 (a = b)
  | _ -> Printf.printf "unexpected\n"
