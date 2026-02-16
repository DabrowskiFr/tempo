open Tempo

let run_worker steps st () =
  for _ = 1 to steps do
    modify_state st (fun x -> x + 1);
    pause ()
  done

let () =
  let run branches =
    execute_trace ~instants:16 ~inputs:[ None ] (fun _input output ->
        let total = new_state 0 in
        parallel (List.map (fun s -> run_worker s total) branches);
        emit output (get_state total))
  in
  let a = run [ 2; 3; 4 ] in
  let b = run [ 4; 2; 3 ] in
  match (a, b) with
  | [ va ], [ vb ] -> Printf.printf "tag=codee;a=%d;b=%d;eq=%b\n" va vb (a = b)
  | _ -> Printf.printf "unexpected\n"
