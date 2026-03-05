open Tempo

let branches = 48
let work = 4

let () =
  let outputs =
    execute_trace ~instants:(work + 6) ~inputs:[ None ] (fun _input output ->
        let done_count = State.create 0 in
        let mk_branch _id () =
          for _ = 1 to work do
            pause ()
          done;
          State.modify done_count (fun x -> x + 1)
        in
        parallel (List.init branches mk_branch);
        emit output (State.get done_count))
  in
  match outputs with
  | [ n ] -> Printf.printf "done-count: %d\n" n
  | _ -> Printf.printf "unexpected-outputs\n"
