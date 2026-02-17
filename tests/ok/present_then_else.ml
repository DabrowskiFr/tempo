open Tempo

let () =
  let run () =
    execute_trace ~instants:5 ~inputs:[ None ] (fun _input output ->
        let trigger = new_signal () in
        let then_hits = State.create 0 in
        let else_hits = State.create 0 in
        App.present_then_else trigger
          (fun () -> State.modify then_hits (fun x -> x + 1))
          (fun () -> State.modify else_hits (fun x -> x + 1));
        emit trigger ();
        App.present_then_else trigger
          (fun () -> State.modify then_hits (fun x -> x + 1))
          (fun () -> State.modify else_hits (fun x -> x + 1));
        pause ();
        emit output (State.get then_hits, State.get else_hits))
  in
  let a = run () in
  let b = run () in
  match (a, b) with
  | [ (t1, e1) ], [ (t2, e2) ] ->
      Printf.printf "tag=codee;a=(%d,%d);b=(%d,%d);eq=%b\n" t1 e1 t2 e2 (a = b)
  | _ -> Printf.printf "unexpected\n"
