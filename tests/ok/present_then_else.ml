open Tempo

let () =
  let run () =
    execute_trace ~instants:5 ~inputs:[ None ] (fun _input output ->
        let trigger = new_signal () in
        let then_hits = new_state 0 in
        let else_hits = new_state 0 in
        present_then_else trigger
          (fun () -> modify_state then_hits (fun x -> x + 1))
          (fun () -> modify_state else_hits (fun x -> x + 1));
        emit trigger ();
        present_then_else trigger
          (fun () -> modify_state then_hits (fun x -> x + 1))
          (fun () -> modify_state else_hits (fun x -> x + 1));
        pause ();
        emit output (get_state then_hits, get_state else_hits))
  in
  let a = run () in
  let b = run () in
  match (a, b) with
  | [ (t1, e1) ], [ (t2, e2) ] ->
      Printf.printf "tag=codee;a=(%d,%d);b=(%d,%d);eq=%b\n" t1 e1 t2 e2 (a = b)
  | _ -> Printf.printf "unexpected\n"
