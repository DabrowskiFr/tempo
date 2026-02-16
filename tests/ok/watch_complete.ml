open Tempo

let () =
  let run ~swap =
    execute_trace ~instants:12 ~inputs:[ None ] (fun _input output ->
        let trigger = new_signal () in
        let ticks = new_state 0 in
        let finished = new_state false in
        let body () =
          watch trigger (fun () ->
              for _ = 1 to 3 do
                modify_state ticks (fun x -> x + 1);
                pause ()
              done);
          set_state finished true
        in
        let driver () =
          pause ();
          pause ();
          pause ();
          pause ()
        in
        let procs = [ body; driver ] in
        parallel (if swap then List.rev procs else procs);
        emit output (get_state ticks, get_state finished))
  in
  let a = run ~swap:false in
  let b = run ~swap:true in
  match (a, b) with
  | [ (t1, f1) ], [ (t2, f2) ] ->
      Printf.printf "tag=codee;a=(%d,%b);b=(%d,%b);eq=%b\n" t1 f1 t2 f2 (a = b)
  | _ -> Printf.printf "unexpected\n"
