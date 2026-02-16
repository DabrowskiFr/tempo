open Tempo

let () =
  let run ~swap =
    execute_trace ~instants:14 ~inputs:[ None ] (fun _input output ->
        let trigger = new_signal () in
        let ticks = new_state 0 in
        let ended = new_state false in
        let body () =
          watch trigger (fun () ->
              while true do
                modify_state ticks (fun x -> x + 1);
                pause ()
              done);
          set_state ended true
        in
        let driver () =
          pause ();
          pause ();
          emit trigger ();
          pause ()
        in
        let procs = [ body; driver ] in
        parallel (if swap then List.rev procs else procs);
        emit output (get_state ticks, get_state ended))
  in
  let a = run ~swap:false in
  let b = run ~swap:true in
  match (a, b) with
  | [ (t1, e1) ], [ (t2, e2) ] ->
      Printf.printf "tag=codee;a=(%d,%b);b=(%d,%b);eq=%b\n" t1 e1 t2 e2 (a = b)
  | _ -> Printf.printf "unexpected\n"
