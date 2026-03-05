open Tempo

let () =
  let run ~swap =
    execute_trace ~instants:14 ~inputs:[ None ] (fun _input output ->
        let trigger = new_signal () in
        let ticks = State.create 0 in
        let ended = State.create false in
        let body () =
          watch trigger (fun () ->
              while true do
                State.modify ticks (fun x -> x + 1);
                pause ()
              done);
          State.set ended true
        in
        let driver () =
          pause ();
          pause ();
          emit trigger ();
          pause ()
        in
        let procs = [ body; driver ] in
        parallel (if swap then List.rev procs else procs);
        emit output (State.get ticks, State.get ended))
  in
  let a = run ~swap:false in
  let b = run ~swap:true in
  match (a, b) with
  | [ (t1, e1) ], [ (t2, e2) ] ->
      Printf.printf "tag=codee;a=(%d,%b);b=(%d,%b);eq=%b\n" t1 e1 t2 e2 (a = b)
  | _ -> Printf.printf "unexpected\n"
