open Tempo

let observe_guard ~guard ~events ~st () =
  for _ = 1 to events do
    when_ guard (fun () -> State.modify st (fun x -> x + 1));
    pause ()
  done

let combined_listener guard_a guard_b combo () =
  when_ guard_a (fun () -> when_ guard_b (fun () -> State.modify combo (fun x -> x + 1)));
  pause ()

let driver guard_a guard_b () =
  emit guard_a ();
  pause ();
  emit guard_a ();
  emit guard_b ();
  pause ();
  emit guard_a ();
  pause ();
  pause ()

let () =
  let run ~swap =
    execute_trace ~instants:16 ~inputs:[ None ] (fun _input output ->
        let guard_a = new_signal () in
        let guard_b = new_signal () in
        let a_hits = State.create 0 in
        let b_hits = State.create 0 in
        let combo = State.create 0 in
        let procs =
          [
            driver guard_a guard_b;
            observe_guard ~guard:guard_a ~events:3 ~st:a_hits;
            observe_guard ~guard:guard_b ~events:1 ~st:b_hits;
            combined_listener guard_a guard_b combo;
          ]
        in
        parallel (if swap then List.rev procs else procs);
        emit output (State.get a_hits, State.get b_hits, State.get combo))
  in
  let a = run ~swap:false in
  let b = run ~swap:true in
  match (a, b) with
  | [ (a1, b1, c1) ], [ (a2, b2, c2) ] ->
      Printf.printf "tag=codee;a=(%d,%d,%d);b=(%d,%d,%d);eq=%b\n" a1 b1 c1 a2 b2 c2 (a = b)
  | _ -> Printf.printf "unexpected\n"
