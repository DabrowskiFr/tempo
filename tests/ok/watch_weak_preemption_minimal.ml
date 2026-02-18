open Tempo

let pp_int_list xs =
  "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"

let () =
  (* Pedagogical counterexample: under weak preemption, the continuation after
     [watch s ...] must keep running in the same instant even when [s] is
     emitted by the watched body. *)
  let outputs =
    execute_trace ~instants:5 ~inputs:[ None; None; None; None; None ]
      (fun _input output ->
        let s = new_signal () in
        let x = State.create 0 in
        watch s (fun () ->
            pause ();
            emit s 1;
            State.set x 20);
        ignore (await_immediate s);
        State.set x 30;
        pause ();
        emit output (State.get x))
  in
  if outputs <> [ 30 ] then
    failwith
      (Printf.sprintf "weak-preemption regression: expected [30], got %s"
         (pp_int_list outputs))
