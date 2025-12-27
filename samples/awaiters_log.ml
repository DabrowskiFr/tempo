open Tempo

(* Demonstrates a task waiting on a signal: the awaiters list should show up in logs. *)
let program _ _ =
  let s = new_signal () in
  (* Emit the signal in the next instant so the awaiter is visible during the first. *)
  let _ = fork (fun () -> pause (); emit s ()) in
  let () = await s in
  Format.printf "signal received@."

let _ = execute program
