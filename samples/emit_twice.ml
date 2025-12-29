open Tempo

let emitter _ _ =
  let s1 = new_signal () in
  let s2 = new_signal () in
  emit s1 ();
  pause ();
  emit s1 ();
  emit s2 ()

let _ = execute emitter
