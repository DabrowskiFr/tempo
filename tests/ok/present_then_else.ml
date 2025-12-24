open Tempo

let scenario () =
  let trigger = new_signal () in

  Format.printf "[driver] instant 0: signal absent@.%!";
  present_then_else trigger
    (fun () ->
      Format.printf "[present] unexpected THEN in instant 0@.%!")
    (fun () ->
      Format.printf "[present] ELSE executes at instant 1@.%!");
  Format.printf "[driver] instant 1: emit trigger before present@.%!";
  emit trigger ();
  present_then_else trigger
    (fun () ->
      Format.printf "[present] THEN executes in instant 1@.%!")
    (fun () ->
      Format.printf "[present] unexpected ELSE in instant 2@.%!");

  Format.printf "[driver] done@.%!"

let () = execute (fun _ _ -> scenario ())
