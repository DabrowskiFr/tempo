open Tempo

let rec worker step limit () =
  if step >= limit then
    Format.printf "[worker] finished after %d steps@.%!" limit
  else (
    Format.printf "[worker] instant %d@.%!" step;
    pause ();
    worker (step + 1) limit ())

let scenario () =
  let trigger = new_signal () in
  let body () =
    watch trigger (fun () ->
        Format.printf "[body] start@.%!";
        worker 0 3 ());
    Format.printf "[body] end (natural completion)@.%!"
  in
  let driver () =
    Format.printf "[driver] trigger will stay silent@.%!";
    pause ();
    pause ();
    pause ();
    Format.printf "[driver] done@.%!"
  in
  parallel [ body; driver ]

let () = execute ~instants:10 (fun _ _ -> scenario ())
