open Tempo

let program _ _ =
  let s = new_signal () in
  let _ =
    Low_level.fork (fun () ->
        pause ();
        emit s ())
  in
  let () = await s in
  Format.printf "signal received@."

let _ = execute program
