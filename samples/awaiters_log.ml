open Tempo

let program _ _ =
  let s = new_signal () in
  let producer () =
    pause ();
    emit s ()
  in
  let consumer () =
    let () = await s in
    Format.printf "signal received@."
  in
  parallel [ producer; consumer ]

let _ = execute program
