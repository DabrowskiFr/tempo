open Tempo

let program _ _ =
  let s = new_signal () in
  parallel
    [
      (fun () -> when_ s (fun () -> Format.printf "signal caught@."))
    ; (fun () ->
        emit s ();
        Format.printf "signal emitted@.")
    ]

let _ = execute program
