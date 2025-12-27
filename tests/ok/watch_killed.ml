open Tempo

let rec work name n () =
  Printf.printf "[%s] cycle %d\n%!" name n;
  pause ();
  work name (n + 1) ()

let scenario () =
  let trigger = new_signal () in
  let body () =
    watch trigger (fun () ->
        Printf.printf "[body] start\n%!";
        work "body" 0 ());
    Printf.printf "[body] end (killed)\n%!"
  in
  let driver () =
    pause ();
    pause ();
    Printf.printf "[driver] emit trigger\n%!";
    emit trigger ();
    pause ();
    Printf.printf "[driver] end\n%!"
  in
  parallel [ body; driver ]

let () = execute (fun _ _ -> scenario ())
