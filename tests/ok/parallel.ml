open Tempo

let worker name steps =
  let rec run  = function
    | [] ->
        Format.printf "[%s] done@.%!" name 
    | msg :: rest ->
        Format.printf "[%s] %s@.%!" name msg;
        pause ();
        run rest
  in
  run steps

let scenario () =
  Format.printf "[driver] launch parallel workers@.%!";
  parallel
    [ (fun () -> worker "fast" [ "start"; "finish" ])
    ; (fun () -> worker "medium" [ "start"; "middle"; "finish" ])
    ; (fun () -> worker "slow" [ "start"; "middle"; "one more time"; "finish" ])
    ];
  Format.printf "[driver] instant 3: all branches completed@.%!"

let () = execute (fun _ _ -> scenario ())
