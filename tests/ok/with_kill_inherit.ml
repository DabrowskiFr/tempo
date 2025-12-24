open Tempo

let scenario () =
  let kill = Low_level.new_kill () in
  let parent () =
    Low_level.with_kill kill (fun () ->
        Format.printf "[parent] may execute one step@.%!";
        let _ =
          Low_level.fork (fun () ->
              Format.printf "[child] may be unreachable@.%!";
              pause ();
              Format.printf "[child] UNREACHABLE@.%!")
        in
        pause ();
        Format.printf "[parent] UNREACHABLE@.%!");
    Format.printf "[parent] continuation after cancel@.%!"
  in
  let killer () =
    Format.printf "[killer] abort shared kill@.%!";
    Low_level.abort_kill kill
  in
  parallel [ parent; killer ]

let () = execute ~instants:5 (fun _ _ -> scenario ())
