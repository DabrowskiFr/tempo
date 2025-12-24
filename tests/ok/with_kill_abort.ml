open Tempo

let scenario () =
  let kill = Low_level.new_kill () in
  let body () =
    Low_level.with_kill kill (fun () ->
        Format.printf "[body] execute first step@.%!";
        pause ();
        Format.printf "[body] UNREACHABLE@.%!");
    Format.printf "[body] continuation after cancel@.%!"
  in
  let driver () =
    Format.printf "[driver] abort kill@.%!";
    Low_level.abort_kill kill
  in
  parallel [ body; driver ]

let () = execute ~instants:5 (fun _ _ -> scenario ())
