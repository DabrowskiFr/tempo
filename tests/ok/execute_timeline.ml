open Tempo

let fail msg = failwith msg

let script = [ Some 1; None; Some 2; Some 3 ]

let program input output =
  let rec loop tick =
    when_ input (fun () ->
        let v = await_immediate input in
        emit output (tick + v));
    pause ();
    loop (tick + 1)
  in
  loop 0

let () =
  let timeline = execute_timeline ~inputs:script program in
  if List.length timeline <> List.length script then fail "timeline length mismatch";
  let outputs = List.map (fun r -> r.output) timeline in
  let expected = [ Some 1; None; Some 3; Some 5 ] in
  if outputs <> expected then fail "unexpected output timeline"
