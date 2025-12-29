open Tempo

let fail msg = failwith msg

let program input output =
  let rec loop () =
    when_ input (fun () -> emit output (await_immediate input + 10));
    pause ();
    loop ()
  in
  loop ()

let () =
  let timeline = execute_timeline ~inputs:[ Some 1; None; Some 2 ] program in
  let json =
    Timeline_json.of_timeline string_of_int string_of_int timeline
  in
  if not (String.length json > 8 && json.[0] = '[') then fail "invalid json timeline"
