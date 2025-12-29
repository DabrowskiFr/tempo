open Tempo

let fail msg = failwith msg

let program _input _output =
  let counter = new_state 0 in
  let worker () =
    let rec loop () =
      modify_state counter (fun x -> x + 1);
      pause ();
      loop ()
    in
    loop ()
  in
  let h = Dynamic.spawn worker in
  Game.after_n 5 (fun () -> Dynamic.stop h);
  Dynamic.join h;
  let v = get_state counter in
  if v < 4 || v > 6 then fail (Printf.sprintf "unexpected counter value: %d" v)

let () = execute ~instants:10 program
