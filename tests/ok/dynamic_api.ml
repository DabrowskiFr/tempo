open Tempo

let fail msg = failwith msg
module Runtime = Extensions.Runtime
module State_cell = Runtime.State_cell
module Dynamic = Runtime.Dynamic

let program _input _output =
  let counter = State_cell.create 0 in
  let worker () =
    let rec loop () =
      State_cell.modify counter (fun x -> x + 1);
      pause ();
      loop ()
    in
    loop ()
  in
  let h = Dynamic.spawn worker in
  Game.after_n 5 (fun () -> Dynamic.stop h);
  Dynamic.join h;
  let v = State_cell.get counter in
  if v < 4 || v > 6 then fail (Printf.sprintf "unexpected counter value: %d" v)

let () = execute ~instants:10 program
