open Tempo

let fail msg = failwith msg
module Runtime = Extensions.Runtime
module State_cell = Runtime.State_cell

let program _input _output =
  let st = State_cell.create 0 in
  parallel
    [ (fun () ->
        Game.after_n 1 (fun () -> State_cell.set st 3);
        Game.after_n 1 (fun () -> State_cell.modify st (fun x -> x + 4)))
    ; (fun () ->
        let a = State_cell.await st in
        if a <> 3 then fail "first state update mismatch";
        let b = State_cell.await st in
        if b <> 7 then fail "second state update mismatch")
    ];
  if State_cell.get st <> 7 then fail "final state mismatch"

let () = execute ~instants:6 program
