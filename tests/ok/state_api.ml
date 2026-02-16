open Tempo

let fail msg = failwith msg

let program _input _output =
  let st = State.create 0 in
  parallel
    [ (fun () ->
        Game.after_n 1 (fun () -> State.set st 3);
        Game.after_n 1 (fun () -> State.modify st (fun x -> x + 4)))
    ; (fun () ->
        let a = State.await st in
        if a <> 3 then fail "first state update mismatch";
        let b = State.await st in
        if b <> 7 then fail "second state update mismatch")
    ];
  if State.get st <> 7 then fail "final state mismatch"

let () = execute ~instants:6 program
