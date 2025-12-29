open Tempo

let fail msg = failwith msg

let program _input _output =
  let st = new_state 0 in
  parallel
    [ (fun () ->
        Game.after_n 1 (fun () -> set_state st 3);
        Game.after_n 1 (fun () -> modify_state st (fun x -> x + 4)))
    ; (fun () ->
        let a = await_state st in
        if a <> 3 then fail "first state update mismatch";
        let b = await_state st in
        if b <> 7 then fail "second state update mismatch")
    ];
  if get_state st <> 7 then fail "final state mismatch"

let () = execute ~instants:6 program
