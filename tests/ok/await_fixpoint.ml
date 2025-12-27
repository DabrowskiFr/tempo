open Tempo

let waiter s () =
  let _ = await s in
  Format.printf "[waiter] done"

let driver s () = emit s ()

let scenario _ _ =
  let s = new_signal () in
  parallel [ waiter s; driver s ]

let _ = execute scenario
