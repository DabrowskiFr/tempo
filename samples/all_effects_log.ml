open Tempo

let program _ _ =
  let ev = new_signal () in
  let agg = new_signal_agg ~initial:0 ~combine:( + ) in

  let producer () =
    pause ();
    emit ev ();
    emit agg 1
  in
  let consumer () =
    when_ ev (fun () -> ());
    let _ = await_immediate ev in
    let _ = await agg in
    ()
  in
  parallel [ producer; consumer ]

let _ = execute program
