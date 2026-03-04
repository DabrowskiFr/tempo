open Tempo

let program _ _ =
  let ev = new_signal () in
  let agg = new_signal_agg ~initial:0 ~combine:( + ) in

  let child =
    Low_level.fork (fun () ->
        pause ();
        emit ev ();
        let _ = await_immediate ev in
        emit agg 1;
        let _ = await agg in
        ())
  in

  let kill = Low_level.new_kill () in
  Low_level.with_kill kill (fun () ->
      when_ ev (fun () -> ());
      Low_level.join child);

  let _ = await agg in
  ()

let _ = execute program
