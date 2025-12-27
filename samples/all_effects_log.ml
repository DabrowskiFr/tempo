open Tempo

(* Exercise all effects to observe logging. *)
let program _ _ =
  let ev = new_signal () in
  let agg = new_signal_agg ~initial:0 ~combine:( + ) in

  (* Fork a child that will pause, emit, await, and then complete. *)
  let child =
    fork (fun () ->
        (* pause / emit / await_immediate *)
        pause ();
        emit ev ();
        let _ = await_immediate ev in
        (* aggregate: emit value and await it *)
        emit agg 1;
        let _ = await agg in
        ())
  in

  (* with_kill + with_guard to exercise those paths *)
  let kill = Low_level.new_kill () in
  Low_level.with_kill kill (fun () ->
      when_ ev (fun () -> ());
      (* join the child *)
      join child);

  (* Final await to ensure completion *)
  let _ = await agg in
  ()

let _ = execute program
