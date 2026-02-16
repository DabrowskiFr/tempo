open Tempo

let fail msg = failwith msg

let program input output =
  let rec loop () =
    when_ input (fun () ->
        let v = await_immediate input in
        emit output v);
    pause ();
    loop ()
  in
  loop ()

let () =
  let script = [ None; Some 7; None ] in
  let remaining_inputs = ref script in
  let input () =
    match !remaining_inputs with
    | [] -> None
    | x :: xs ->
        remaining_inputs := xs;
        x
  in
  let outputs = ref [] in
  let output v = outputs := v :: !outputs in
  let snapshots = ref [] in
  let on_instant snap = snapshots := snap :: !snapshots in
  execute_inspect ~instants:3 ~input ~output ~on_instant program;
  let snaps = List.rev !snapshots in
  match snaps with
  | [ s0; s1; s2 ] ->
      if s0.instant <> 0 || s1.instant <> 1 || s2.instant <> 2 then
        fail "unexpected instant numbering";
      if s0.current_tasks <> 0 || s1.current_tasks <> 0 || s2.current_tasks <> 0 then
        fail "current queue must be empty at instant end";
      if s0.signal_count <> 2 || s1.signal_count <> 2 || s2.signal_count <> 2 then
        fail "unexpected signal registry size";
      if s0.blocked_tasks <> 1 || s0.next_tasks <> 0 then
        fail "instant 0 order mismatch (blocked before rollover)";
      if s1.blocked_tasks <> 0 || s1.next_tasks <> 1 then
        fail "instant 1 order mismatch (pause scheduled before rollover)";
      if s2.blocked_tasks <> 1 || s2.next_tasks <> 0 then
        fail "instant 2 order mismatch (blocked before rollover)";
      if List.rev !outputs <> [ 7 ] then
        fail "unexpected output sequence"
  | _ -> fail "unexpected snapshot count"
