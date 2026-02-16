open Tempo

let fail msg = failwith msg

let program _input output =
  let branch () =
    pause ();
    pause ()
  in
  parallel [ branch; branch; branch; branch ];
  emit output 1

let () =
  let snapshots = ref [] in
  let on_instant snap = snapshots := snap :: !snapshots in
  let outputs = ref [] in
  let output v = outputs := v :: !outputs in
  execute_inspect ~instants:4 ~output ~on_instant program;
  let snaps = List.rev !snapshots in
  if List.length snaps <> 3 then fail "unexpected instant count";
  List.iteri
    (fun i s ->
      if s.instant <> i then fail "unexpected instant index";
      if s.current_tasks <> 0 then
        fail "current queue must be empty when on_instant callback runs";
      if s.signal_count <> 2 then fail "unexpected signal registry size")
    snaps;
  if List.rev !outputs <> [ 1 ] then fail "unexpected output sequence"
