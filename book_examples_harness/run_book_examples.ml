let run_one name f =
  try
    let observed = f () in
    Printf.printf "OK %s -> %s\n" name observed
  with exn ->
    failwith (Printf.sprintf "book example %s failed: %s" name (Printexc.to_string exn))

let () =
  run_one Emit_await_example.name Emit_await_example.run;
  run_one Await_immediate_example.name Await_immediate_example.run;
  run_one When_watch_parallel_example.name When_watch_parallel_example.run;
  run_one Execute_trace_example.name Execute_trace_example.run
