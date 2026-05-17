open Tempo

let benchmark = ref "propagation_chains"
let size = ref 10
let run_id = ref 1
let diag_csv = ref ""
let diag_compact = ref false
let memtrace_file = ref ""
let memtrace_rate = ref 1e-4

let parse_args () =
  let specs =
    [
      ("--benchmark", Arg.Set_string benchmark, "Benchmark name");
      ("--size", Arg.Set_int size, "Benchmark size parameter");
      ("--run", Arg.Set_int run_id, "Run index");
      ("--diag-csv", Arg.Set_string diag_csv, "Path to write runtime/GC diagnostics as CSV");
      ("--diag-compact", Arg.Set diag_compact, "Run Gc.compact() before final benchmark row");
      ("--memtrace-file", Arg.Set_string memtrace_file, "Path to write a memtrace trace");
      ("--memtrace-rate", Arg.Set_float memtrace_rate, "Memtrace sampling rate (default: 1e-4)");
    ]
  in
  Arg.parse specs (fun _ -> ()) "tempo_bench --benchmark <name> --size <n> --run <k>"

let effective_propagation n = max 1 n
let effective_broadcast n = max 1 n
let effective_fork_depth n =
  let n = max 2 n in
  max 1 (int_of_float (log (float_of_int n) /. log 2.))

let effective_guard_depth n = max 1 n
let effective_preemption_depth n = max 1 n
let effective_multi_rounds n =
  let n = max 2 n in
  max 2 (int_of_float (log (float_of_int n) /. log 2.))

let instants_for bench n =
  match bench with
  | "propagation_chains_multi" -> effective_multi_rounds n
  | "broadcast_expansion" -> 2
  | "fork_explosion" -> effective_fork_depth n
  | "guarded_cascades_multi" -> effective_multi_rounds n
  | "nested_preemption" -> 2
  | _ -> 1

let bench_propagation_chains n =
  let n = effective_propagation n in
  let link s_in s_out () =
    ignore (await_immediate s_in);
    emit s_out ()
  in
  let rec chain depth s_in s_out () =
    if depth <= 0 then
      emit s_out ()
    else
      let mid = new_signal () in
      parallel [ link s_in mid; chain (depth - 1) mid s_out ]
  in
  let start_sig = new_signal () in
  let end_sig = new_signal () in
  let reached_end = ref false in
  let starter () = emit start_sig () in
  let sink () =
    ignore (await_immediate end_sig);
    reached_end := true
  in
  parallel [ chain n start_sig end_sig; sink; starter ];
  if not !reached_end then failwith "propagation chain did not reach end"

let bench_propagation_chains_multi n =
  let rounds = effective_multi_rounds n in
  let n = effective_propagation n in
  let link s_in s_out () =
    ignore (await_immediate s_in);
    emit s_out ()
  in
  let rec chain depth s_in s_out () =
    if depth <= 0 then
      emit s_out ()
    else
      let mid = new_signal () in
      parallel [ link s_in mid; chain (depth - 1) mid s_out ]
  in
  let one_round () =
    let start_sig = new_signal () in
    let end_sig = new_signal () in
    let reached_end = ref false in
    let starter () = emit start_sig () in
    let sink () =
      ignore (await_immediate end_sig);
      reached_end := true
    in
    parallel [ chain n start_sig end_sig; sink; starter ];
    if not !reached_end then failwith "propagation chain did not reach end"
  in
  let rec loop i () =
    if i <= 0 then ()
    else (
      one_round ();
      if i > 1 then (
        pause ();
        loop (i - 1) ()))
  in
  loop rounds ()

let bench_guarded_cascades n =
  let depth = effective_guard_depth n in
  let guards = Array.init depth (fun _ -> new_signal ()) in
  let reached = ref false in
  let rec nested i k =
    if i >= depth then k ()
    else when_ guards.(i) (fun () -> nested (i + 1) k)
  in
  let consumer () = nested 0 (fun () -> reached := true) in
  let producer () =
    for i = 0 to depth - 1 do
      emit guards.(i) ()
    done
  in
  parallel [ consumer; producer ];
  if not !reached then failwith "guarded cascade did not activate reaction"

let bench_guarded_cascades_multi n =
  let rounds = effective_multi_rounds n in
  let depth = effective_guard_depth n in
  let one_round () =
    let guards = Array.init depth (fun _ -> new_signal ()) in
    let reached = ref false in
    let rec nested i k =
      if i >= depth then k ()
      else when_ guards.(i) (fun () -> nested (i + 1) k)
    in
    let consumer () = nested 0 (fun () -> reached := true) in
    let producer () =
      for i = 0 to depth - 1 do
        emit guards.(i) ()
      done
    in
    parallel [ consumer; producer ];
    if not !reached then failwith "guarded cascade did not activate reaction"
  in
  let rec loop i () =
    if i <= 0 then ()
    else (
      one_round ();
      if i > 1 then (
        pause ();
        loop (i - 1) ()))
  in
  loop rounds ()

let bench_broadcast_expansion n =
  let n = effective_broadcast n in
  let s = new_signal () in
  let hits = ref 0 in
  let producer () = emit s () in
  let observers =
    List.init n (fun _ () ->
        ignore (await_immediate s);
        incr hits)
  in
  parallel (producer :: observers);
  if !hits <> n then
    failwith (Printf.sprintf "broadcast mismatch: expected %d got %d" n !hits)

let bench_fork_explosion n =
  let depth = effective_fork_depth n in
  let rec branch d () =
    if d <= 0 then ()
    else (
      pause ();
      parallel [ branch (d - 1); branch (d - 1) ])
  in
  branch depth ()

let bench_nested_preemption n =
  let depth = effective_preemption_depth n in
  let cancel_even = new_signal () in
  let cancel_odd = new_signal () in
  let rec nested level body =
    if level >= depth then body ()
    else
      let cancel = if level mod 2 = 0 then cancel_even else cancel_odd in
      watch cancel (fun () -> nested (level + 1) body)
  in
  let rec worker steps () =
    if steps <= 0 then ()
    else (
      pause ();
      worker (steps - 1) ())
  in
  let completed = ref false in
  let body () =
    nested 0 (worker (depth + 3));
    completed := true
  in
  let driver () =
    pause ();
    emit cancel_even ()
  in
  parallel [ body; driver ];
  if not !completed then failwith "nested preemption did not terminate"

let run_selected bench n =
  match bench with
  | "propagation_chains" -> bench_propagation_chains n
  | "propagation_chains_multi" -> bench_propagation_chains_multi n
  | "broadcast_expansion" -> bench_broadcast_expansion n
  | "fork_explosion" -> bench_fork_explosion n
  | "guarded_cascades" -> bench_guarded_cascades n
  | "guarded_cascades_multi" -> bench_guarded_cascades_multi n
  | "nested_preemption" -> bench_nested_preemption n
  | x -> invalid_arg ("unknown benchmark: " ^ x)

let peak_mb () =
  let st = Gc.stat () in
  (float_of_int st.heap_words *. float_of_int Sys.word_size /. 8.0)
  /. (1024.0 *. 1024.0)

let words_to_mb words =
  (float_of_int words *. float_of_int Sys.word_size /. 8.0) /. (1024.0 *. 1024.0)

let int_of_string_opt (s : string) =
  try Some (int_of_string s) with
  | Failure _ -> None

let current_rss_mb () =
  let pid = Unix.getpid () in
  let cmd = Printf.sprintf "ps -o rss= -p %d" pid in
  let ic = Unix.open_process_in cmd in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.close_process_in ic))
    (fun () ->
      let line =
        match input_line ic with
        | line -> String.trim line
        | exception End_of_file -> ""
      in
      match int_of_string_opt line with
      | Some kb when kb >= 0 -> float_of_int kb /. 1024.0
      | _ -> nan)

let phase_to_string : Tempo.snapshot_phase -> string = function
  | `Before_step -> "before_step"
  | `After_step -> "after_step"
  | `After_finalize -> "after_finalize"
  | `After_rollover -> "after_rollover"

let write_diag_header oc =
  output_string oc
    "timestamp_ms,rss_mb,phase,instant,step,current_q,blocked_q,next_q,tracked_signals,awaiters,guard_waiters,kill_watchers,live_tasks,kill_context_refs,kill_context_nodes,kill_context_max_depth,active_thread_slots,total_active_threads,total_suspended_threads,task_counter,thread_counter,signal_counter,free_task_count,gc_minor_words,gc_promoted_words,gc_major_words,gc_minor_collections,gc_major_collections,gc_heap_words,gc_live_words,gc_free_words,gc_top_heap_words,gc_stack_size,gc_heap_mb,gc_live_mb,gc_top_heap_mb,cum_tasks_created,cum_tasks_disposed,cum_tasks_enqueued_now,cum_tasks_enqueued_next,cum_tasks_blocked,cum_signals_created,cum_signals_tracked,cum_signals_untracked,cum_awaiters_registered,cum_awaiters_resumed,cum_awaiters_pruned,cum_guard_waiter_registrations,cum_guard_waiter_wakeups,cum_kill_watchers_registered,cum_kill_watchers_fired,cum_kill_watchers_pruned\n"

let write_diag_row oc ~timestamp_ms ~rss_mb (snap : Tempo.runtime_snapshot) =
  Printf.fprintf oc
    "%.3f,%.3f,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%.0f,%.0f,%.0f,%d,%d,%d,%d,%d,%d,%d,%.6f,%.6f,%.6f,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n"
    timestamp_ms
    rss_mb
    (phase_to_string snap.phase)
    snap.instant
    snap.step
    snap.current_q
    snap.blocked_q
    snap.next_q
    snap.tracked_signals
    snap.awaiters
    snap.guard_waiters
    snap.kill_watchers
    snap.live_tasks
    snap.kill_context_refs
    snap.kill_context_nodes
    snap.kill_context_max_depth
    snap.active_thread_slots
    snap.total_active_threads
    snap.total_suspended_threads
    snap.task_counter
    snap.thread_counter
    snap.signal_counter
    snap.free_task_count
    snap.gc_minor_words
    snap.gc_promoted_words
    snap.gc_major_words
    snap.gc_minor_collections
    snap.gc_major_collections
    snap.gc_heap_words
    snap.gc_live_words
    snap.gc_free_words
    snap.gc_top_heap_words
    snap.gc_stack_size
    (words_to_mb snap.gc_heap_words)
    (words_to_mb snap.gc_live_words)
    (words_to_mb snap.gc_top_heap_words)
    snap.cum_tasks_created
    snap.cum_tasks_disposed
    snap.cum_tasks_enqueued_now
    snap.cum_tasks_enqueued_next
    snap.cum_tasks_blocked
    snap.cum_signals_created
    snap.cum_signals_tracked
    snap.cum_signals_untracked
    snap.cum_awaiters_registered
    snap.cum_awaiters_resumed
    snap.cum_awaiters_pruned
    snap.cum_guard_waiter_registrations
    snap.cum_guard_waiter_wakeups
    snap.cum_kill_watchers_registered
    snap.cum_kill_watchers_fired
    snap.cum_kill_watchers_pruned

let () =
  parse_args ();
  let context =
    Printf.sprintf "tempo-bench:%s:n=%d:run=%d" !benchmark !size !run_id
  in
  let tracer =
    if !memtrace_file = "" then
      None
    else
      Some
        (Memtrace.start_tracing ~context:(Some context) ~sampling_rate:!memtrace_rate
           ~filename:!memtrace_file)
  in
  let diag_out =
    if !diag_csv = "" then None
    else
      let oc = open_out !diag_csv in
      write_diag_header oc;
      Some oc
  in
  let cleanup () =
    Option.iter close_out_noerr diag_out;
    Option.iter Memtrace.stop_tracing tracer
  in
  Fun.protect ~finally:cleanup (fun () ->
      Gc.full_major ();
      let diag_t0 = Unix.gettimeofday () in
      let on_snapshot =
        match diag_out with
        | None -> None
        | Some oc ->
            Some
              (fun snap ->
                let timestamp_ms = (Unix.gettimeofday () -. diag_t0) *. 1000.0 in
                write_diag_row oc ~timestamp_ms ~rss_mb:(current_rss_mb ()) snap;
                flush oc)
      in
      let instants = instants_for !benchmark !size in
      let t0 = Unix.gettimeofday () in
      let scenario _input _output = run_selected !benchmark !size in
      execute ?on_snapshot scenario;
      let t1 = Unix.gettimeofday () in
      Gc.full_major ();
      if !diag_compact then Gc.compact ();
      let time_ms = (t1 -. t0) *. 1000.0 in
      Printf.printf "tempo,%s,%d,%d,%.3f,%d,%.3f\n%!" !benchmark !size !run_id
        time_ms instants (peak_mb ()))
