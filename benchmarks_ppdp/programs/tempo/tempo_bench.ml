open Tempo

let benchmark = ref "propagation_chains"
let size = ref 10
let run_id = ref 1

let parse_args () =
  let specs =
    [
      ("--benchmark", Arg.Set_string benchmark, "Benchmark name");
      ("--size", Arg.Set_int size, "Benchmark size parameter");
      ("--run", Arg.Set_int run_id, "Run index");
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

let bench_propagation_chains n =
  let n = effective_propagation n in
  let signals = Array.init (n + 1) (fun _ -> new_signal ()) in
  let reached_end = ref false in
  let starter () = emit signals.(0) () in
  let sink () =
    ignore (await_immediate signals.(n));
    reached_end := true
  in
  let workers =
    List.init n (fun i () ->
        ignore (await_immediate signals.(i));
        emit signals.(i + 1) ())
  in
  parallel (starter :: sink :: workers);
  if not !reached_end then failwith "propagation chain did not reach end"

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
  | "broadcast_expansion" -> bench_broadcast_expansion n
  | "fork_explosion" -> bench_fork_explosion n
  | "guarded_cascades" -> bench_guarded_cascades n
  | "nested_preemption" -> bench_nested_preemption n
  | x -> invalid_arg ("unknown benchmark: " ^ x)

let peak_mb () =
  let st = Gc.stat () in
  (float_of_int st.heap_words *. float_of_int Sys.word_size /. 8.0)
  /. (1024.0 *. 1024.0)

let () =
  parse_args ();
  Gc.full_major ();
  let instants = ref 0 in
  let t0 = Unix.gettimeofday () in
  let scenario _input _output = run_selected !benchmark !size in
  execute
    ~input:(fun () ->
      incr instants;
      None)
    scenario;
  let t1 = Unix.gettimeofday () in
  Gc.full_major ();
  let time_ms = (t1 -. t0) *. 1000.0 in
  Printf.printf "tempo,%s,%d,%d,%.3f,%d,%.3f\n%!" !benchmark !size !run_id
    time_ms !instants (peak_mb ())
