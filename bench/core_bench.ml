open Tempo

type point = {
  complexity : int;
  total_ms : float;
  per_op_ns : float;
}

type bench = {
  id : string;
  title : string;
  complexities : int list;
  ops_count : int -> float;
  run : int -> unit;
}

type series = {
  name : string;
  color : string;
  pts : (float * float) list;
}

type compare_point = {
  benchmark : string;
  complexity : int;
  baseline_ns : float;
  current_ns : float;
  delta_pct : float;
}

let palette =
  [|
    "#3b82f6";
    "#22c55e";
    "#f59e0b";
    "#ef4444";
    "#8b5cf6";
    "#14b8a6";
    "#e11d48";
    "#0ea5e9";
  |]

let now_ms () = Unix.gettimeofday () *. 1000.0

let rec ensure_dir path =
  if path = "" || path = "." || path = "/" then ()
  else if Sys.file_exists path then ()
  else (
    ensure_dir (Filename.dirname path);
    Unix.mkdir path 0o755)

let with_out path f =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> f oc)

let measure_ms ~warmup ~repeats ~stat f =
  for _ = 1 to warmup do
    f ()
  done;
  let samples = ref [] in
  for _ = 1 to repeats do
    Gc.compact ();
    let t0 = now_ms () in
    f ();
    samples := (now_ms () -. t0) :: !samples
  done;
  let arr = Array.of_list !samples in
  match stat with
  | "mean" ->
      let sum = Array.fold_left ( +. ) 0.0 arr in
      sum /. float_of_int (Array.length arr)
  | "median" ->
      Array.sort Float.compare arr;
      let n = Array.length arr in
      if n mod 2 = 1 then arr.(n / 2) else 0.5 *. (arr.((n / 2) - 1) +. arr.(n / 2))
  | _ -> invalid_arg "--stat must be mean or median"

let run_pause_loop n =
  execute ~instants:(n + 3) (fun _input _output ->
      for _ = 1 to n do
        pause ()
      done)

let run_emit_await n =
  execute ~instants:(n + 4) (fun _input _output ->
      let s = new_signal () in
      parallel
        [
          (fun () ->
            for i = 1 to n do
              emit s i;
              pause ()
            done);
          (fun () ->
            for _ = 1 to n do
              ignore (await s)
            done);
        ])

let run_await_immediate n =
  execute ~instants:(n + 3) (fun _input _output ->
      let s = new_signal () in
      for i = 1 to n do
        parallel
          [
            (fun () -> ignore (await_immediate s));
            (fun () -> emit s i);
          ];
        pause ()
      done)

let run_when_guard n =
  execute ~instants:(n + 4) (fun _input _output ->
      let g = new_signal () in
      let count = State.create 0 in
      parallel
        [
          (fun () ->
            for i = 1 to n do
              if i mod 2 = 0 then emit g ();
              pause ()
            done);
          (fun () ->
            for _ = 1 to n do
              when_ g (fun () -> State.modify count (fun x -> x + 1));
              pause ()
            done);
        ])

let run_watch_preempt n =
  execute ~instants:(n + 12) (fun _input _output ->
      let stop = new_signal () in
      parallel
        [
          (fun () ->
            watch stop (fun () ->
                for _ = 1 to n do
                  pause ()
                done));
          (fun () ->
            for _ = 1 to max 1 (n / 3) do
              pause ()
            done;
            emit stop ();
            pause ());
        ])

let run_parallel_fanout branches =
  let work = 40 in
  execute ~instants:(work + 3) (fun _input _output ->
      let branch () =
        for _ = 1 to work do
          pause ()
        done
      in
      parallel (List.init branches (fun _ -> branch)))

let run_combined_core n =
  execute ~instants:(n + 12) (fun _input _output ->
      let tick = new_signal () in
      let gate = new_signal () in
      let stop = new_signal () in
      let counter = State.create 0 in
      parallel
        [
          (fun () ->
            for i = 1 to n do
              emit tick i;
              if i mod 3 = 0 then emit gate ();
              if i = n then emit stop ();
              pause ()
            done);
          (fun () ->
            watch stop (fun () ->
                for _ = 1 to n do
                  ignore (await tick);
                  pause ()
                done));
          (fun () ->
            for _ = 1 to n do
              when_ gate (fun () -> State.modify counter (fun x -> x + 1));
              pause ()
            done);
        ])

let scale_complexity scale n =
  let v = int_of_float (Float.round (float_of_int n *. scale)) in
  max 1 v

let benches ~scale =
  [
    {
      id = "pause_loop";
      title = "Core pause() loop";
      complexities = List.map (scale_complexity scale) [ 1_000; 5_000; 10_000; 20_000; 40_000 ];
      ops_count = (fun n -> float_of_int n);
      run = run_pause_loop;
    };
    {
      id = "emit_await";
      title = "emit + await (event signal)";
      complexities = List.map (scale_complexity scale) [ 500; 1_000; 2_000; 4_000; 8_000 ];
      ops_count = (fun n -> float_of_int (2 * n));
      run = run_emit_await;
    };
    {
      id = "await_immediate";
      title = "await_immediate handshake";
      complexities = List.map (scale_complexity scale) [ 200; 500; 1_000; 2_000; 4_000 ];
      ops_count = (fun n -> float_of_int n);
      run = run_await_immediate;
    };
    {
      id = "when_guard";
      title = "when_ guard cost";
      complexities = List.map (scale_complexity scale) [ 500; 1_000; 2_000; 4_000; 8_000 ];
      ops_count = (fun n -> float_of_int n);
      run = run_when_guard;
    };
    {
      id = "watch_preempt";
      title = "watch preemption cost";
      complexities = List.map (scale_complexity scale) [ 300; 700; 1_500; 3_000; 6_000 ];
      ops_count = (fun n -> float_of_int n);
      run = run_watch_preempt;
    };
    {
      id = "parallel_fanout";
      title = "parallel fanout branches";
      complexities = List.map (scale_complexity scale) [ 8; 16; 32; 64; 96; 128 ];
      ops_count = (fun n -> float_of_int (n * 40));
      run = run_parallel_fanout;
    };
    {
      id = "combined_core";
      title = "combined core operators";
      complexities = List.map (scale_complexity scale) [ 300; 700; 1_500; 3_000; 6_000 ];
      ops_count = (fun n -> float_of_int (4 * n));
      run = run_combined_core;
    };
  ]

let escape_xml s =
  let b = Buffer.create (String.length s + 16) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&apos;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let log10 x = log x /. log 10.0

let svg_line_chart ~log_y ~title ~x_label ~y_label ~series ~path =
  let width = 980.0 in
  let height = 580.0 in
  let left = 90.0 in
  let top = 70.0 in
  let right = 28.0 in
  let bottom = 80.0 in
  let plot_w = width -. left -. right in
  let plot_h = height -. top -. bottom in
  let xs = List.concat_map (fun s -> List.map fst s.pts) series in
  let to_view y = if log_y then log10 (max 1e-9 y) else y in
  let from_view y = if log_y then 10.0 ** y else y in
  let ys = List.concat_map (fun s -> List.map snd s.pts) series in
  let ys_view = List.map to_view ys in
  let x_min = List.fold_left min max_float xs in
  let x_max = List.fold_left max min_float xs in
  let y_min_raw = List.fold_left min max_float ys_view in
  let y_max_raw = List.fold_left max min_float ys_view in
  let y_min = if log_y then y_min_raw else min 0.0 y_min_raw in
  let y_max = if y_max_raw <= y_min then y_min +. 1.0 else y_max_raw +. (0.08 *. (y_max_raw -. y_min +. 1e-9)) in
  let map_x x =
    if x_max = x_min then left +. (plot_w /. 2.0)
    else left +. (((x -. x_min) /. (x_max -. x_min)) *. plot_w)
  in
  let map_y_view y =
    let v = (y -. y_min) /. (y_max -. y_min) in
    top +. ((1.0 -. v) *. plot_h)
  in
  with_out path (fun oc ->
      Printf.fprintf oc
        "<svg xmlns='http://www.w3.org/2000/svg' width='%.0f' height='%.0f' viewBox='0 0 %.0f %.0f' \
         style='background:#0b1220;font-family:Menlo,Consolas,monospace'>\n"
        width height width height;
      Printf.fprintf oc "<text x='%.0f' y='36' fill='#e2e8f0' font-size='24'>%s</text>\n" left
        (escape_xml title);
      Printf.fprintf oc "<rect x='%.2f' y='%.2f' width='%.2f' height='%.2f' fill='#111827' stroke='#334155'/>\n"
        left top plot_w plot_h;
      for i = 0 to 6 do
        let t = float_of_int i /. 6.0 in
        let y = top +. (t *. plot_h) in
        let yv = y_max -. (t *. (y_max -. y_min)) in
        Printf.fprintf oc "<line x1='%.2f' y1='%.2f' x2='%.2f' y2='%.2f' stroke='#1f2937'/>\n" left y
          (left +. plot_w) y;
        Printf.fprintf oc "<text x='12' y='%.2f' fill='#94a3b8' font-size='12'>%.3g</text>\n" (y +. 4.0)
          (from_view yv)
      done;
      for i = 0 to 6 do
        let t = float_of_int i /. 6.0 in
        let x = left +. (t *. plot_w) in
        let xv = x_min +. (t *. (x_max -. x_min)) in
        Printf.fprintf oc "<line x1='%.2f' y1='%.2f' x2='%.2f' y2='%.2f' stroke='#1f2937'/>\n" x top x
          (top +. plot_h);
        Printf.fprintf oc "<text x='%.2f' y='%.2f' fill='#94a3b8' font-size='12'>%.0f</text>\n" (x -. 10.0)
          (top +. plot_h +. 18.0) xv
      done;
      List.iter
        (fun s ->
          let d =
            String.concat " "
              (List.mapi
                 (fun i (x, y) ->
                   let px = map_x x in
                   let py = map_y_view (to_view y) in
                   if i = 0 then Printf.sprintf "M %.2f %.2f" px py
                   else Printf.sprintf "L %.2f %.2f" px py)
                 s.pts)
          in
          Printf.fprintf oc "<path d='%s' fill='none' stroke='%s' stroke-width='2.4'/>\n" d s.color;
          List.iter
            (fun (x, y) ->
              Printf.fprintf oc "<circle cx='%.2f' cy='%.2f' r='3.2' fill='%s'/>\n" (map_x x)
                (map_y_view (to_view y))
                s.color)
            s.pts)
        series;
      Printf.fprintf oc "<text x='%.2f' y='%.2f' fill='#cbd5e1' font-size='14'>%s</text>\n"
        (left +. (plot_w /. 2.0) -. 48.0)
        (height -. 24.0) (escape_xml x_label);
      Printf.fprintf oc
        "<text x='22' y='%.2f' fill='#cbd5e1' font-size='14' transform='rotate(-90 22 %.2f)'>%s</text>\n"
        (top +. (plot_h /. 2.0)) (top +. (plot_h /. 2.0)) (escape_xml y_label);
      let legend_x = left +. plot_w -. 230.0 in
      let legend_y = top +. 18.0 in
      List.iteri
        (fun i s ->
          let y = legend_y +. (float_of_int i *. 22.0) in
          Printf.fprintf oc "<line x1='%.2f' y1='%.2f' x2='%.2f' y2='%.2f' stroke='%s' stroke-width='3'/>\n"
            legend_x y (legend_x +. 22.0) y s.color;
          Printf.fprintf oc "<text x='%.2f' y='%.2f' fill='#e2e8f0' font-size='13'>%s</text>\n"
            (legend_x +. 28.0) (y +. 4.0) (escape_xml s.name))
        series;
      output_string oc "</svg>\n")

let write_csv path rows =
  with_out path (fun oc ->
      output_string oc "benchmark,complexity,total_ms,per_op_ns\n";
      List.iter
        (fun (id, (p : point)) ->
          Printf.fprintf oc "%s,%d,%.6f,%.3f\n" id p.complexity p.total_ms p.per_op_ns)
        rows)

let write_report path runs =
  with_out path (fun oc ->
      output_string oc "# Tempo Core Bench Report\n\n";
      output_string oc
        "Each benchmark is measured with warmup/repeated runs and exported as CSV/SVG.\n\n";
      List.iter
        (fun (b, pts) ->
          let worst =
            List.fold_left (fun acc p -> max acc p.total_ms) 0.0 pts
          in
          let best_ns =
            List.fold_left (fun acc p -> min acc p.per_op_ns) max_float pts
          in
          Printf.fprintf oc "## %s (`%s`)\n\n" b.title b.id;
          Printf.fprintf oc "- max total time: %.3f ms\n" worst;
          Printf.fprintf oc "- best normalized cost: %.3f ns/op\n" best_ns;
          Printf.fprintf oc "- chart: `%s_total_ms.svg`\n\n" b.id)
        runs;
      output_string oc "## Combined chart\n\n";
      output_string oc "- `core_per_op_ns.svg`\n")

let load_baseline_csv path =
  let tbl = Hashtbl.create 128 in
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let _header = input_line ic in
      (try
         while true do
           let line = String.trim (input_line ic) in
           if line <> "" then
             match String.split_on_char ',' line with
             | [ bench; c; _ms; ns ] ->
                 Hashtbl.replace tbl (bench, int_of_string c) (float_of_string ns)
             | _ -> ()
         done
       with End_of_file -> ());
      tbl)

let compare_with_baseline ~baseline_tbl ~(runs : (bench * point list) list) =
  let points = ref [] in
  List.iter
    (fun (b, pts) ->
      List.iter
        (fun (p : point) ->
          match Hashtbl.find_opt baseline_tbl (b.id, p.complexity) with
          | None -> ()
          | Some base ->
              let delta = ((p.per_op_ns -. base) /. max base 1e-9) *. 100.0 in
              points :=
                {
                  benchmark = b.id;
                  complexity = p.complexity;
                  baseline_ns = base;
                  current_ns = p.per_op_ns;
                  delta_pct = delta;
                }
                :: !points)
        pts)
    runs;
  List.rev !points

let write_compare_csv path points =
  with_out path (fun oc ->
      output_string oc "benchmark,complexity,baseline_per_op_ns,current_per_op_ns,delta_pct\n";
      List.iter
        (fun p ->
          Printf.fprintf oc "%s,%d,%.3f,%.3f,%.3f\n" p.benchmark p.complexity p.baseline_ns
            p.current_ns p.delta_pct)
        points)

let write_compare_report path points =
  let max_reg =
    List.fold_left
      (fun acc p -> if p.delta_pct > acc.delta_pct then p else acc)
      {
        benchmark = "n/a";
        complexity = 0;
        baseline_ns = 0.0;
        current_ns = 0.0;
        delta_pct = neg_infinity;
      }
      points
  in
  let max_gain =
    List.fold_left
      (fun acc p -> if p.delta_pct < acc.delta_pct then p else acc)
      {
        benchmark = "n/a";
        complexity = 0;
        baseline_ns = 0.0;
        current_ns = 0.0;
        delta_pct = infinity;
      }
      points
  in
  with_out path (fun oc ->
      output_string oc "# Core Bench Baseline Comparison\n\n";
      if points = [] then output_string oc "No matching points between current run and baseline.\n"
      else (
        Printf.fprintf oc "- compared points: %d\n" (List.length points);
        Printf.fprintf oc "- worst regression: `%s` complexity `%d` delta `%.2f%%`\n"
          max_reg.benchmark max_reg.complexity max_reg.delta_pct;
        Printf.fprintf oc "- best improvement: `%s` complexity `%d` delta `%.2f%%`\n\n"
          max_gain.benchmark max_gain.complexity max_gain.delta_pct;
        output_string oc
          "| benchmark | complexity | baseline ns/op | current ns/op | delta % |\n|---|---:|---:|---:|---:|\n";
        List.iter
          (fun p ->
            Printf.fprintf oc "| %s | %d | %.3f | %.3f | %.3f |\n" p.benchmark p.complexity
              p.baseline_ns p.current_ns p.delta_pct)
          points))

let parse_sweep s =
  s
  |> String.split_on_char ','
  |> List.filter (fun x -> String.trim x <> "")
  |> List.map (fun x -> float_of_string (String.trim x))

let scale_label x =
  let s = Printf.sprintf "%.3g" x in
  String.map (function '.' -> '_' | c -> c) s

let run_suite ~out_dir ~repeats ~warmup ~stat ~scale ~only ~log_y ~baseline_csv ~fail_on_regression_pct =
  ensure_dir out_dir;
  let selected =
    benches ~scale
    |> List.filter (fun b -> only = "" || b.id = only)
  in
  if selected = [] then invalid_arg "No benchmark selected";
  Printf.printf "Running %d core benchmarks (warmup=%d repeats=%d scale=%.2f)\n%!"
    (List.length selected) warmup repeats scale;
  let runs =
    List.map
      (fun b ->
        Printf.printf "  - %s\n%!" b.id;
        let pts =
          List.map
            (fun n ->
              let ms = measure_ms ~warmup ~repeats ~stat (fun () -> b.run n) in
              let per_op_ns = (ms *. 1_000_000.0) /. max 1.0 (b.ops_count n) in
              { complexity = n; total_ms = ms; per_op_ns })
            b.complexities
        in
        (b, pts))
      selected
  in
  let csv_rows =
    List.concat_map
      (fun (b, pts) -> List.map (fun p -> (b.id, p)) pts)
      runs
  in
  write_csv (Filename.concat out_dir "core_bench.csv") csv_rows;
  List.iteri
    (fun i (b, pts) ->
      let c = palette.(i mod Array.length palette) in
      svg_line_chart
        ~log_y:false
        ~title:(Printf.sprintf "%s — total time vs complexity" b.title)
        ~x_label:"complexity"
        ~y_label:"total time (ms)"
        ~series:
          [
            {
              name = b.id;
              color = c;
              pts = List.map (fun (p : point) -> (float_of_int p.complexity, p.total_ms)) pts;
            };
          ]
        ~path:(Filename.concat out_dir (b.id ^ "_total_ms.svg"));
      if log_y then
        svg_line_chart ~log_y:true
          ~title:(Printf.sprintf "%s — total time vs complexity (log Y)" b.title)
          ~x_label:"complexity"
          ~y_label:"total time (ms, log)"
          ~series:
            [
              {
                name = b.id;
                color = c;
                pts = List.map (fun (p : point) -> (float_of_int p.complexity, p.total_ms)) pts;
              };
            ]
          ~path:(Filename.concat out_dir (b.id ^ "_total_ms_log.svg")))
    runs;
  let per_op_series =
    List.mapi
      (fun i (b, pts) ->
        {
          name = b.id;
          color = palette.(i mod Array.length palette);
          pts = List.map (fun (p : point) -> (float_of_int p.complexity, p.per_op_ns)) pts;
        })
      runs
  in
  svg_line_chart ~log_y:false ~title:"Tempo Core Bench — normalized cost"
    ~x_label:"complexity"
    ~y_label:"ns/op"
    ~series:per_op_series
    ~path:(Filename.concat out_dir "core_per_op_ns.svg");
  if log_y then
    svg_line_chart ~log_y:true ~title:"Tempo Core Bench — normalized cost (log Y)"
      ~x_label:"complexity"
      ~y_label:"ns/op (log)"
      ~series:per_op_series
      ~path:(Filename.concat out_dir "core_per_op_ns_log.svg");
  write_report (Filename.concat out_dir "REPORT.md") runs;
  (match baseline_csv with
  | None -> ()
  | Some path ->
      let baseline_tbl = load_baseline_csv path in
      let points = compare_with_baseline ~baseline_tbl ~runs in
      write_compare_csv (Filename.concat out_dir "COMPARE.csv") points;
      write_compare_report (Filename.concat out_dir "COMPARE.md") points;
      (match fail_on_regression_pct with
      | None -> ()
      | Some limit ->
          let worst =
            List.fold_left (fun acc p -> max acc p.delta_pct) neg_infinity points
          in
          if worst > limit then (
            Printf.eprintf
              "Regression threshold exceeded: worst delta %.2f%% > allowed %.2f%%\n%!" worst
              limit;
            exit 3)));
  Printf.printf "Bench outputs written to %s\n%!" out_dir

let () =
  let out_dir = ref "bench/results/core" in
  let repeats = ref 5 in
  let warmup = ref 1 in
  let scale = ref 1.0 in
  let only = ref "" in
  let log_y = ref false in
  let sweep = ref "" in
  let stat = ref "median" in
  let baseline_csv = ref "" in
  let fail_on_regression_pct = ref "" in
  let specs =
    [
      ("--out-dir", Arg.Set_string out_dir, "Output directory for CSV/SVG/report");
      ("--repeats", Arg.Set_int repeats, "Repeated measures per point (default: 5)");
      ("--warmup", Arg.Set_int warmup, "Warmup runs per point (default: 1)");
      ("--scale", Arg.Set_float scale, "Complexity multiplier (default: 1.0)");
      ("--only", Arg.Set_string only, "Run only one benchmark id");
      ("--log-y", Arg.Set log_y, "Generate additional SVG charts with logarithmic Y scale");
      ("--sweep", Arg.Set_string sweep, "Comma-separated scales, e.g. 0.5,1,2");
      ("--stat", Arg.Set_string stat, "Statistic for repeated samples: mean|median (default: median)");
      ("--baseline-csv", Arg.Set_string baseline_csv, "Compare against a previous core_bench.csv");
      ("--fail-on-regression-pct", Arg.Set_string fail_on_regression_pct, "Fail if any matched point regresses above this percent");
    ]
  in
  Arg.parse specs (fun _ -> ()) "core_bench options:";
  if !repeats < 1 then invalid_arg "--repeats must be >= 1";
  if !warmup < 0 then invalid_arg "--warmup must be >= 0";
  let baseline_csv_opt = if !baseline_csv = "" then None else Some !baseline_csv in
  let fail_opt =
    if !fail_on_regression_pct = "" then None
    else Some (float_of_string !fail_on_regression_pct)
  in
  if !sweep = "" then (
    if !scale <= 0.0 then invalid_arg "--scale must be > 0";
    run_suite ~out_dir:!out_dir ~repeats:!repeats ~warmup:!warmup ~stat:!stat ~scale:!scale ~only:!only
      ~log_y:!log_y ~baseline_csv:baseline_csv_opt ~fail_on_regression_pct:fail_opt)
  else (
    let scales = parse_sweep !sweep in
    if scales = [] then invalid_arg "--sweep is empty";
    List.iter (fun s -> if s <= 0.0 then invalid_arg "--sweep scales must be > 0") scales;
    ensure_dir !out_dir;
    with_out (Filename.concat !out_dir "SWEEP.md") (fun oc ->
        output_string oc "# Core Bench Sweep\n\n";
        List.iter
          (fun s ->
            let sub = Filename.concat !out_dir ("scale_" ^ scale_label s) in
            run_suite ~out_dir:sub ~repeats:!repeats ~warmup:!warmup ~stat:!stat ~scale:s ~only:!only ~log_y:!log_y
              ~baseline_csv:baseline_csv_opt ~fail_on_regression_pct:fail_opt;
            Printf.fprintf oc "- scale %.3g: `%s`\n" s sub)
          scales))
