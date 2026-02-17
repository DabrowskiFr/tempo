let output_dir =
  match Sys.getenv_opt "TEMPO_BOOK_TRACES_OUT_DIR" with
  | Some p when String.length p > 0 -> p
  | _ -> "book/generated/traces"

let rec ensure_dir path =
  if Sys.file_exists path then ()
  else
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    Unix.mkdir path 0o755

let escape_tex s =
  let b = Buffer.create (String.length s + 16) in
  String.iter
    (function
      | '\\' -> Buffer.add_string b "\\textbackslash{}"
      | '{' -> Buffer.add_string b "\\{"
      | '}' -> Buffer.add_string b "\\}"
      | '_' -> Buffer.add_string b "\\_"
      | '%' -> Buffer.add_string b "\\%"
      | '&' -> Buffer.add_string b "\\&"
      | '#' -> Buffer.add_string b "\\#"
      | '$' -> Buffer.add_string b "\\$"
      | '^' -> Buffer.add_string b "\\^{}"
      | '~' -> Buffer.add_string b "\\~{}"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let truncate_rows rows ~max_instants =
  let n = List.length rows in
  if n <= max_instants then (rows, 0)
  else
    let head_n = min 3 n in
    let tail_n = min 2 (n - head_n) in
    let rec take k xs =
      match (k, xs) with
      | 0, _ | _, [] -> []
      | k, x :: tl -> x :: take (k - 1) tl
    in
    let rec drop k xs =
      match (k, xs) with
      | 0, _ | _, [] -> xs
      | k, _ :: tl -> drop (k - 1) tl
    in
    let head = take head_n rows in
    let tail = drop (n - tail_n) rows in
    (head @ tail, n - (head_n + tail_n))

let write_trace_file ~name ~trace_max rows =
  ensure_dir output_dir;
  let out_path = Filename.concat output_dir (name ^ ".tex") in
  let rows', omitted = truncate_rows rows ~max_instants:trace_max in
  let oc = open_out out_path in
  output_string oc "\\begin{tcolorbox}[tempoBox,title=Execution Trace]\n";
  output_string oc "\\begin{tabular}{r l l}\n";
  output_string oc "\\toprule\n";
  output_string oc "t & input & output \\\\\n";
  output_string oc "\\midrule\n";
  List.iter
    (fun (t, i, o) ->
      Printf.fprintf oc "%d & %s & %s \\\\\n" t (escape_tex i) (escape_tex o))
    rows';
  if omitted > 0 then
    Printf.fprintf oc
      "\\multicolumn{3}{l}{\\emph{... %d instants omitted ...}} \\\\\n"
      omitted;
  output_string oc "\\bottomrule\n";
  output_string oc "\\end{tabular}\n";
  output_string oc "\\end{tcolorbox}\n";
  close_out oc

let () =
  write_trace_file ~name:Emit_await_example.name
    ~trace_max:Emit_await_example.trace_max
    (Emit_await_example.trace_rows ());
  write_trace_file ~name:Await_immediate_example.name
    ~trace_max:Await_immediate_example.trace_max
    (Await_immediate_example.trace_rows ());
  write_trace_file ~name:When_watch_parallel_example.name
    ~trace_max:When_watch_parallel_example.trace_max
    (When_watch_parallel_example.trace_rows ());
  write_trace_file ~name:Execute_trace_example.name
    ~trace_max:Execute_trace_example.trace_max
    (Execute_trace_example.trace_rows ())
