type 'a timeline_instant = { instant : int; output : 'a option }
type inspector_snapshot = { instant : int; runnable : int; blocked : int }

let execute_trace ?instants ?input process =
  let outputs = ref [] in
  Tempo_engine.execute ?instants ?input ~output:(fun v -> outputs := v :: !outputs) process;
  List.rev !outputs

let execute_timeline ?instants ?input process =
  let instant = ref 0 in
  let timeline = ref [] in
  Tempo_engine.execute ?instants ?input
    ~output:(fun v ->
      timeline := { instant = !instant; output = Some v } :: !timeline;
      incr instant)
    process;
  List.rev !timeline

let execute_inspect ?instants ?input process =
  let snapshots = ref [] in
  Tempo_engine.execute ?instants ?input
    ~output:(fun _ -> ())
    (fun input output ->
      snapshots := { instant = 0; runnable = 0; blocked = 0 } :: !snapshots;
      process input output);
  List.rev !snapshots
