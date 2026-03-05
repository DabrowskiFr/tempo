type 'a timeline_instant = { instant : int; output : 'a option }
type inspector_snapshot = { instant : int; runnable : int; blocked : int }

let execute_trace ?instants ?input process =
  let outputs = ref [] in
  Tempo_engine.execute ?instants ?input ~output:(fun v -> outputs := v :: !outputs) process;
  List.rev !outputs

let execute_timeline ?instants ?input process =
  let current_instant = ref 0 in
  let outputs = Hashtbl.create 16 in
  let input_wrapper =
    match input with
    | None ->
        (fun () ->
          let i = !current_instant in
          current_instant := i + 1;
          None)
    | Some f ->
        (fun () ->
          let i = !current_instant in
          current_instant := i + 1;
          f ())
  in
  Tempo_engine.execute ?instants ~input:input_wrapper
    ~output:(fun v -> Hashtbl.replace outputs (!current_instant - 1) v)
    process;
  let total_instants = match instants with Some n -> n | None -> !current_instant in
  List.init total_instants (fun instant ->
      { instant; output = Hashtbl.find_opt outputs instant })

let execute_inspect ?instants ?input process =
  let snapshots = ref [] in
  Tempo_engine.execute ?instants ?input
    ~output:(fun _ -> ())
    (fun input output ->
      snapshots := { instant = 0; runnable = 0; blocked = 0 } :: !snapshots;
      process input output);
  List.rev !snapshots
