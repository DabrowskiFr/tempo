open Tempo
open Raylib

type ext_input =
  | In_a
  | In_b

type signal_name =
  | Sig_a
  | Sig_b

type block =
  | Emit of signal_name
  | Await of signal_name
  | Await_imm of signal_name
  | Pause_block
  | When_emit of signal_name * signal_name
  | Watch_await_emit of signal_name * signal_name * signal_name
  | Parallel_emit of signal_name * signal_name

let signal_name_to_string = function
  | Sig_a -> "A"
  | Sig_b -> "B"

let block_label = function
  | Emit s -> Printf.sprintf "emit %s" (signal_name_to_string s)
  | Await s -> Printf.sprintf "await %s" (signal_name_to_string s)
  | Await_imm s -> Printf.sprintf "await_immediate %s" (signal_name_to_string s)
  | Pause_block -> "pause"
  | When_emit (g, t) ->
      Printf.sprintf "when %s -> emit %s" (signal_name_to_string g) (signal_name_to_string t)
  | Watch_await_emit (w, a, t) ->
      Printf.sprintf "watch %s { await %s; emit %s }"
        (signal_name_to_string w)
        (signal_name_to_string a)
        (signal_name_to_string t)
  | Parallel_emit (a, b) ->
      Printf.sprintf "parallel { emit %s || emit %s }"
        (signal_name_to_string a)
        (signal_name_to_string b)

type timeline_row = {
  instant : int;
  input : ext_input option;
  output : string option;
}

let ext_input_to_string = function
  | None -> "-"
  | Some In_a -> "A"
  | Some In_b -> "B"

let cycle_input = function
  | None -> Some In_a
  | Some In_a -> Some In_b
  | Some In_b -> None

let signal_of_name sa sb = function
  | Sig_a -> sa
  | Sig_b -> sb

let simulate ~(blocks : block list) ~(inputs : ext_input option list) ~(instants : int) : timeline_row list =
  let run input output =
    let sa = new_signal () in
    let sb = new_signal () in
    let trace = new_signal_agg ~initial:[] ~combine:(fun acc msg -> msg :: acc) in
    let rec input_pump () =
      when_ input (fun () ->
          match await_immediate input with
          | In_a ->
              if not (is_present sa) then emit sa ();
              emit trace "input A"
          | In_b ->
              if not (is_present sb) then emit sb ();
              emit trace "input B");
      pause ();
      input_pump ()
    in
    let emit_once sigv name =
      if is_present sigv then
        emit trace (Printf.sprintf "emit %s skipped (already present this instant)" name)
      else (
        emit sigv ();
        emit trace (Printf.sprintf "emit %s" name))
    in
    let rec eval_block = function
      | Emit s ->
          let sigv = signal_of_name sa sb s in
          emit_once sigv (signal_name_to_string s)
      | Await s ->
          let sigv = signal_of_name sa sb s in
          let () = await sigv in
          emit trace (Printf.sprintf "await %s satisfied" (signal_name_to_string s))
      | Await_imm s ->
          let sigv = signal_of_name sa sb s in
          let () = await_immediate sigv in
          emit trace (Printf.sprintf "await_immediate %s satisfied" (signal_name_to_string s))
      | Pause_block ->
          emit trace "pause";
          pause ()
      | When_emit (guard_s, target_s) ->
          let guard = signal_of_name sa sb guard_s in
          let target = signal_of_name sa sb target_s in
          when_ guard (fun () ->
              emit trace
                (Printf.sprintf "when %s active" (signal_name_to_string guard_s));
              emit_once target (signal_name_to_string target_s))
      | Watch_await_emit (watch_s, await_s, target_s) ->
          let watched = signal_of_name sa sb watch_s in
          let awaited = signal_of_name sa sb await_s in
          let target = signal_of_name sa sb target_s in
          watch watched (fun () ->
              let () = await awaited in
              emit trace
                (Printf.sprintf "watch body passed: %s then emit %s"
                   (signal_name_to_string await_s)
                   (signal_name_to_string target_s));
              emit_once target (signal_name_to_string target_s))
      | Parallel_emit (left_s, right_s) ->
          let left_sig = signal_of_name sa sb left_s in
          let right_sig = signal_of_name sa sb right_s in
          parallel
            [ (fun () ->
                emit trace (Printf.sprintf "parallel-left %s" (signal_name_to_string left_s));
                emit_once left_sig (signal_name_to_string left_s))
            ; (fun () ->
                emit trace (Printf.sprintf "parallel-right %s" (signal_name_to_string right_s));
                emit_once right_sig (signal_name_to_string right_s))
            ]
    and eval_blocks = function
      | [] -> emit trace "program end"
      | b :: rest ->
          eval_block b;
          eval_blocks rest
    in
    let rec flush_trace () =
      let msgs = await trace in
      emit output (String.concat " | " (List.rev msgs));
      flush_trace ()
    in
    parallel [ input_pump; (fun () -> eval_blocks blocks); flush_trace ]
  in
  let timeline = execute_timeline ~instants ~inputs run in
  List.map
    (fun ({ instant; input; output } : (ext_input, string) timeline_instant) ->
      { instant; input; output })
    timeline

let parse_args () =
  let headless = ref false in
  let instants = ref 16 in
  let specs =
    [ ( "--headless",
        Arg.Set headless,
        "Run one deterministic scenario and print the timeline" )
    ; ("--instants", Arg.Set_int instants, "Number of logical instants (default: 16)")
    ]
  in
  Arg.parse specs (fun _ -> ()) "tempo-core-studio";
  (!headless, max 4 !instants)

let run_headless instants =
  let blocks =
    [ Emit Sig_a
    ; Pause_block
    ; Await Sig_b
    ; Parallel_emit (Sig_a, Sig_b)
    ; Watch_await_emit (Sig_b, Sig_a, Sig_b)
    ]
  in
  let inputs =
    [ Some In_b
    ; None
    ; Some In_b
    ; Some In_a
    ; None
    ; Some In_b
    ; None
    ; None
    ]
  in
  let rows = simulate ~blocks ~inputs ~instants in
  List.iter
    (fun r ->
      Printf.printf "t=%02d in=%s out=%s\n"
        r.instant
        (ext_input_to_string r.input)
        (match r.output with None -> "-" | Some s -> s))
    rows

let point_in_rect x y rx ry rw rh =
  x >= rx && x <= rx + rw && y >= ry && y <= ry + rh

let draw_button x y w h label active =
  let rect = Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h) in
  let bg = if active then Color.create 65 150 210 255 else Color.create 38 62 88 255 in
  draw_rectangle_rec rect bg;
  draw_rectangle_lines_ex rect 2.0 (Color.create 195 220 245 255);
  draw_text label (x + 10) (y + 9) 18 Color.raywhite

let () =
  let headless, instants = parse_args () in
  if headless then run_headless instants
  else (
    init_window 1400 900 "Tempo Core Studio";
    set_target_fps 60;

    let palette : (string * block) list =
      [ ("emit A", Emit Sig_a)
      ; ("emit B", Emit Sig_b)
      ; ("await A", Await Sig_a)
      ; ("await_immediate A", Await_imm Sig_a)
      ; ("pause", Pause_block)
      ; ("when A -> emit B", When_emit (Sig_a, Sig_b))
      ; ("watch B {await A; emit B}", Watch_await_emit (Sig_b, Sig_a, Sig_b))
      ; ("parallel {emit A || emit B}", Parallel_emit (Sig_a, Sig_b))
      ]
    in

    let script = ref [ Emit Sig_a; Pause_block; Await Sig_b ] in
    let input_cells = Array.make instants None in
    let results = ref [] in

    let run_simulation () =
      let inputs = Array.to_list input_cells in
      results := simulate ~blocks:!script ~inputs ~instants
    in

    run_simulation ();

    while not (window_should_close ()) do
      let mouse_x = get_mouse_x () in
      let mouse_y = get_mouse_y () in
      let click = is_mouse_button_pressed MouseButton.Left in

      begin_drawing ();
      clear_background (Color.create 20 31 48 255);

      draw_text "Tempo Core Studio" 24 16 36 (Color.create 235 240 255 255);
      draw_text
        "Scratch-like playground focused on Tempo Core primitives: emit, await, await_immediate, pause, when, watch, parallel"
        24 58 18 (Color.create 176 198 224 255);

      let palette_x = 24 in
      let palette_y = 100 in
      let palette_w = 360 in
      draw_rectangle palette_x palette_y palette_w 760 (Color.create 24 44 69 255);
      draw_rectangle_lines palette_x palette_y palette_w 760 (Color.create 105 145 187 255);
      draw_text "Palette (click to append block)" (palette_x + 14) (palette_y + 10) 20 Color.raywhite;

      List.iteri
        (fun i (label, block) ->
          let y = palette_y + 50 + (i * 54) in
          let x = palette_x + 12 in
          let w = 336 in
          let h = 44 in
          draw_button x y w h label false;
          if click && point_in_rect mouse_x mouse_y x y w h then script := !script @ [ block ])
        palette;

      let script_x = 410 in
      let script_y = 100 in
      let script_w = 620 in
      draw_rectangle script_x script_y script_w 540 (Color.create 26 47 73 255);
      draw_rectangle_lines script_x script_y script_w 540 (Color.create 105 145 187 255);
      draw_text "Program Blocks (Tempo Core sequence)" (script_x + 14) (script_y + 10) 20 Color.raywhite;

      List.iteri
        (fun i b ->
          let y = script_y + 48 + (i * 46) in
          let row = Rectangle.create (float_of_int (script_x + 12)) (float_of_int y) 590.0 36.0 in
          draw_rectangle_rec row (Color.create 45 80 120 255);
          draw_rectangle_lines_ex row 1.5 (Color.create 165 205 240 255);
          draw_text (Printf.sprintf "%02d" (i + 1)) (script_x + 20) (y + 9) 18 (Color.create 255 220 130 255);
          draw_text (block_label b) (script_x + 58) (y + 9) 18 Color.raywhite;
          let rx = script_x + 560 in
          let rw = 42 in
          let rh = 36 in
          draw_button rx y rw rh "X" false;
          if click && point_in_rect mouse_x mouse_y rx y rw rh then
            script := List.filteri (fun j _ -> j <> i) !script)
        !script;

      let panel_x = 1050 in
      let panel_y = 100 in
      draw_rectangle panel_x panel_y 326 540 (Color.create 24 44 69 255);
      draw_rectangle_lines panel_x panel_y 326 540 (Color.create 105 145 187 255);
      draw_text "Actions" (panel_x + 14) (panel_y + 10) 20 Color.raywhite;

      let bx = panel_x + 16 in
      let bw = 292 in
      let run_y = panel_y + 54 in
      let clear_script_y = panel_y + 112 in
      let clear_inputs_y = panel_y + 164 in
      let sample_y = panel_y + 216 in
      draw_button bx run_y bw 48 "Run Simulation" true;
      draw_button bx clear_script_y bw 44 "Clear Program" false;
      draw_button bx clear_inputs_y bw 44 "Clear Inputs" false;
      draw_button bx sample_y bw 44 "Load Sample Program" false;

      if click && point_in_rect mouse_x mouse_y bx run_y bw 48 then run_simulation ();
      if click && point_in_rect mouse_x mouse_y bx clear_script_y bw 44 then script := [];
      if click && point_in_rect mouse_x mouse_y bx clear_inputs_y bw 44 then Array.fill input_cells 0 instants None;
      if click && point_in_rect mouse_x mouse_y bx sample_y bw 44 then
        script :=
          [ Emit Sig_a
          ; Pause_block
          ; Await Sig_b
          ; When_emit (Sig_a, Sig_b)
          ; Parallel_emit (Sig_a, Sig_b)
          ; Watch_await_emit (Sig_b, Sig_a, Sig_b)
          ];

      draw_text "Tick input editor (click cell: - -> A -> B)" 24 655 20 Color.raywhite;
      for i = 0 to instants - 1 do
        let x = 24 + (i * 84) in
        let y = 686 in
        let w = 78 in
        let h = 44 in
        let rect = Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h) in
        let cell = input_cells.(i) in
        let bg =
          match cell with
          | None -> Color.create 42 58 78 255
          | Some In_a -> Color.create 167 92 54 255
          | Some In_b -> Color.create 66 108 176 255
        in
        draw_rectangle_rec rect bg;
        draw_rectangle_lines_ex rect 1.5 (Color.create 190 214 239 255);
        draw_text (Printf.sprintf "%02d" i) (x + 6) 692 14 (Color.create 190 214 239 255);
        draw_text (ext_input_to_string cell) (x + 33) 705 20 Color.raywhite;
        if click && point_in_rect mouse_x mouse_y x y w h then input_cells.(i) <- cycle_input cell
      done;

      draw_text "Timeline output (per logical instant)" 24 745 20 Color.raywhite;
      let max_rows = 6 in
      List.iteri
        (fun i row ->
          if i < max_rows then
            let y = 772 + (i * 20) in
            draw_text
              (Printf.sprintf "t=%02d in=%s out=%s"
                 row.instant
                 (ext_input_to_string row.input)
                 (match row.output with None -> "-" | Some s -> s))
              24 y 18 (Color.create 230 238 249 255))
        !results;

      draw_text
        "Core focus: each block compiles to Tempo primitives; no FRP layer used."
        24 876 18 (Color.create 158 184 214 255);

      end_drawing ()
    done;

    close_window ())
