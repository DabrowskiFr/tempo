open Tempo
open Raylib

type ext_input =
  | In_a
  | In_b

type signal_name =
  | Sig_a
  | Sig_b

type block_kind =
  | K_emit
  | K_await
  | K_await_imm
  | K_pause
  | K_when
  | K_watch
  | K_parallel

type block = {
  id : int;
  mutable kind : block_kind;
  mutable s1 : signal_name;
  mutable body1 : block list;
  mutable body2 : block list;
}

type timeline_row = {
  instant : int;
  input : ext_input option;
  output : string option;
}

type row = {
  id : int;
  depth : int;
  text : string;
}

let signal_name_to_string = function
  | Sig_a -> "A"
  | Sig_b -> "B"

let ext_input_to_string = function
  | None -> "-"
  | Some In_a -> "A"
  | Some In_b -> "B"

let cycle_signal = function
  | Sig_a -> Sig_b
  | Sig_b -> Sig_a

let cycle_input = function
  | None -> Some In_a
  | Some In_a -> Some In_b
  | Some In_b -> None

let kind_to_string = function
  | K_emit -> "emit"
  | K_await -> "await"
  | K_await_imm -> "await_immediate"
  | K_pause -> "pause"
  | K_when -> "when"
  | K_watch -> "watch"
  | K_parallel -> "parallel"

let cycle_kind = function
  | K_emit -> K_await
  | K_await -> K_await_imm
  | K_await_imm -> K_pause
  | K_pause -> K_when
  | K_when -> K_watch
  | K_watch -> K_parallel
  | K_parallel -> K_emit

let has_body1 = function
  | K_when | K_watch | K_parallel -> true
  | _ -> false

let has_body2 = function
  | K_parallel -> true
  | _ -> false

let block_label b =
  match b.kind with
  | K_emit -> Printf.sprintf "emit %s" (signal_name_to_string b.s1)
  | K_await -> Printf.sprintf "await %s" (signal_name_to_string b.s1)
  | K_await_imm -> Printf.sprintf "await_immediate %s" (signal_name_to_string b.s1)
  | K_pause -> "pause"
  | K_when ->
      Printf.sprintf "when %s do body1[%d]"
        (signal_name_to_string b.s1)
        (List.length b.body1)
  | K_watch ->
      Printf.sprintf "watch %s do body1[%d]"
        (signal_name_to_string b.s1)
        (List.length b.body1)
  | K_parallel ->
      Printf.sprintf "parallel body1[%d] || body2[%d]"
        (List.length b.body1)
        (List.length b.body2)

let point_in_rect x y rx ry rw rh =
  x >= rx && x <= rx + rw && y >= ry && y <= ry + rh

let draw_button x y w h label active =
  let rect = Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h) in
  let bg = if active then Color.create 65 150 210 255 else Color.create 38 62 88 255 in
  draw_rectangle_rec rect bg;
  draw_rectangle_lines_ex rect 2.0 (Color.create 195 220 245 255);
  draw_text label (x + 10) (y + 9) 18 Color.raywhite

let next_id =
  let r = ref 0 in
  fun () ->
    incr r;
    !r

let mk_block kind s1 =
  { id = next_id (); kind; s1; body1 = []; body2 = [] }

let sample_program () =
  let b1 = mk_block K_emit Sig_a in
  let b2 = mk_block K_pause Sig_a in
  let b3 = mk_block K_when Sig_b in
  b3.body1 <- [ mk_block K_emit Sig_a ];
  let b4 = mk_block K_parallel Sig_a in
  b4.body1 <- [ mk_block K_emit Sig_a ];
  b4.body2 <- [ mk_block K_emit Sig_b; mk_block K_await Sig_a ];
  [ b1; b2; b3; b4 ]

let rec find_by_id_in_list (target_id : int) (blocks : block list) : block option =
  match blocks with
  | [] -> None
  | (b : block) :: rest ->
      if b.id = target_id then Some b
      else
        match find_by_id_in_list target_id b.body1 with
        | Some _ as r -> r
        | None -> (
            match find_by_id_in_list target_id b.body2 with
            | Some _ as r -> r
            | None -> find_by_id_in_list target_id rest)

let rec remove_by_id_from_list (target_id : int) (blocks : block list) : block list * bool =
  let rec loop (acc : block list) (remaining : block list) =
    match remaining with
    | [] -> (List.rev acc, false)
    | (b : block) :: rest ->
        if b.id = target_id then (List.rev_append acc rest, true)
        else
          let body1', removed1 = remove_by_id_from_list target_id b.body1 in
          b.body1 <- body1';
          if removed1 then (List.rev_append (b :: acc) rest, true)
          else
            let body2', removed2 = remove_by_id_from_list target_id b.body2 in
            b.body2 <- body2';
            if removed2 then (List.rev_append (b :: acc) rest, true)
            else loop (b :: acc) rest
  in
  loop [] blocks

let rec insert_after_in_list (target_id : int) (new_block : block) (blocks : block list) :
    block list * bool =
  let rec loop acc = function
    | [] -> (List.rev acc, false)
    | (b : block) :: rest ->
        if b.id = target_id then (List.rev_append acc (b :: new_block :: rest), true)
        else
          let body1', inserted1 = insert_after_in_list target_id new_block b.body1 in
          b.body1 <- body1';
          if inserted1 then (List.rev_append (b :: acc) rest, true)
          else
            let body2', inserted2 = insert_after_in_list target_id new_block b.body2 in
            b.body2 <- body2';
            if inserted2 then (List.rev_append (b :: acc) rest, true)
            else loop (b :: acc) rest
  in
  loop [] blocks

let append_block ~selected_id ~program block =
  if selected_id = 0 then program @ [ block ]
  else
    match find_by_id_in_list selected_id program with
    | Some b when b.kind = K_when || b.kind = K_watch ->
        b.body1 <- b.body1 @ [ block ];
        program
    | Some b when b.kind = K_parallel ->
        if List.length b.body1 <= List.length b.body2 then b.body1 <- b.body1 @ [ block ]
        else b.body2 <- b.body2 @ [ block ];
        program
    | Some _ ->
        let program', inserted = insert_after_in_list selected_id block program in
        if inserted then program' else program @ [ block ]
    | None -> program @ [ block ]

let rec flatten_blocks (depth : int) (blocks : block list) : row list =
  List.concat
    (List.map
       (fun (b : block) ->
         let me = [ { id = b.id; depth; text = block_label b } ] in
         let c1 = flatten_blocks (depth + 1) b.body1 in
         let c2 = flatten_blocks (depth + 1) b.body2 in
         me @ c1 @ c2)
       blocks)

let flatten_tree (blocks : block list) : row list =
  { id = 0; depth = 0; text = "main" } :: flatten_blocks 1 blocks

let signal_of_name sa sb = function
  | Sig_a -> sa
  | Sig_b -> sb

let simulate ~(blocks : block list) ~(inputs : ext_input option list) ~(instants : int) : timeline_row list =
  let run input output =
    let sa = new_signal () in
    let sb = new_signal () in
    let trace = new_signal_agg ~initial:[] ~combine:(fun acc msg -> msg :: acc) in
    let emit_once sigv name =
      if is_present sigv then
        emit trace (Printf.sprintf "emit %s skipped (already present this instant)" name)
      else (
        emit sigv ();
        emit trace (Printf.sprintf "emit %s" name))
    in
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
    and eval_block b =
      match b.kind with
      | K_emit ->
          let sigv = signal_of_name sa sb b.s1 in
          emit_once sigv (signal_name_to_string b.s1)
      | K_await ->
          let sigv = signal_of_name sa sb b.s1 in
          let () = await sigv in
          emit trace (Printf.sprintf "await %s satisfied" (signal_name_to_string b.s1))
      | K_await_imm ->
          let sigv = signal_of_name sa sb b.s1 in
          let () = await_immediate sigv in
          emit trace (Printf.sprintf "await_immediate %s satisfied" (signal_name_to_string b.s1))
      | K_pause ->
          emit trace "pause";
          pause ()
      | K_when ->
          let guard = signal_of_name sa sb b.s1 in
          when_ guard (fun () ->
              emit trace (Printf.sprintf "when %s active" (signal_name_to_string b.s1));
              eval_blocks b.body1)
      | K_watch ->
          let watched = signal_of_name sa sb b.s1 in
          watch watched (fun () -> eval_blocks b.body1)
      | K_parallel ->
          parallel [ (fun () -> eval_blocks b.body1); (fun () -> eval_blocks b.body2) ]
    and eval_blocks lst = List.iter eval_block lst
    and program_once () =
      eval_blocks blocks;
      emit trace "program end"
    and flush_trace () =
      let msgs = await trace in
      emit output (String.concat " | " (List.rev msgs));
      flush_trace ()
    in
    parallel [ input_pump; program_once; flush_trace ]
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
  let program = sample_program () in
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
  let rows = simulate ~blocks:program ~inputs ~instants in
  List.iter
    (fun r ->
      Printf.printf "t=%02d in=%s out=%s\n"
        r.instant
        (ext_input_to_string r.input)
        (match r.output with None -> "-" | Some s -> s))
    rows

let () =
  let headless, instants = parse_args () in
  if headless then run_headless instants
  else (
    init_window 1400 900 "Tempo Core Studio";
    set_target_fps 60;

    let palette : (string * block_kind) list =
      [ ("emit", K_emit)
      ; ("await", K_await)
      ; ("await_immediate", K_await_imm)
      ; ("pause", K_pause)
      ; ("when (container)", K_when)
      ; ("watch (container)", K_watch)
      ; ("parallel (container)", K_parallel)
      ]
    in

    let script = ref (sample_program ()) in
    let selected_id = ref 0 in
    let input_cells = Array.make instants None in
    if instants > 0 then input_cells.(0) <- Some In_b;
    if instants > 2 then input_cells.(2) <- Some In_b;
    if instants > 3 then input_cells.(3) <- Some In_a;
    if instants > 5 then input_cells.(5) <- Some In_b;

    let results = ref [] in
    let run_count = ref 0 in
    let status = ref "Ready" in

    let run_simulation () =
      let inputs = Array.to_list input_cells in
      results := simulate ~blocks:!script ~inputs ~instants;
      incr run_count;
      let non_empty =
        List.fold_left
          (fun acc row -> match row.output with None -> acc | Some _ -> acc + 1)
          0 !results
      in
      status :=
        Printf.sprintf "Simulation run #%d: %d instants, %d outputs"
          !run_count instants non_empty
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
        "Hierarchical core playground: blocks with nested bodies (Scratch-like)"
        24 58 18 (Color.create 176 198 224 255);

      let palette_x = 24 in
      let palette_y = 100 in
      draw_rectangle palette_x palette_y 360 760 (Color.create 24 44 69 255);
      draw_rectangle_lines palette_x palette_y 360 760 (Color.create 105 145 187 255);
      draw_text "Palette (click to insert)" (palette_x + 14) (palette_y + 10) 20 Color.raywhite;

      List.iteri
        (fun i (label, kind) ->
          let y = palette_y + 50 + (i * 54) in
          draw_button (palette_x + 12) y 336 44 label false;
          if click && point_in_rect mouse_x mouse_y (palette_x + 12) y 336 44 then (
            let b = mk_block kind Sig_a in
            script := append_block ~selected_id:!selected_id ~program:!script b;
            run_simulation ()))
        palette;

      let script_x = 410 in
      let script_y = 100 in
      draw_rectangle script_x script_y 620 540 (Color.create 26 47 73 255);
      draw_rectangle_lines script_x script_y 620 540 (Color.create 105 145 187 255);
      draw_text "Program Tree" (script_x + 14) (script_y + 10) 20 Color.raywhite;

      let rows = flatten_tree !script in
      List.iteri
        (fun i r ->
          if i < 16 then
            let y = script_y + 48 + (i * 30) in
            let selected = r.id = !selected_id in
            let bg = if selected then Color.create 74 116 163 255 else Color.create 45 80 120 255 in
            let row_rect = Rectangle.create (float_of_int (script_x + 12)) (float_of_int y) 590.0 26.0 in
            draw_rectangle_rec row_rect bg;
            draw_rectangle_lines_ex row_rect 1.5 (Color.create 165 205 240 255);
            draw_text
              (Printf.sprintf "%s%s" (String.make (r.depth * 2) ' ') r.text)
              (script_x + 18)
              (y + 6)
              16
              Color.raywhite;
            if click && point_in_rect mouse_x mouse_y (script_x + 12) y 590 26 then
              selected_id := r.id)
        rows;

      let panel_x = 1050 in
      let panel_y = 100 in
      draw_rectangle panel_x panel_y 326 540 (Color.create 24 44 69 255);
      draw_rectangle_lines panel_x panel_y 326 540 (Color.create 105 145 187 255);
      draw_text "Actions" (panel_x + 14) (panel_y + 10) 20 Color.raywhite;
      draw_text (Printf.sprintf "Runs: %d" !run_count) (panel_x + 220) (panel_y + 14) 18
        (Color.create 255 220 130 255);

      draw_button (panel_x + 16) (panel_y + 54) 292 46 "Run Simulation" true;
      draw_button (panel_x + 16) (panel_y + 108) 292 42 "Clear Program" false;
      draw_button (panel_x + 16) (panel_y + 156) 292 42 "Clear Inputs" false;
      draw_button (panel_x + 16) (panel_y + 204) 292 42 "Load Sample" false;

      if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 54) 292 46 then run_simulation ();
      if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 108) 292 42 then (
        script := [];
        selected_id := 0;
        run_simulation ());
      if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 156) 292 42 then (
        Array.fill input_cells 0 instants None;
        run_simulation ());
      if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 204) 292 42 then (
        script := sample_program ();
        selected_id := 0;
        run_simulation ());

      draw_text "Selected block editor" (panel_x + 16) (panel_y + 262) 18 Color.raywhite;
      begin
        if !selected_id = 0 then
          draw_text "main selected: insertions go to top-level"
            (panel_x + 16) (panel_y + 286) 16 (Color.create 170 192 220 255)
        else
          match find_by_id_in_list !selected_id !script with
            | None -> draw_text "Selection lost" (panel_x + 16) (panel_y + 362) 16 (Color.create 220 120 120 255)
            | Some b ->
                draw_text (Printf.sprintf "id=%d  kind=%s" b.id (kind_to_string b.kind))
                  (panel_x + 16) (panel_y + 286) 15 (Color.create 220 236 252 255);
                draw_text (Printf.sprintf "signal=%s" (signal_name_to_string b.s1))
                  (panel_x + 16) (panel_y + 306) 15 (Color.create 220 236 252 255);
                draw_button (panel_x + 16) (panel_y + 330) 140 36 "Cycle Kind" false;
                draw_button (panel_x + 168) (panel_y + 330) 140 36 "Cycle Signal" false;
                draw_button (panel_x + 16) (panel_y + 374) 292 36 "Remove Selected" false;
                if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 330) 140 36 then (
                  b.kind <- cycle_kind b.kind;
                  if not (has_body1 b.kind) then b.body1 <- [];
                  if not (has_body2 b.kind) then b.body2 <- [];
                  run_simulation ());
                if click && point_in_rect mouse_x mouse_y (panel_x + 168) (panel_y + 330) 140 36 then (
                  b.s1 <- cycle_signal b.s1;
                  run_simulation ());
                if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 374) 292 36 then (
                  script := fst (remove_by_id_from_list b.id !script);
                  selected_id := 0;
                  run_simulation ())
      end;

      draw_text "Tick input editor (click cell: - -> A -> B)" 24 600 20 Color.raywhite;
      for i = 0 to instants - 1 do
        let x = 24 + (i * 84) in
        let y = 630 in
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
        draw_text (Printf.sprintf "%02d" i) (x + 6) 636 14 (Color.create 190 214 239 255);
        draw_text (ext_input_to_string cell) (x + 33) 649 20 Color.raywhite;
        if click && point_in_rect mouse_x mouse_y x y w h then (
          input_cells.(i) <- cycle_input cell;
          run_simulation ())
      done;

      draw_text "Timeline output (per logical instant)" 24 690 20 Color.raywhite;
      draw_text "Tip: edit tree + inputs, simulation refreshes immediately" 420 692 16
        (Color.create 180 205 230 255);
      let max_rows = min instants 10 in
      List.iteri
        (fun i row ->
          if i < max_rows then
            let y = 718 + (i * 16) in
            let bg =
              if i mod 2 = 0 then Color.create 28 46 70 255
              else Color.create 33 53 80 255
            in
            draw_rectangle 24 (y - 1) 1320 16 bg;
            draw_text
              (Printf.sprintf "t=%02d in=%s out=%s"
                 row.instant
                 (ext_input_to_string row.input)
                 (match row.output with None -> "-" | Some s -> s))
              28 y 15 (Color.create 236 244 255 255))
        !results;

      draw_text
        "Core focus: hierarchical blocks compile to Tempo primitives; no FRP layer used."
        24 868 17 (Color.create 158 184 214 255);
      draw_text !status 720 868 15 (Color.create 210 225 245 255);

      end_drawing ()
    done;

    close_window ())
