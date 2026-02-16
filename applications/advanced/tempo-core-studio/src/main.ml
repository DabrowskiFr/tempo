open Tempo
open Raylib

type ext_input = {
  red : bool;
  blue : bool;
  green : bool;
  yellow : bool;
}

type signal_name =
  | Sig_a
  | Sig_b
  | Sig_c
  | Sig_d

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

type parallel_branch =
  | Branch_left
  | Branch_right

type selection_target =
  | Target_main
  | Target_block of int
  | Target_parallel_branch of int * parallel_branch

type row = {
  target : selection_target;
  depth : int;
  text : string;
  signal : signal_name option;
}

let signal_name_to_string = function
  | Sig_a -> "red"
  | Sig_b -> "blue"
  | Sig_c -> "green"
  | Sig_d -> "yellow"

let ext_input_to_string = function
  | None -> "-"
  | Some { red; blue; green; yellow } ->
      let names =
        List.filter_map
          (fun (on, name) -> if on then Some name else None)
          [ (red, "red"); (blue, "blue"); (green, "green"); (yellow, "yellow") ]
      in
      if names = [] then "-" else String.concat "+" names

let ext_input_short = function
  | None -> "-"
  | Some { red; blue; green; yellow } ->
      let tags =
        List.filter_map
          (fun (on, tag) -> if on then Some tag else None)
          [ (red, "R"); (blue, "B"); (green, "G"); (yellow, "Y") ]
      in
      if tags = [] then "-" else String.concat "" tags

let cycle_signal = function
  | Sig_a -> Sig_b
  | Sig_b -> Sig_c
  | Sig_c -> Sig_d
  | Sig_d -> Sig_a

let signal_color = function
  | Sig_a -> Color.create 210 84 84 255
  | Sig_b -> Color.create 76 132 214 255
  | Sig_c -> Color.create 88 186 98 255
  | Sig_d -> Color.create 228 188 78 255

let input_has_any = function
  | { red; blue; green; yellow } -> red || blue || green || yellow

let toggle_input_signal signal = function
  | None -> (
      match signal with
      | Sig_a -> Some { red = true; blue = false; green = false; yellow = false }
      | Sig_b -> Some { red = false; blue = true; green = false; yellow = false }
      | Sig_c -> Some { red = false; blue = false; green = true; yellow = false }
      | Sig_d -> Some { red = false; blue = false; green = false; yellow = true })
  | Some v ->
      let next =
        match signal with
        | Sig_a -> { v with red = not v.red }
        | Sig_b -> { v with blue = not v.blue }
        | Sig_c -> { v with green = not v.green }
        | Sig_d -> { v with yellow = not v.yellow }
      in
      if input_has_any next then Some next else None

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

let kind_uses_signal = function
  | K_emit | K_await | K_await_imm | K_when | K_watch -> true
  | K_pause | K_parallel -> false

let block_label b =
  match b.kind with
  | K_emit -> "emit"
  | K_await -> "await"
  | K_await_imm -> "await_immediate"
  | K_pause -> "pause"
  | K_when -> "when do"
  | K_watch -> "watch do"
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

let append_to_parallel_branch ~(parallel_id : int) ~(branch : parallel_branch) ~(program : block list)
    (block : block) : block list option =
  match find_by_id_in_list parallel_id program with
  | Some b when b.kind = K_parallel ->
      begin
        match branch with
        | Branch_left -> b.body1 <- b.body1 @ [ block ]
        | Branch_right -> b.body2 <- b.body2 @ [ block ]
      end;
      Some program
  | _ -> None

let append_block ~(selected_target : selection_target) ~(program : block list) (block : block) =
  match selected_target with
  | Target_main -> program @ [ block ]
  | Target_parallel_branch (pid, branch) -> (
      match append_to_parallel_branch ~parallel_id:pid ~branch ~program block with
      | Some updated -> updated
      | None -> program @ [ block ])
  | Target_block sid -> (
      match find_by_id_in_list sid program with
      | Some b when b.kind = K_when || b.kind = K_watch ->
          b.body1 <- b.body1 @ [ block ];
          program
      | Some b when b.kind = K_parallel ->
          if List.length b.body1 <= List.length b.body2 then b.body1 <- b.body1 @ [ block ]
          else b.body2 <- b.body2 @ [ block ];
          program
      | Some _ ->
          let program', inserted = insert_after_in_list sid block program in
          if inserted then program' else program @ [ block ]
      | None -> program @ [ block ])

let rec flatten_blocks (depth : int) (blocks : block list) : row list =
  List.concat
    (List.map
       (fun (b : block) ->
         let me =
           [ { target = Target_block b.id
             ; depth
             ; text = block_label b
             ; signal = if kind_uses_signal b.kind then Some b.s1 else None
             }
           ]
         in
         if b.kind = K_parallel then
           let left_begin = { target = Target_parallel_branch (b.id, Branch_left); depth = depth + 1; text = "begin"; signal = None } in
           let left_body = flatten_blocks (depth + 2) b.body1 in
           let left_end = { target = Target_parallel_branch (b.id, Branch_left); depth = depth + 1; text = "end"; signal = None } in
           let right_begin = { target = Target_parallel_branch (b.id, Branch_right); depth = depth + 1; text = "begin"; signal = None } in
           let right_body = flatten_blocks (depth + 2) b.body2 in
           let right_end = { target = Target_parallel_branch (b.id, Branch_right); depth = depth + 1; text = "end"; signal = None } in
           me @ [ left_begin ] @ left_body @ [ left_end; right_begin ] @ right_body @ [ right_end ]
         else
           let c1 = flatten_blocks (depth + 1) b.body1 in
           let c2 = flatten_blocks (depth + 1) b.body2 in
           me @ c1 @ c2)
       blocks)

let flatten_tree (blocks : block list) : row list =
  { target = Target_main; depth = 0; text = "main"; signal = None } :: flatten_blocks 1 blocks

let signal_of_name sa sb sc sd = function
  | Sig_a -> sa
  | Sig_b -> sb
  | Sig_c -> sc
  | Sig_d -> sd

let input_color = function
  | None -> Color.create 42 58 78 255
  | Some { red; blue; green; yellow } ->
      let add_if on (ar, ag, ab, n) (r, g, b) =
        if on then (ar + r, ag + g, ab + b, n + 1) else (ar, ag, ab, n)
      in
      let ar, ag, ab, n = (0, 0, 0, 0) in
      let ar, ag, ab, n = add_if red (ar, ag, ab, n) (190, 76, 76) in
      let ar, ag, ab, n = add_if blue (ar, ag, ab, n) (66, 108, 176) in
      let ar, ag, ab, n = add_if green (ar, ag, ab, n) (88, 186, 98) in
      let ar, ag, ab, n = add_if yellow (ar, ag, ab, n) (228, 188, 78) in
      if n = 0 then Color.create 42 58 78 255
      else Color.create (ar / n) (ag / n) (ab / n) 255

let simulate ~(blocks : block list) ~(inputs : ext_input option list) ~(instants : int) : timeline_row list =
  let run input output =
    let sa = new_signal () in
    let sb = new_signal () in
    let sc = new_signal () in
    let sd = new_signal () in
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
          let frame = await_immediate input in
          if frame.red then (
            if not (is_present sa) then emit sa ();
            emit trace "input red");
          if frame.blue then (
            if not (is_present sb) then emit sb ();
            emit trace "input blue");
          if frame.green then (
            if not (is_present sc) then emit sc ();
            emit trace "input green");
          if frame.yellow then (
            if not (is_present sd) then emit sd ();
            emit trace "input yellow"));
      pause ();
      input_pump ()
    and eval_block b =
      match b.kind with
      | K_emit ->
          let sigv = signal_of_name sa sb sc sd b.s1 in
          emit_once sigv (signal_name_to_string b.s1)
      | K_await ->
          let sigv = signal_of_name sa sb sc sd b.s1 in
          let () = await sigv in
          emit trace (Printf.sprintf "await %s satisfied" (signal_name_to_string b.s1))
      | K_await_imm ->
          let sigv = signal_of_name sa sb sc sd b.s1 in
          let () = await_immediate sigv in
          emit trace (Printf.sprintf "await_immediate %s satisfied" (signal_name_to_string b.s1))
      | K_pause ->
          emit trace "pause";
          pause ()
      | K_when ->
          let guard = signal_of_name sa sb sc sd b.s1 in
          when_ guard (fun () ->
              emit trace (Printf.sprintf "when %s active" (signal_name_to_string b.s1));
              eval_blocks b.body1)
      | K_watch ->
          let watched = signal_of_name sa sb sc sd b.s1 in
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
    [ Some { red = false; blue = true; green = false; yellow = false }
    ; None
    ; Some { red = false; blue = true; green = false; yellow = false }
    ; Some { red = true; blue = false; green = false; yellow = false }
    ; None
    ; Some { red = false; blue = true; green = false; yellow = false }
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
    let selected_target = ref Target_main in
    let input_cells = Array.make instants None in
    if instants > 0 then input_cells.(0) <- Some { red = false; blue = true; green = false; yellow = false };
    if instants > 2 then input_cells.(2) <- Some { red = false; blue = true; green = false; yellow = false };
    if instants > 3 then input_cells.(3) <- Some { red = true; blue = false; green = false; yellow = false };
    if instants > 5 then input_cells.(5) <- Some { red = false; blue = true; green = false; yellow = false };

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

      let palette_x = 24 in
      let palette_y = 100 in
      draw_rectangle palette_x palette_y 360 760 (Color.create 24 44 69 255);
      draw_rectangle_lines palette_x palette_y 360 760 (Color.create 105 145 187 255);
      draw_text "Palette (click to insert)" (palette_x + 14) (palette_y + 10) 20 Color.raywhite;

      List.iteri
        (fun i (label, kind) ->
          let y = palette_y + 50 + (i * 54) in
          draw_button (palette_x + 12) y 336 44 "" false;
          if kind_uses_signal kind then (
            let prefix, suffix =
              match kind with
              | K_emit -> ("emit", "")
              | K_await -> ("await", "")
              | K_await_imm -> ("await_immediate", "")
              | K_when -> ("when", "do")
              | K_watch -> ("watch", "do")
              | _ -> ("", "")
            in
            let tx = palette_x + 26 in
            let ty = y + 9 in
            draw_text prefix tx ty 18 Color.raywhite;
            let px = tx + measure_text prefix 18 + 12 in
            draw_circle px (y + 22) 6.5 (signal_color Sig_b);
            draw_circle_lines px (y + 22) 6.5 (Color.create 225 236 248 255);
            if suffix <> "" then draw_text suffix (px + 12) ty 18 Color.raywhite)
          else
            draw_text label (palette_x + 22) (y + 9) 18 Color.raywhite;
          if click && point_in_rect mouse_x mouse_y (palette_x + 12) y 336 44 then (
            let b = mk_block kind Sig_a in
            script := append_block ~selected_target:!selected_target ~program:!script b;
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
            let selected = r.target = !selected_target in
            let bg = if selected then Color.create 74 116 163 255 else Color.create 45 80 120 255 in
            let row_rect = Rectangle.create (float_of_int (script_x + 12)) (float_of_int y) 590.0 26.0 in
            draw_rectangle_rec row_rect bg;
            draw_rectangle_lines_ex row_rect 1.5 (Color.create 165 205 240 255);
            begin
              match r.signal with
              | None -> ()
              | Some s ->
                  let displayed = Printf.sprintf "%s%s" (String.make (r.depth * 2) ' ') r.text in
                  let tw = measure_text displayed 16 in
                  let cx = script_x + 18 + tw + 12 in
                  draw_circle cx (y + 13) 6.0 (signal_color s);
                  draw_circle_lines cx (y + 13) 6.0 (Color.create 235 245 255 255)
            end;
            draw_text
              (Printf.sprintf "%s%s" (String.make (r.depth * 2) ' ') r.text)
              (script_x + 18)
              (y + 6)
              16
              Color.raywhite;
            if click && point_in_rect mouse_x mouse_y (script_x + 12) y 590 26 then
              selected_target := r.target)
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
        selected_target := Target_main;
        run_simulation ());
      if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 156) 292 42 then (
        Array.fill input_cells 0 instants None;
        run_simulation ());
      if click && point_in_rect mouse_x mouse_y (panel_x + 16) (panel_y + 204) 292 42 then (
        script := sample_program ();
        selected_target := Target_main;
        run_simulation ());

      draw_text "Selected block editor" (panel_x + 16) (panel_y + 262) 18 Color.raywhite;
      begin
        match !selected_target with
        | Target_main ->
            draw_text "main selected: insertions go to top-level"
              (panel_x + 16) (panel_y + 286) 16 (Color.create 170 192 220 255)
        | Target_parallel_branch (_, branch) ->
            let txt =
              match branch with
              | Branch_left -> "parallel branch selected: insertions go to left branch"
              | Branch_right -> "parallel branch selected: insertions go to right branch"
            in
            draw_text txt (panel_x + 16) (panel_y + 286) 16 (Color.create 170 192 220 255)
        | Target_block sid -> (
            match find_by_id_in_list sid !script with
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
                  selected_target := Target_main;
                  run_simulation ()))
      end;

      draw_text "Tick input editor (toggle red/blue/green/yellow per instant)" 24 600 20 Color.raywhite;
      for i = 0 to instants - 1 do
        let x = 24 + (i * 84) in
        let y = 630 in
        let w = 78 in
        let h = 62 in
        let rect = Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h) in
        let cell = input_cells.(i) in
        let bg = input_color cell in
        draw_rectangle_rec rect bg;
        draw_rectangle_lines_ex rect 1.5 (Color.create 190 214 239 255);
        draw_text (Printf.sprintf "%02d" i) (x + 6) 636 14 (Color.create 190 214 239 255);
        draw_text (ext_input_short cell) (x + 24) 650 14 Color.raywhite;

        let red_x = x + 16 in
        let blue_x = x + 46 in
        let green_x = x + 16 in
        let yellow_x = x + 46 in
        let sel_y1 = y + 45 in
        let sel_y2 = y + 58 in
        let red_on =
          match cell with
          | Some v -> v.red
          | None -> false
        in
        let blue_on =
          match cell with
          | Some v -> v.blue
          | None -> false
        in
        let green_on =
          match cell with
          | Some v -> v.green
          | None -> false
        in
        let yellow_on =
          match cell with
          | Some v -> v.yellow
          | None -> false
        in
        draw_circle red_x sel_y1 5.5
          (if red_on then signal_color Sig_a else Color.create 66 78 92 255);
        draw_circle_lines red_x sel_y1 5.5 (Color.create 225 236 248 255);
        draw_circle blue_x sel_y1 5.5
          (if blue_on then signal_color Sig_b else Color.create 66 78 92 255);
        draw_circle_lines blue_x sel_y1 5.5 (Color.create 225 236 248 255);
        draw_circle green_x sel_y2 5.5
          (if green_on then signal_color Sig_c else Color.create 66 78 92 255);
        draw_circle_lines green_x sel_y2 5.5 (Color.create 225 236 248 255);
        draw_circle yellow_x sel_y2 5.5
          (if yellow_on then signal_color Sig_d else Color.create 66 78 92 255);
        draw_circle_lines yellow_x sel_y2 5.5 (Color.create 225 236 248 255);

        if click && point_in_rect mouse_x mouse_y (red_x - 8) (sel_y1 - 8) 16 16 then (
          input_cells.(i) <- toggle_input_signal Sig_a cell;
          run_simulation ());
        if click && point_in_rect mouse_x mouse_y (blue_x - 8) (sel_y1 - 8) 16 16 then (
          input_cells.(i) <- toggle_input_signal Sig_b cell;
          run_simulation ());
        if click && point_in_rect mouse_x mouse_y (green_x - 8) (sel_y2 - 8) 16 16 then (
          input_cells.(i) <- toggle_input_signal Sig_c cell;
          run_simulation ());
        if click && point_in_rect mouse_x mouse_y (yellow_x - 8) (sel_y2 - 8) 16 16 then (
          input_cells.(i) <- toggle_input_signal Sig_d cell;
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
