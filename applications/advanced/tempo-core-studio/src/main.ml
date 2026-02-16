open Tempo
open Raylib
open Tempo_game

(* -------------------------------------------------------------------------- *)
(* Part 1: Domain logic (model, editing, view data)                           *)
(* -------------------------------------------------------------------------- *)

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
  | K_parallel -> "parallel do"

let point_in_rect x y rx ry rw rh =
  x >= rx && x <= rx + rw && y >= ry && y <= ry + rh

let mk_button ~id ~x ~y ~w ~h ~label =
  Ui.button ~id { Ui.x = float_of_int x; y = float_of_int y; w = float_of_int w; h = float_of_int h } ~label ()

let logical_width = 1280
let logical_height = 768

let c_bg_top = Color.create 23 36 54 255
let c_bg_bottom = Color.create 14 22 34 255
let c_topbar = Color.create 10 27 45 220
let c_topline = Color.create 88 140 188 200
let c_seq_box = Color.create 19 33 49 220
let c_seq_border = Color.create 90 138 182 230

let dim_signal_color = function
  | Sig_a -> Color.create 104 63 63 255
  | Sig_b -> Color.create 57 76 103 255
  | Sig_c -> Color.create 62 99 66 255
  | Sig_d -> Color.create 116 102 64 255

let draw_signal_quad ~x ~y ~cell_w ~cell_h ~gap ~is_on =
  let draw_one px py signal =
    let c = if is_on signal then signal_color signal else dim_signal_color signal in
    draw_rectangle px py cell_w cell_h c;
    draw_rectangle_lines px py cell_w cell_h (Color.create 215 232 250 210)
  in
  let red_rect = (x, y, cell_w, cell_h) in
  let blue_rect = (x + cell_w + gap, y, cell_w, cell_h) in
  let green_rect = (x, y + cell_h + gap, cell_w, cell_h) in
  let yellow_rect = (x + cell_w + gap, y + cell_h + gap, cell_w, cell_h) in
  let rx, ry, _, _ = red_rect in
  draw_one rx ry Sig_a;
  let bx, by, _, _ = blue_rect in
  draw_one bx by Sig_b;
  let gx, gy, _, _ = green_rect in
  draw_one gx gy Sig_c;
  let yx, yy, _, _ = yellow_rect in
  draw_one yx yy Sig_d;
  (red_rect, blue_rect, green_rect, yellow_rect)

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

let signal_active_in_input signal = function
  | None -> false
  | Some v -> (
      match signal with
      | Sig_a -> v.red
      | Sig_b -> v.blue
      | Sig_c -> v.green
      | Sig_d -> v.yellow)

let signal_of_token = function
  | "red" -> Some Sig_a
  | "blue" -> Some Sig_b
  | "green" -> Some Sig_c
  | "yellow" -> Some Sig_d
  | _ -> None

let extract_output_signals (s : string) : signal_name list =
  let len = String.length s in
  let is_alpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  in
  let rec tokenize i acc =
    if i >= len then List.rev acc
    else if is_alpha s.[i] then
      let j = ref (i + 1) in
      while !j < len && is_alpha s.[!j] do
        incr j
      done;
      let tok = String.lowercase_ascii (String.sub s i (!j - i)) in
      tokenize !j (tok :: acc)
    else tokenize (i + 1) acc
  in
  let tokens = tokenize 0 [] in
  let rec collect toks acc =
    match toks with
    | "emit" :: color :: rest -> (
        match signal_of_token color with
        | Some sig_name when not (List.mem sig_name acc) -> collect rest (acc @ [ sig_name ])
        | _ -> collect rest acc)
    | "input" :: color :: rest -> (
        match signal_of_token color with
        | Some sig_name when not (List.mem sig_name acc) -> collect rest (acc @ [ sig_name ])
        | _ -> collect rest acc)
    | _ :: rest -> collect rest acc
    | [] -> acc
  in
  collect tokens []

(* -------------------------------------------------------------------------- *)
(* Part 2: Synchronous execution (Tempo primitives and instants)              *)
(* -------------------------------------------------------------------------- *)

module Sync = struct
  let signal_of_name sa sb sc sd = function
    | Sig_a -> sa
    | Sig_b -> sb
    | Sig_c -> sc
    | Sig_d -> sd

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
end

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
  let rows = Sync.simulate ~blocks:program ~inputs ~instants in
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
    init_window logical_width logical_height "Tempo Core Studio";
    set_window_state [ ConfigFlags.Window_resizable ];
    set_target_fps 60;
    let canvas = load_render_texture logical_width logical_height in
    set_texture_filter (RenderTexture.texture canvas) TextureFilter.Point;

    let palette : (string * block_kind) list =
      [ ("emit", K_emit)
      ; ("await", K_await)
      ; ("await_immediate", K_await_imm)
      ; ("pause", K_pause)
      ; ("when", K_when)
      ; ("watch", K_watch)
      ; ("parallel", K_parallel)
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
    let notice = ref "Ready." in
    let notice_ttl = ref 0 in

    let set_notice msg =
      notice := msg;
      notice_ttl := 140
    in

    let run_simulation () =
      let inputs = Array.to_list input_cells in
      results := Sync.simulate ~blocks:!script ~inputs ~instants;
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
      let win_w = get_screen_width () in
      let win_h = get_screen_height () in
      let sx = float_of_int win_w /. float_of_int logical_width in
      let sy = float_of_int win_h /. float_of_int logical_height in
      let scale = max 1.0 (floor (min sx sy)) in
      let dst_w = int_of_float (float_of_int logical_width *. scale) in
      let dst_h = int_of_float (float_of_int logical_height *. scale) in
      let dst_x = (win_w - dst_w) / 2 in
      let dst_y = (win_h - dst_h) / 2 in
      let mp = get_mouse_position () in
      let mx = int_of_float (Vector2.x mp) in
      let my = int_of_float (Vector2.y mp) in
      let inside_viewport =
        mx >= dst_x && mx < dst_x + dst_w && my >= dst_y && my < dst_y + dst_h
      in
      let logical_mx =
        if inside_viewport && scale > 0.0 then
          (float_of_int (mx - dst_x)) /. scale
        else -1000.0
      in
      let logical_my =
        if inside_viewport && scale > 0.0 then
          (float_of_int (my - dst_y)) /. scale
        else -1000.0
      in
      let interaction =
        { Ui.pointer = { x = logical_mx; y = logical_my }
        ; down = is_mouse_button_down MouseButton.Left
        ; pressed = is_mouse_button_pressed MouseButton.Left
        }
      in
      let click = interaction.pressed in
      let mouse_x = int_of_float interaction.pointer.x in
      let mouse_y = int_of_float interaction.pointer.y in
      if !notice_ttl > 0 then decr notice_ttl;

      begin_texture_mode canvas;
      clear_background c_bg_bottom;
      draw_rectangle_gradient_v 0 0 logical_width logical_height c_bg_top c_bg_bottom;
      draw_rectangle 0 0 logical_width 72 c_topbar;
      draw_line 0 72 logical_width 72 c_topline;

      draw_text "Tempo Core Studio" 24 16 36 (Color.create 235 240 255 255);

      let palette_x = 24 in
      let palette_y = 100 in
      draw_rectangle palette_x palette_y 360 450 (Color.create 24 44 69 255);
      draw_rectangle_lines palette_x palette_y 360 450 (Color.create 105 145 187 255);
      draw_text "Palette (click to insert)" (palette_x + 14) (palette_y + 10) 20 Color.raywhite;

      List.iteri
        (fun i (label, kind) ->
          let y = palette_y + 50 + (i * 54) in
          let btn = mk_button ~id:(Printf.sprintf "palette:%d" i) ~x:(palette_x + 12) ~y ~w:336 ~h:44 ~label in
          Tempo_game_raylib.Ui.draw_button btn;
          if Ui.button_pressed interaction btn then (
            let b = mk_block kind Sig_a in
            script := append_block ~selected_target:!selected_target ~program:!script b;
            set_notice (Printf.sprintf "Added block: %s" label);
            run_simulation ()))
        palette;

      let script_x = 410 in
      let script_y = 100 in
      draw_rectangle script_x script_y 620 450 (Color.create 26 47 73 255);
      draw_rectangle_lines script_x script_y 620 450 (Color.create 105 145 187 255);
      draw_text "Program Tree" (script_x + 14) (script_y + 10) 20 Color.raywhite;

      let rows = flatten_tree !script in
      List.iteri
        (fun i r ->
          if i < 13 then
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
      let panel =
        Hud.panel
          ~rect:{ Ui.x = float_of_int panel_x; y = float_of_int panel_y; w = 326.0; h = 450.0 }
          ~title:"Actions"
      in
      Tempo_game_raylib.Hud.draw_panel panel;
      draw_text (Printf.sprintf "Runs: %d" !run_count) (panel_x + 220) (panel_y + 14) 18
        (Color.create 255 220 130 255);

      let clear_prog_btn = mk_button ~id:"clear_program" ~x:(panel_x + 16) ~y:(panel_y + 54) ~w:292 ~h:42 ~label:"Clear Program" in
      let clear_inputs_btn = mk_button ~id:"clear_inputs" ~x:(panel_x + 16) ~y:(panel_y + 102) ~w:292 ~h:42 ~label:"Clear Inputs" in
      let load_sample_btn = mk_button ~id:"load_sample" ~x:(panel_x + 16) ~y:(panel_y + 150) ~w:292 ~h:42 ~label:"Load Sample" in
      Tempo_game_raylib.Ui.draw_button clear_prog_btn;
      Tempo_game_raylib.Ui.draw_button clear_inputs_btn;
      Tempo_game_raylib.Ui.draw_button load_sample_btn;

      if Ui.button_pressed interaction clear_prog_btn then (
        script := [];
        selected_target := Target_main;
        set_notice "Program cleared.";
        run_simulation ());
      if Ui.button_pressed interaction clear_inputs_btn then (
        Array.fill input_cells 0 instants None;
        set_notice "Input sequence cleared.";
        run_simulation ());
      if Ui.button_pressed interaction load_sample_btn then (
        script := sample_program ();
        selected_target := Target_main;
        set_notice "Sample program loaded.";
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
                let remove_btn = mk_button ~id:"remove_selected" ~x:(panel_x + 16) ~y:(panel_y + 392) ~w:292 ~h:34 ~label:"Remove Selected" in
                let change_primitive_btn =
                  mk_button ~id:"change_primitive" ~x:(panel_x + 16) ~y:(panel_y + 434) ~w:292 ~h:34 ~label:"Change Primitive"
                in
                Tempo_game_raylib.Ui.draw_button remove_btn;
                Tempo_game_raylib.Ui.draw_button change_primitive_btn;

                if kind_uses_signal b.kind then (
                  let pad_x = panel_x + 16 in
                  let pad_y = panel_y + 330 in
                  let red_rect, blue_rect, green_rect, yellow_rect =
                    draw_signal_quad ~x:pad_x ~y:pad_y ~cell_w:140 ~cell_h:20 ~gap:12
                      ~is_on:(fun signal -> b.s1 = signal)
                  in
                  let rx, ry, rw, rh = red_rect in
                  let bx, by, bw, bh = blue_rect in
                  let gx, gy, gw, gh = green_rect in
                  let yx, yy, yw, yh = yellow_rect in
                  if click && point_in_rect mouse_x mouse_y rx ry rw rh && b.s1 <> Sig_a then (
                    b.s1 <- Sig_a;
                    set_notice "Signal set to red.";
                    run_simulation ());
                  if click && point_in_rect mouse_x mouse_y bx by bw bh && b.s1 <> Sig_b then (
                    b.s1 <- Sig_b;
                    set_notice "Signal set to blue.";
                    run_simulation ());
                  if click && point_in_rect mouse_x mouse_y gx gy gw gh && b.s1 <> Sig_c then (
                    b.s1 <- Sig_c;
                    set_notice "Signal set to green.";
                    run_simulation ());
                  if click && point_in_rect mouse_x mouse_y yx yy yw yh && b.s1 <> Sig_d then (
                    b.s1 <- Sig_d;
                    set_notice "Signal set to yellow.";
                    run_simulation ()))
                else
                  draw_text "no signal selector for this kind" (panel_x + 168) (panel_y + 342) 13
                    (Color.create 150 170 198 255);

                if Ui.button_pressed interaction change_primitive_btn then (
                  b.kind <- cycle_kind b.kind;
                  if not (has_body1 b.kind) then b.body1 <- [];
                  if not (has_body2 b.kind) then b.body2 <- [];
                  set_notice (Printf.sprintf "Primitive changed to %s." (kind_to_string b.kind));
                  run_simulation ());
                if Ui.button_pressed interaction remove_btn then (
                  script := fst (remove_by_id_from_list b.id !script);
                  selected_target := Target_main;
                  set_notice "Selected block removed.";
                  run_simulation ()))
      end;

      draw_rectangle 20 556 1240 94 c_seq_box;
      draw_rectangle_lines 20 556 1240 94 c_seq_border;
      draw_text "Input sequencer (click quadrants)" 24 560 20 Color.raywhite;
      for i = 0 to instants - 1 do
        let x = 24 + (i * 84) in
        let y = 588 in
        let w = 78 in
        let h = 62 in
        let rect = Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h) in
        let cell = input_cells.(i) in
        draw_rectangle_rec rect (Color.create 23 34 48 255);
        draw_rectangle_lines_ex rect 1.5 (Color.create 190 214 239 255);
        draw_text (Printf.sprintf "%02d" i) (x + 6) 633 12 (Color.create 190 214 239 255);

        let red_rect, blue_rect, green_rect, yellow_rect =
          draw_signal_quad ~x:(x + 9) ~y:(y + 15) ~cell_w:28 ~cell_h:18 ~gap:4
            ~is_on:(fun signal -> signal_active_in_input signal cell)
        in
        let rx, ry, rw, rh = red_rect in
        let bx, by, bw, bh = blue_rect in
        let gx, gy, gw, gh = green_rect in
        let yx, yy, yw, yh = yellow_rect in

        if click && point_in_rect mouse_x mouse_y rx ry rw rh then (
          input_cells.(i) <- toggle_input_signal Sig_a cell;
          set_notice (Printf.sprintf "Input t=%02d toggled red." i);
          run_simulation ());
        if click && point_in_rect mouse_x mouse_y bx by bw bh then (
          input_cells.(i) <- toggle_input_signal Sig_b cell;
          set_notice (Printf.sprintf "Input t=%02d toggled blue." i);
          run_simulation ());
        if click && point_in_rect mouse_x mouse_y gx gy gw gh then (
          input_cells.(i) <- toggle_input_signal Sig_c cell;
          set_notice (Printf.sprintf "Input t=%02d toggled green." i);
          run_simulation ());
        if click && point_in_rect mouse_x mouse_y yx yy yw yh then (
          input_cells.(i) <- toggle_input_signal Sig_d cell;
          set_notice (Printf.sprintf "Input t=%02d toggled yellow." i);
          run_simulation ())
      done;

      draw_rectangle 20 648 1240 94 c_seq_box;
      draw_rectangle_lines 20 648 1240 94 c_seq_border;
      draw_text "Output sequencer" 24 652 20 Color.raywhite;
      draw_text "Tip: edit tree + pads, simulation refreshes immediately" 420 654 16
        (Color.create 180 205 230 255);
      for i = 0 to instants - 1 do
        let x = 24 + (i * 84) in
        let y = 680 in
        let w = 78 in
        let h = 62 in
        let rect = Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h) in
        draw_rectangle_rec rect (Color.create 23 34 48 255);
        draw_rectangle_lines_ex rect 1.5 (Color.create 190 214 239 255);
        draw_text (Printf.sprintf "%02d" i) (x + 6) 733 12 (Color.create 190 214 239 255);

        let out_signals =
          let out_i = i + 1 in
          if out_i < List.length !results then
            match (List.nth !results out_i).output with
            | None -> []
            | Some s -> extract_output_signals s
          else []
        in
        let _ =
          draw_signal_quad ~x:(x + 9) ~y:(y + 15) ~cell_w:28 ~cell_h:18 ~gap:4
            ~is_on:(fun signal -> List.mem signal out_signals)
        in
        ()
      done;

      draw_text
        "Core focus: hierarchical blocks compile to Tempo primitives; no FRP layer used."
        24 746 15 (Color.create 158 184 214 255);
      draw_text !status 720 746 14 (Color.create 210 225 245 255);
      if !notice_ttl > 0 then (
        let alpha = min 255 (80 + (!notice_ttl * 2)) in
        draw_rectangle 24 44 640 20 (Color.create 30 57 82 (alpha / 2));
        draw_text !notice 30 46 14 (Color.create 220 238 255 alpha));

      end_texture_mode ();

      begin_drawing ();
      clear_background (Color.create 8 12 18 255);
      let src =
        Rectangle.create 0.0 0.0 (float_of_int logical_width) (-.(float_of_int logical_height))
      in
      let dst =
        Rectangle.create
          (float_of_int dst_x)
          (float_of_int dst_y)
          (float_of_int dst_w)
          (float_of_int dst_h)
      in
      draw_texture_pro (RenderTexture.texture canvas) src dst (Vector2.create 0.0 0.0) 0.0 Color.raywhite;
      end_drawing ()
    done;

    unload_render_texture canvas;
    close_window ())
