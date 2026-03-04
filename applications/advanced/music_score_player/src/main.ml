open Tempo
open Raylib

module Synth = Tempo_fluidsynth
module Score = Tempo_score
open Score

type note = Score.note
type instrument = Score.instrument
type score_voice = Score.voice
type score_data = Score.t

type host_event =
  | Pulse
  | Toggle_play
  | Restart
  | Select_score of int
  | Select_soundfont of int
  | Quit

type frame = {
  playing : bool;
  bpm : int;
  unit_ms : int;
  current_unit : int;
}

type viewport = {
  cell_w : int;
  visible_start : int;
  visible_units : int;
}

type transport = {
  mutable playing : bool;
  mutable bpm : int;
  mutable unit_ms : int;
  mutable current_unit : int;
}

type metronome_source = {
  mutable next_pulse_at : float;
  mutable unit_ms : int;
  mutable pending_pulses : int;
}

type score_choice = {
  label : string;
  path : string;
}

type soundfont_choice = {
  label : string;
  sf_path : string;
}

type ui_state = {
  choices : score_choice array;
  soundfonts : soundfont_choice array;
  mutable selected_index : int;
  mutable selected_soundfont : int;
  mutable score_scroll_col : int;
  mutable last_toggle_at : float;
}

type run_request =
  | Continue
  | Quit_app
  | Reload_score of int
  | Reload_soundfont of int

type note_event =
  | Note_on of instrument * note
  | Note_off of instrument * int
  | Control_cc of int * int * int
  | Panic

type host_bridge = {
  mutable pending_audio_rev : note_event list;
  active_notes : int array array;
}

exception Quit_request
exception Reload_score_request of int
exception Reload_soundfont_request of int

let grid_cell_h_default = 44
let grid_left = 180
let grid_top = 120
let width = 1240
let grid_right_margin = 40
let default_cell_w = 24

let mk_color r g b = Color.create r g b 255

let unit_ms_of_score_bpm (score : score_data) bpm =
  let bpm = max 1 bpm in
  let quarter_ms = 60000.0 /. float_of_int bpm in
  let bar_quarters =
    (float_of_int score.time_signature_num *. 4.0)
    /. float_of_int (max 1 score.time_signature_den)
  in
  let bar_ms = quarter_ms *. bar_quarters in
  let unit_ms = bar_ms /. float_of_int (max 1 score.units_per_bar) in
  max 1 (int_of_float (Float.round unit_ms))

let draw_text_centered text x y size color =
  let w = measure_text text size in
  draw_text text (x - (w / 2)) y size color

let velocity_of_volume volume =
  let scaled = int_of_float (volume *. 127.0) in
  max 1 (min 127 scaled)

let color_of_index idx =
  let palette =
    [|
      mk_color 240 124 92;
      mk_color 94 191 161;
      mk_color 102 152 236;
      mk_color 234 188 71;
      mk_color 205 132 225;
      mk_color 117 212 234;
      mk_color 245 117 158;
      mk_color 142 220 109;
    |]
  in
  palette.(idx mod Array.length palette)

let height_of_score (score : score_data) =
  grid_top + (Array.length score.voices * grid_cell_h_default) + 160

let clamp lo hi v = max lo (min hi v)

let row_height_of_score (score : score_data) =
  let voices = max 1 (Array.length score.voices) in
  let list_h = 250.0 in
  let list_y = float_of_int (get_screen_height ()) -. list_h -. 70.0 in
  let available = int_of_float list_y - grid_top - 18 in
  clamp 20 grid_cell_h_default (available / voices)

let viewport_of_frame (score : score_data) (frame : frame) =
  let grid_width = width - grid_left - grid_right_margin in
  let voices = max 1 (Array.length score.voices) in
  let density_scale =
    if voices <= 10 then 1.0
    else max 0.45 (10.0 /. float_of_int voices)
  in
  let cell_w = max 10 (int_of_float (Float.round (float_of_int default_cell_w *. density_scale))) in
  let visible_units = max 1 (grid_width / cell_w) in
  let max_start = max 0 (score.total_units - visible_units) in
  let visible_start =
    if score.total_units <= visible_units || frame.current_unit < 0 then 0
    else clamp 0 max_start (frame.current_unit - (visible_units / 3))
  in
  { cell_w; visible_start; visible_units }

let score_asset_paths () =
  let root = "applications/advanced/music_score_player/assets/tscore" in
  if not (Sys.file_exists root) then []
  else
    Sys.readdir root |> Array.to_list
    |> List.filter (fun name ->
           Filename.check_suffix name ".tscore"
           || Filename.check_suffix name ".tbin"
           || Filename.check_suffix name ".tempo-scoreb")
    |> List.sort String.compare
    |> List.map (Filename.concat root)

let score_choices () =
  score_asset_paths ()
  |> List.map (fun path -> { label = Filename.basename path; path })
  |> Array.of_list

let soundfont_asset_paths () =
  let root = "applications/advanced/music_score_player/assets/soundfonts" in
  if not (Sys.file_exists root) then []
  else
    Sys.readdir root |> Array.to_list
    |> List.filter (fun name ->
           Filename.check_suffix name ".sf2" || Filename.check_suffix name ".sf3")
    |> List.sort String.compare
    |> List.map (Filename.concat root)

let soundfont_choices () =
  soundfont_asset_paths ()
  |> List.map (fun path ->
         {
           label = Filename.basename path;
           sf_path = path;
         })
  |> Array.of_list

let score_of_choice (choice : score_choice) =
  Score.of_binary_file choice.path

let score_note_count (score : score_data) =
  Score.note_count score

let acceptable_score (score : score_data) =
  Array.length score.voices <= 16
  && score_note_count score <= 12000
  && Array.length score.controls <= 24000
  && Array.length score.tempo_changes <= 4000
  && score.total_units <= 12000
  && score.units_per_bar > 0
  && score.units_per_bar <= 64

let load_score (choice : score_choice) =
  try score_of_choice choice
      |> fun score ->
      if acceptable_score score then score
      else (
        Printf.eprintf
          "score %s rejected: too dense for interactive showcase (voices=%d notes=%d units=%d bar=%d)\n\
           %!"
          choice.label (Array.length score.voices) (score_note_count score)
          score.total_units score.units_per_bar;
        Score.default)
  with exn ->
    Printf.eprintf "failed to load score %s: %s\n%!" choice.label
      (Printexc.to_string exn);
    Score.default

let initial_selected_index choices =
  match Sys.argv |> Array.to_list |> List.tl with
  | path :: _ ->
      let rec loop idx =
        if idx >= Array.length choices then 0
        else if choices.(idx).path = path then idx
        else loop (idx + 1)
      in
      loop 0
  | [] -> 0

let initial_soundfont_index soundfonts =
  let find_label label =
    let rec loop idx =
      if idx >= Array.length soundfonts then None
      else if String.equal soundfonts.(idx).label label then Some idx
      else loop (idx + 1)
    in
    loop 0
  in
  match Sys.argv |> Array.to_list |> List.tl with
  | _score_arg :: soundfont_arg :: _ -> (
      let rec loop idx =
        if idx >= Array.length soundfonts then 0
        else if soundfonts.(idx).sf_path = soundfont_arg then idx
        else loop (idx + 1)
      in
      loop 0)
  | _ ->
      Option.value (find_label "MuseScore_General.sf2")
        ~default:
          (Option.value (find_label "GeneralUser-GS.sf2") ~default:0)

let create_source unit_ms =
  {
    next_pulse_at = Unix.gettimeofday () +. (float_of_int unit_ms /. 1000.0);
    unit_ms;
    pending_pulses = 0;
  }

let stop_source _source = ()

let reset_metronome_clock source =
  source.next_pulse_at <- Unix.gettimeofday () +. (float_of_int source.unit_ms /. 1000.0);
  source.pending_pulses <- 0

let point_in_rect px py x y w h =
  px >= x && px <= x +. w && py >= y && py <= y +. h

let selector_geometry () =
  let list_x = 20.0 in
  let list_w = float_of_int (width - 40) in
  let list_h = 250.0 in
  let list_y = float_of_int (get_screen_height ()) -. list_h -. 70.0 in
  let title_h = 22.0 in
  let item_h = 34.0 in
  (list_x, list_y, list_w, list_h, title_h, item_h)

let visible_choice_rows list_h title_h item_h =
  max 1 (int_of_float ((list_h -. title_h -. 18.0) /. item_h))

let max_choice_row_scroll (ui : ui_state) visible_rows =
  max 0 (Array.length ui.choices - visible_rows)

let clamp_choice_scroll ui visible_rows =
  ui.score_scroll_col <- clamp 0 (max_choice_row_scroll ui visible_rows) ui.score_scroll_col

let poll_events source (ui : ui_state) =
  let events = ref [] in
  let push ev = events := ev :: !events in
  let choice_count = Array.length ui.choices in
  let soundfont_count = Array.length ui.soundfonts in
  let list_x, list_y, list_w, list_h, title_h, item_h = selector_geometry () in
  let visible_rows = visible_choice_rows list_h title_h item_h in
  let ensure_visible idx =
    if idx < ui.score_scroll_col then ui.score_scroll_col <- idx
    else if idx >= ui.score_scroll_col + visible_rows then
      ui.score_scroll_col <- idx - visible_rows + 1
  in
  let select_relative delta =
    if choice_count > 0 then
      let idx = (ui.selected_index + delta + choice_count) mod choice_count in
      ui.selected_index <- idx;
      ensure_visible idx;
      push (Select_score idx)
  in
  let select_soundfont_relative delta =
    if soundfont_count > 0 then
      let idx =
        (ui.selected_soundfont + delta + soundfont_count) mod soundfont_count
      in
      ui.selected_soundfont <- idx;
      push (Select_soundfont idx)
  in
  if is_key_pressed Key.Left then select_relative (-1);
  if is_key_pressed Key.Right then select_relative 1;
  if is_key_pressed Key.Down then select_relative 1;
  if is_key_pressed Key.Up then select_relative (-1);
  if is_key_pressed Key.F5 then select_soundfont_relative (-1);
  if is_key_pressed Key.F6 then select_soundfont_relative 1;
  clamp_choice_scroll ui visible_rows;
  let wheel = get_mouse_wheel_move () in
  if wheel <> 0.0 then (
    let mp = get_mouse_position () in
    if point_in_rect (Vector2.x mp) (Vector2.y mp) list_x list_y list_w list_h then (
      ui.score_scroll_col <- ui.score_scroll_col - int_of_float wheel;
      clamp_choice_scroll ui visible_rows));
  if is_mouse_button_pressed MouseButton.Left then (
    let mp = get_mouse_position () in
    let mx = Vector2.x mp in
    let my = Vector2.y mp in
    if point_in_rect mx my list_x list_y list_w list_h then (
      let row0_y = list_y +. title_h +. 4.0 in
      if my >= row0_y then (
        let rel = int_of_float ((my -. row0_y) /. item_h) in
        let idx = ui.score_scroll_col + rel in
        if idx >= 0 && idx < Array.length ui.choices then (
          ui.selected_index <- idx;
          ensure_visible idx;
          push (Select_score idx))));
    let max_scroll = max_choice_row_scroll ui visible_rows in
    if max_scroll > 0 then (
      let track_x = list_x +. list_w -. 10.0 in
      let track_y = list_y +. title_h +. 4.0 in
      let track_h = list_h -. title_h -. 18.0 in
      if point_in_rect mx my track_x track_y 6.0 track_h then (
        let thumb_h =
          max 24.0
            (track_h *. (float_of_int visible_rows /. float_of_int (Array.length ui.choices)))
        in
        let ratio =
          clamp 0 1000
            (int_of_float
               (((my -. track_y -. (thumb_h /. 2.0)) /. (track_h -. thumb_h))
               *. 1000.0))
        in
        ui.score_scroll_col <- (ratio * max_scroll) / 1000;
        clamp_choice_scroll ui visible_rows)));
  if window_should_close () || is_key_pressed Key.Escape then push Quit;
  if is_key_pressed Key.Space then (
    let now = Unix.gettimeofday () in
    if now -. ui.last_toggle_at >= 0.18 then (
      ui.last_toggle_at <- now;
      push Toggle_play));
  if is_key_pressed Key.R then push Restart;
  let now = Unix.gettimeofday () in
  while now >= source.next_pulse_at do
    source.pending_pulses <- source.pending_pulses + 1;
    source.next_pulse_at <- source.next_pulse_at +. (float_of_int source.unit_ms /. 1000.0)
  done;
  let events = List.rev !events in
  let pulse_events =
    if source.pending_pulses > 0 then (
      source.pending_pulses <- source.pending_pulses - 1;
      [ Pulse ])
    else []
  in
  match events @ pulse_events with
  | [] -> None
  | batch -> Some batch

let make_interactive_source source ui =
  {
    poll =
      (fun () ->
        poll_events source ui);
    wait =
      (fun () ->
        let now = Unix.gettimeofday () in
        let until_pulse = source.next_pulse_at -. now in
        let sleep_s = max 0.0 (min (1.0 /. 120.0) until_pulse) in
        if sleep_s > 0.0 then Unix.sleepf sleep_s;
        Option.iter Tempo.notify_wakeup (Tempo.current_wakeup ()));
  }

let make_frame (transport : transport) : frame =
  {
    playing = transport.playing;
    bpm = transport.bpm;
    unit_ms = transport.unit_ms;
    current_unit = transport.current_unit;
  }

let reset_transport transport source =
  transport.current_unit <- -1;
  reset_metronome_clock source

let rec wait_pulses pulse count () =
  if count <= 0 then ()
  else (
    ignore (await pulse);
    wait_pulses pulse (count - 1) ())

let note_release_process restart pulse note_events spec (note : note) () =
  watch restart (fun () ->
      wait_pulses pulse note.duration_units ();
      emit note_events (Note_off (spec.instrument, note.midi)))

let trigger_note restart pulse note_events spec (note : note) =
  emit note_events (Note_on (spec.instrument, note));
  ignore
    (Low_level.fork (fun () ->
         note_release_process restart pulse note_events spec note ()))

let rec voice_process restart pulse note_events (spec : score_voice) () =
  let rec run_from index current_unit () =
    if index >= Array.length spec.notes then ()
    else
      let start_unit = spec.notes.(index).start_unit in
      wait_pulses pulse (start_unit - current_unit) ();
      let rec launch_same_start idx =
        if idx < Array.length spec.notes && spec.notes.(idx).start_unit = start_unit then (
          trigger_note restart pulse note_events spec spec.notes.(idx);
          launch_same_start (idx + 1))
        else idx
      in
      run_from (launch_same_start index) start_unit ()
  in
  watch restart (fun () -> run_from 0 (-1) ());
  voice_process restart pulse note_events spec ()

let rec control_process_timeline restart pulse note_events
    (controls : Score.control array) () =
  let rec run_from index current_unit () =
    if index >= Array.length controls then ()
    else
      let start_unit = controls.(index).start_unit in
      wait_pulses pulse (start_unit - current_unit) ();
      let rec launch_same_start idx =
        if idx < Array.length controls && controls.(idx).start_unit = start_unit then (
          let c = controls.(idx) in
          emit note_events (Control_cc (c.channel, c.control, c.value));
          launch_same_start (idx + 1))
        else idx
      in
      run_from (launch_same_start index) start_unit ()
  in
  watch restart (fun () -> run_from 0 (-1) ());
  control_process_timeline restart pulse note_events controls ()

let audio_bridge_process note_events bridge () =
  let rec loop () =
    let batch = List.rev (await note_events) in
    bridge.pending_audio_rev <- List.rev_append batch bridge.pending_audio_rev;
    loop ()
  in
  loop ()

let render_process render_request transport output () =
  let rec loop () =
    ignore (await render_request);
    pause ();
    emit output (make_frame transport);
    loop ()
  in
  loop ()

let control_process score pulse restart render_request note_events transport source input () =
  let tempo_cursor = ref 0 in
  let tempo_len = Array.length score.tempo_changes in
  let set_bpm bpm =
    let bpm = clamp 20 300 bpm in
    let unit_ms = unit_ms_of_score_bpm score bpm in
    if bpm <> transport.bpm || unit_ms <> transport.unit_ms then (
      transport.bpm <- bpm;
      transport.unit_ms <- unit_ms;
      source.unit_ms <- unit_ms;
      source.next_pulse_at <- Unix.gettimeofday () +. (float_of_int unit_ms /. 1000.0))
  in
  let apply_tempo_at_unit unit =
    while !tempo_cursor < tempo_len && score.tempo_changes.(!tempo_cursor).start_unit < unit do
      incr tempo_cursor
    done;
    while !tempo_cursor < tempo_len && score.tempo_changes.(!tempo_cursor).start_unit = unit do
      set_bpm score.tempo_changes.(!tempo_cursor).bpm;
      incr tempo_cursor
    done
  in
  let reset_tempo () =
    tempo_cursor := 0;
    set_bpm score.initial_bpm;
    apply_tempo_at_unit 0
  in
  let advance_one_unit () =
    let prev_unit = transport.current_unit in
    let next_unit =
      if transport.current_unit + 1 >= score.total_units then 0
      else transport.current_unit + 1
    in
    transport.current_unit <- next_unit;
    let wrapped = prev_unit >= 0 && next_unit = 0 in
    if wrapped then reset_tempo () else apply_tempo_at_unit next_unit;
    (next_unit, wrapped)
  in
  let handle event =
    match event with
    | Quit ->
        emit note_events Panic;
        raise Quit_request
    | Toggle_play ->
        transport.playing <- not transport.playing;
        if not transport.playing then emit note_events Panic;
        true
    | Restart ->
        reset_transport transport source;
        reset_tempo ();
        emit note_events Panic;
        emit restart ();
        true
    | Select_score idx ->
        emit note_events Panic;
        raise (Reload_score_request idx)
    | Select_soundfont idx ->
        emit note_events Panic;
        raise (Reload_soundfont_request idx)
    | Pulse ->
        if transport.playing then (
          let _next_unit, wrapped = advance_one_unit () in
          if wrapped then (
            emit note_events Panic;
            emit restart ());
          emit pulse ();
          true)
        else true
  in
  let rec loop () =
    when_ input (fun () ->
        let batch = await_immediate input in
        let needs_render = List.fold_left (fun acc event -> handle event || acc) false batch in
        if needs_render then emit render_request ();
        pause ();
        loop ())
  in
  pause ();
  reset_tempo ();
  emit render_request ();
  loop ()

let draw_background (score : score_data) height (viewport : viewport) row_h =
  let cw = viewport.cell_w in
  let visible_end = min score.total_units (viewport.visible_start + viewport.visible_units) in
  let visible_grid_h = (Array.length score.voices * row_h) + 16 in
  clear_background (mk_color 15 18 28);
  draw_rectangle_gradient_v 0 0 width height (mk_color 25 31 48) (mk_color 11 14 22);
  if score.units_per_bar > 0 then (
    let first_bar = viewport.visible_start / score.units_per_bar in
    let last_bar = visible_end / score.units_per_bar in
    for bar = first_bar to last_bar do
      let bar_start = bar * score.units_per_bar in
      let bar_end = min score.total_units ((bar + 1) * score.units_per_bar) in
      let draw_start = max bar_start viewport.visible_start in
      let draw_end = min bar_end visible_end in
      if draw_start < draw_end then (
        let x = grid_left + ((draw_start - viewport.visible_start) * cw) in
        let w = max 1 ((draw_end - draw_start) * cw) in
        let bg =
          if bar mod 2 = 0 then Color.create 255 210 120 20
          else Color.create 120 170 255 12
        in
        draw_rectangle x (grid_top - 6) w (visible_grid_h + 8) bg;
        draw_rectangle x (grid_top - 34) w 18 (Color.create 255 244 178 22)
      )
    done);
  for x = viewport.visible_start to visible_end do
    let xx = grid_left + ((x - viewport.visible_start) * cw) in
    let is_bar =
      score.units_per_bar > 0 && x < score.total_units && x mod score.units_per_bar = 0
    in
    draw_line xx (grid_top - 8) xx
      (grid_top + (Array.length score.voices * row_h) + 8)
      (if is_bar then Color.create 255 244 178 210
       else Color.create 255 255 255 18);
    if is_bar then
      draw_line (xx + 1) (grid_top - 8) (xx + 1)
        (grid_top + (Array.length score.voices * row_h) + 8)
        (Color.create 255 244 178 130);
    if is_bar then
      draw_text
        (string_of_int ((x / score.units_per_bar) + 1))
        (xx + 8) (grid_top - 32) 18 (Color.create 255 244 178 255)
  done

let draw_score_selector (ui : ui_state) =
  let list_x, list_y, list_w, list_h, title_h, item_h = selector_geometry () in
  let visible_rows = visible_choice_rows list_h title_h item_h in
  clamp_choice_scroll ui visible_rows;
  let max_scroll = max_choice_row_scroll ui visible_rows in
  draw_rectangle_rounded
    (Rectangle.create list_x list_y list_w list_h)
    0.2 8 (Color.create 255 255 255 22);
  draw_rectangle_rounded_lines
    (Rectangle.create list_x list_y list_w list_h)
    0.2 8 (mk_color 189 210 235);
  draw_text "Scores (click to select)" (int_of_float list_x + 10) (int_of_float list_y + 6)
    16 (mk_color 189 210 235);
  let row0_y = list_y +. title_h +. 4.0 in
  for row = 0 to visible_rows - 1 do
    let idx = ui.score_scroll_col + row in
    if idx < Array.length ui.choices then (
      let x = list_x +. 6.0 in
      let y = row0_y +. (float_of_int row *. item_h) in
      let bg =
        if idx = ui.selected_index then Color.create 255 255 255 32
        else Color.create 255 255 255 10
      in
      draw_rectangle (int_of_float x) (int_of_float y) (int_of_float (list_w -. 20.0))
        (int_of_float item_h - 2) bg;
      draw_text ui.choices.(idx).label (int_of_float x + 8) (int_of_float y + 6)
        18 Color.raywhite)
  done;
  if max_scroll > 0 then (
    let track_x = list_x +. list_w -. 10.0 in
    let track_y = row0_y in
    let track_h = list_h -. title_h -. 18.0 in
    let thumb_h =
      max 24.0
        (track_h *. (float_of_int visible_rows /. float_of_int (Array.length ui.choices)))
    in
    let thumb_y =
      track_y
      +. ((float_of_int ui.score_scroll_col /. float_of_int max_scroll) *. (track_h -. thumb_h))
    in
    draw_rectangle (int_of_float track_x) (int_of_float track_y) 6 (int_of_float track_h)
      (Color.create 255 255 255 28);
    draw_rectangle (int_of_float track_x) (int_of_float thumb_y) 6 (int_of_float thumb_h)
      (Color.create 255 255 255 130))

let draw_header (score : score_data) height (frame : frame) (viewport : viewport)
    (ui : ui_state) =
  draw_text "Tempo Music Player" 24 20 28 Color.raywhite;
  let status = if frame.playing then "PLAYING" else "PAUSED" in
  draw_text (Printf.sprintf "Score: %s" score.title) 24 54 18 (mk_color 220 232 244);
  draw_text
    (Printf.sprintf "SoundFont: %s | Status: %s | BPM: %d | Unit: %d ms (%s)"
       ui.soundfonts.(ui.selected_soundfont).label status frame.bpm frame.unit_ms
       score.unit_label)
    24 78 18 (mk_color 220 232 244);
  draw_text
    (Printf.sprintf "Meter: %d/%d | View: %d-%d | Scores: %d"
       score.time_signature_num score.time_signature_den viewport.visible_start
       (min score.total_units (viewport.visible_start + viewport.visible_units))
       (Array.length ui.choices))
    24 102 18 (mk_color 220 232 244);
  draw_text "SPACE start/pause | R restart | Mouse wheel: score scroll | F5/F6 soundfont | ESC quit"
    24 (height - 40) 18 (mk_color 198 216 234);
  draw_score_selector ui

let draw_voice_row (_score : score_data) (frame : frame) (viewport : viewport) row row_h
    (spec : score_voice) =
  let cw = viewport.cell_w in
  let y = grid_top + (row * row_h) in
  let color = color_of_index row in
  let visible_end = viewport.visible_start + viewport.visible_units in
  let font_size = clamp 12 20 (row_h - 14) in
  draw_text spec.name 26 (y + 6) font_size color;
  draw_rectangle (grid_left - 8) y ((viewport.visible_units * cw) + 16) row_h
    (Color.create 255 255 255 10);
  Array.iter
    (fun (note : note) ->
      let note_end = note.start_unit + note.duration_units in
      let draw_start = max note.start_unit viewport.visible_start in
      let draw_end = min note_end visible_end in
      if draw_start < draw_end then (
        let x = grid_left + ((draw_start - viewport.visible_start) * cw) in
        let w = max 10 (((draw_end - draw_start) * cw) - 4) in
        let block_h = max 8 (row_h - 16) in
        let base =
          Color.create (Color.r color) (Color.g color) (Color.b color) 105
        in
        draw_rectangle x (y + 8) w block_h base;
        draw_rectangle_lines x (y + 8) w block_h color))
    spec.notes;
  let active_notes =
    if frame.playing then
      spec.notes
      |> Array.to_list
      |> List.filter (fun (note : note) ->
             note.start_unit <= frame.current_unit
             && frame.current_unit < note.start_unit + note.duration_units)
    else []
  in
  List.iteri
    (fun idx (playing : note) ->
      let note_end = playing.start_unit + playing.duration_units in
      let draw_start = max playing.start_unit viewport.visible_start in
      let draw_end = min note_end visible_end in
      if draw_start < draw_end then (
        let x = grid_left + ((draw_start - viewport.visible_start) * cw) in
        let w = max 10 (((draw_end - draw_start) * cw) - 4) in
        let offset = min 10 (idx * 4) in
        draw_rectangle x (y + 8 + offset) w (max 4 (row_h - 16 - offset)) color))
    active_notes;
  match active_notes with
  | [] -> ()
  | playing :: _ ->
      let note_end = playing.start_unit + playing.duration_units in
      let draw_start = max playing.start_unit viewport.visible_start in
      let draw_end = min note_end visible_end in
      if draw_start < draw_end then
        let x = grid_left + ((draw_start - viewport.visible_start) * cw) in
        let w = max 10 (((draw_end - draw_start) * cw) - 4) in
        draw_text_centered
          (Printf.sprintf "%d notes" (List.length active_notes))
          (x + (w / 2)) (y + 8) (clamp 10 16 (row_h - 18)) Color.raywhite

let draw_playhead (score : score_data) (frame : frame) (viewport : viewport) row_h =
  let cw = viewport.cell_w in
  let step = max 0 frame.current_unit in
  if step >= viewport.visible_start && step <= viewport.visible_start + viewport.visible_units then
    let x = grid_left + ((step - viewport.visible_start) * cw) in
    draw_rectangle (x - 2) (grid_top - 12) 4
      ((Array.length score.voices * row_h) + 24)
      (Color.create 255 244 178 255)

let draw_frame score height ui (frame : frame) =
  let viewport = viewport_of_frame score frame in
  let row_h = row_height_of_score score in
  begin_drawing ();
  draw_background score height viewport row_h;
  draw_header score height frame viewport ui;
  Array.iteri (fun row spec -> draw_voice_row score frame viewport row row_h spec) score.voices;
  draw_playhead score frame viewport row_h;
  end_drawing ()

let apply_audio_commands synth bridge commands =
  let active = bridge.active_notes in
  let clear_active () =
    for ch = 0 to 15 do
      for key = 0 to 127 do
        active.(ch).(key) <- 0
      done
    done
  in
  let order = function
    | Panic -> 0
    | Note_off _ -> 1
    | Control_cc _ -> 2
    | Note_on _ -> 3
  in
  let stable_sort lst =
    lst
    |> List.mapi (fun i ev -> (i, ev))
    |> List.sort (fun (ia, a) (ib, b) ->
           match compare (order a) (order b) with
           | 0 -> compare ia ib
           | n -> n)
    |> List.map snd
  in
  stable_sort commands
  |> List.iter (function
       | Panic ->
           for channel = 0 to 15 do
             Synth.all_notes_off synth ~channel
           done;
           clear_active ()
       | Note_on (instrument, note) ->
           let ch = clamp 0 15 instrument.channel in
           let key = clamp 0 127 note.midi in
           active.(ch).(key) <- active.(ch).(key) + 1;
           Synth.note_on synth ~channel:instrument.channel ~key:note.midi
             ~velocity:(velocity_of_volume note.volume)
       | Note_off (instrument, key) ->
           let ch = clamp 0 15 instrument.channel in
           let key = clamp 0 127 key in
           let count = active.(ch).(key) in
           if count <= 1 then (
             active.(ch).(key) <- 0;
             Synth.note_off synth ~channel:instrument.channel ~key)
           else active.(ch).(key) <- count - 1
       | Control_cc (channel, control, value) ->
           Synth.control_change synth ~channel ~control ~value)

let configure_instrument synth (spec : score_voice) =
  let channel = spec.instrument.channel in
  let preset = spec.instrument.preset in
  let attempts =
    [ (spec.instrument.bank, preset); (0, preset); (0, 0) ]
  in
  let rec try_attempts = function
    | [] ->
        Printf.eprintf
          "program_select failed for channel=%d bank=%d preset=%d; keeping default patch\n\
           %!"
          channel spec.instrument.bank preset
    | (bank, prog) :: rest -> (
        try Synth.program_select synth ~channel ~bank ~preset:prog
        with Failure _ | Invalid_argument _ -> try_attempts rest)
  in
  try_attempts attempts

let render_output score height ui synth bridge (frame : frame) =
  let audio_commands = List.rev bridge.pending_audio_rev in
  bridge.pending_audio_rev <- [];
  apply_audio_commands synth bridge audio_commands;
  draw_frame score height ui frame

let main_program score bridge source input output =
  let restart = new_signal () in
  let pulse = new_signal () in
  let render_request = new_signal_agg ~initial:false ~combine:(fun _ () -> true) in
  let note_events =
    new_signal_agg ~initial:[] ~combine:(fun acc ev -> ev :: acc)
  in
  let transport =
    {
      playing = false;
      bpm = score.initial_bpm;
      unit_ms = unit_ms_of_score_bpm score score.initial_bpm;
      current_unit = -1;
    }
  in
  let control_threads =
    if Array.length score.controls = 0 then []
    else [ (fun () -> control_process_timeline restart pulse note_events score.controls ()) ]
  in
  let voice_threads =
    Array.to_list
      (Array.mapi
         (fun _voice_index spec ->
           fun () -> voice_process restart pulse note_events spec ())
         score.voices)
  in
  parallel
    ([ (fun () ->
         control_process score pulse restart render_request note_events transport source
           input ());
       (fun () -> audio_bridge_process note_events bridge ());
       (fun () -> render_process render_request transport output ());
     ]
    @ control_threads @ voice_threads)

let () =
  let choices = score_choices () in
  if Array.length choices = 0 then
    failwith
      "No score found in applications/advanced/music_score_player/assets/tscore";
  let soundfonts = soundfont_choices () in
  if Array.length soundfonts = 0 then
    failwith
      "No soundfont found in applications/advanced/music_score_player/assets/soundfonts";
  let ui =
    {
      choices;
      soundfonts;
      selected_index = initial_selected_index choices;
      selected_soundfont = initial_soundfont_index soundfonts;
      score_scroll_col = 0;
      last_toggle_at = 0.0;
    }
  in
  let initial_score = score_of_choice ui.choices.(ui.selected_index) in
  let height = max 680 (height_of_score initial_score) in
  init_window width height "Tempo Music Player";
  set_target_fps 60;
  let rec run_selected () =
    let score = load_score ui.choices.(ui.selected_index) in
    let selected_sf_idx = ui.selected_soundfont in
    ui.selected_soundfont <- selected_sf_idx;
    let soundfont_path = ui.soundfonts.(selected_sf_idx).sf_path in
    let synth = Synth.create ~soundfont:soundfont_path ~gain:0.7 () in
    let bridge =
      { pending_audio_rev = []; active_notes = Array.init 16 (fun _ -> Array.make 128 0) }
    in
    Array.iter (configure_instrument synth) score.voices;
    let source = create_source (unit_ms_of_score_bpm score score.initial_bpm) in
    let input = make_interactive_source source ui in
    draw_frame score height ui
      {
        playing = false;
        bpm = score.initial_bpm;
        unit_ms = unit_ms_of_score_bpm score score.initial_bpm;
        current_unit = -1;
      };
    let next_action =
      try
        run_interactive ~input ~output:(render_output score height ui synth bridge)
          (main_program score bridge source);
        Continue
      with
      | Quit_request -> Quit_app
      | Reload_score_request idx -> Reload_score idx
      | Reload_soundfont_request idx -> Reload_soundfont idx
    in
    stop_source source;
    Array.iter
      (fun spec -> Synth.all_notes_off synth ~channel:spec.instrument.channel)
      score.voices;
    Synth.shutdown synth;
    match next_action with
    | Continue | Quit_app -> ()
    | Reload_score idx ->
        ui.selected_index <- idx;
        run_selected ()
    | Reload_soundfont idx ->
        ui.selected_soundfont <- idx;
        run_selected ()
  in
  run_selected ();
  close_window ()
