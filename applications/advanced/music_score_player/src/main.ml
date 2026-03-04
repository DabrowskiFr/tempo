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
  bpm : int;
  mutable current_unit : int;
}

type metronome_source = {
  mutable next_pulse_at : float;
  unit_ms : int;
  mutable pending_pulses : int;
}

type score_choice = {
  label : string;
  path : string option;
}

type ui_state = {
  choices : score_choice array;
  mutable dropdown_open : bool;
  mutable selected_index : int;
}

type run_request =
  | Continue
  | Quit_app
  | Reload_score of int

type note_event =
  | Note_on of instrument * note
  | Note_off of instrument * int
  | Panic

type host_bridge = {
  mutable pending_audio_rev : note_event list;
}

exception Quit_request
exception Reload_score_request of int

let grid_cell_h = 44
let grid_left = 180
let grid_top = 120
let width = 1240
let grid_right_margin = 40
let default_cell_w = 24

let mk_color r g b = Color.create r g b 255

let unit_ms_of_bpm bpm =
  max 25 (int_of_float (15000.0 /. float_of_int bpm))

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
  grid_top + (Array.length score.voices * grid_cell_h) + 160

let clamp lo hi v = max lo (min hi v)

let viewport_of_frame (score : score_data) (frame : frame) =
  let grid_width = width - grid_left - grid_right_margin in
  let cell_w = default_cell_w in
  let visible_units = max 1 (grid_width / cell_w) in
  let max_start = max 0 (score.total_units - visible_units) in
  let visible_start =
    if score.total_units <= visible_units || frame.current_unit < 0 then 0
    else clamp 0 max_start (frame.current_unit - (visible_units / 3))
  in
  { cell_w; visible_start; visible_units }

let midi_asset_paths () =
  let root = "applications/advanced/music_score_player/assets" in
  if not (Sys.file_exists root) then []
  else
    Sys.readdir root |> Array.to_list
    |> List.filter (fun name ->
           Filename.check_suffix name ".mid" || Filename.check_suffix name ".midi")
    |> List.sort String.compare
    |> List.map (Filename.concat root)

let score_choices () =
  let file_choices =
    midi_asset_paths ()
    |> List.map (fun path -> { label = Filename.basename path; path = Some path })
  in
  Array.of_list ({ label = "Built-in score"; path = None } :: file_choices)

let score_of_choice (choice : score_choice) =
  match choice.path with
  | None -> Score.default
  | Some path -> Score.of_midi_file path

let score_note_count (score : score_data) =
  Score.note_count score

let acceptable_score (score : score_data) =
  Array.length score.voices <= 12
  && score_note_count score <= 2200
  && score.total_units <= 2400
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
        else if choices.(idx).path = Some path then idx
        else loop (idx + 1)
      in
      loop 0
  | [] -> 0

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

let selector_geometry ui =
  let button_x = float_of_int (width - 336) in
  let button_y = 14.0 in
  let button_w = 300.0 in
  let button_h = 42.0 in
  let item_h = 34.0 in
  let list_x = button_x in
  let list_y = button_y +. button_h +. 8.0 in
  let list_h = float_of_int (Array.length ui.choices) *. item_h in
  (button_x, button_y, button_w, button_h, list_x, list_y, item_h, list_h)

let poll_events source (ui : ui_state) =
  let events = ref [] in
  let push ev = events := ev :: !events in
  let choice_count = Array.length ui.choices in
  let select_relative delta =
    if choice_count > 0 then
      let idx = (ui.selected_index + delta + choice_count) mod choice_count in
      ui.selected_index <- idx;
      ui.dropdown_open <- false;
      push (Select_score idx)
  in
  if ui.dropdown_open then (
    if is_key_pressed Key.Down then
      ui.selected_index <- min (Array.length ui.choices - 1) (ui.selected_index + 1);
    if is_key_pressed Key.Up then ui.selected_index <- max 0 (ui.selected_index - 1);
    if is_key_pressed Key.Enter then (
      ui.dropdown_open <- false;
      push (Select_score ui.selected_index));
    if is_key_pressed Key.Escape then ui.dropdown_open <- false);
  if is_key_pressed Key.Left then select_relative (-1);
  if is_key_pressed Key.Right then select_relative 1;
  if is_mouse_button_pressed MouseButton.Left then (
    let mp = get_mouse_position () in
    let mx = Vector2.x mp in
    let my = Vector2.y mp in
    let button_x, button_y, button_w, button_h, list_x, list_y, item_h, _list_h =
      selector_geometry ui
    in
    let prev_x = button_x -. 42.0 in
    let next_x = button_x +. button_w +. 6.0 in
    if point_in_rect mx my prev_x button_y 36.0 button_h then
      select_relative (-1)
    else if point_in_rect mx my next_x button_y 36.0 button_h then
      select_relative 1
    else if ui.dropdown_open then (
      let selection = ref None in
      Array.iteri
        (fun idx _ ->
          let item_y = list_y +. (float_of_int idx *. item_h) in
          if point_in_rect mx my list_x item_y button_w item_h then selection := Some idx)
        ui.choices;
      match !selection with
      | Some idx ->
          ui.dropdown_open <- false;
          ui.selected_index <- idx;
          push (Select_score idx)
      | None ->
          if point_in_rect mx my button_x button_y button_w button_h then
            ui.dropdown_open <- false
          else ui.dropdown_open <- false)
    else if point_in_rect mx my button_x button_y button_w button_h then
      ui.dropdown_open <- true);
  if window_should_close () || is_key_pressed Key.Escape then push Quit;
  if is_key_pressed Key.Space then push Toggle_play;
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

let make_frame transport =
  {
    playing = transport.playing;
    bpm = transport.bpm;
    unit_ms = unit_ms_of_bpm transport.bpm;
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
  watch restart (fun () -> run_from 0 0 ());
  voice_process restart pulse note_events spec ()

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
  let advance_one_unit () =
    let next_unit =
      if transport.current_unit + 1 >= score.total_units then 0
      else transport.current_unit + 1
    in
    transport.current_unit <- next_unit;
    next_unit
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
        emit note_events Panic;
        emit restart ();
        true
    | Select_score idx ->
        emit note_events Panic;
        raise (Reload_score_request idx)
    | Pulse ->
        if transport.playing then (
          let next_unit = advance_one_unit () in
          if next_unit = 0 then (
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
  emit render_request ();
  loop ()

let draw_background (score : score_data) height (viewport : viewport) =
  let cw = viewport.cell_w in
  let visible_end = min score.total_units (viewport.visible_start + viewport.visible_units) in
  let visible_grid_h = (Array.length score.voices * grid_cell_h) + 16 in
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
      (grid_top + (Array.length score.voices * grid_cell_h) + 8)
      (if is_bar then Color.create 255 244 178 210
       else Color.create 255 255 255 18);
    if is_bar then
      draw_line (xx + 1) (grid_top - 8) (xx + 1)
        (grid_top + (Array.length score.voices * grid_cell_h) + 8)
        (Color.create 255 244 178 130);
    if is_bar then
      draw_text
        (string_of_int ((x / score.units_per_bar) + 1))
        (xx + 8) (grid_top - 32) 18 (Color.create 255 244 178 255)
  done

let draw_score_selector (ui : ui_state) =
  let button_x, button_y, button_w, button_h, list_x, list_y, item_h, list_h =
    selector_geometry ui
  in
  let prev_x = button_x -. 42.0 in
  let next_x = button_x +. button_w +. 6.0 in
  let draw_small_button x label =
    draw_rectangle_rounded (Rectangle.create x button_y 36.0 button_h) 0.2 8
      (Color.create 255 255 255 22);
    draw_rectangle_rounded_lines (Rectangle.create x button_y 36.0 button_h) 0.2 8
      (mk_color 189 210 235);
    draw_text_centered label (int_of_float (x +. 18.0)) (int_of_float button_y + 11) 20
      Color.raywhite
  in
  draw_small_button prev_x "<";
  draw_rectangle_rounded
    (Rectangle.create button_x button_y button_w button_h)
    0.2 8 (Color.create 255 255 255 22);
  draw_rectangle_rounded_lines
    (Rectangle.create button_x button_y button_w button_h)
    0.2 8 (mk_color 189 210 235);
  draw_text "Score" (int_of_float button_x + 12) (int_of_float button_y + 5) 14
    (mk_color 189 210 235);
  draw_text ui.choices.(ui.selected_index).label (int_of_float button_x + 12)
    (int_of_float button_y + 20) 18
    Color.raywhite;
  draw_text (if ui.dropdown_open then "v" else ">")
    (int_of_float (button_x +. button_w -. 22.0)) (int_of_float button_y + 12) 18
    (mk_color 189 210 235);
  draw_small_button next_x ">";
  if ui.dropdown_open then (
    draw_rectangle_rounded
      (Rectangle.create list_x list_y button_w list_h)
      0.08 8 (Color.create 12 16 24 242);
    draw_rectangle_lines (int_of_float list_x) (int_of_float list_y)
      (int_of_float button_w) (int_of_float list_h) (mk_color 189 210 235);
    Array.iteri
      (fun idx choice ->
        let y = list_y +. (float_of_int idx *. item_h) in
        let bg =
          if idx = ui.selected_index then Color.create 255 255 255 32
          else Color.create 255 255 255 16
        in
        draw_rectangle (int_of_float list_x) (int_of_float y) (int_of_float button_w)
          (int_of_float item_h) bg;
        draw_text choice.label (int_of_float list_x + 12) (int_of_float y + 8) 18
          Color.raywhite)
      ui.choices)

let draw_header (score : score_data) height (frame : frame) (viewport : viewport)
    (ui : ui_state) =
  draw_text "Tempo Advanced Music Score Player" 24 20 28 Color.raywhite;
  draw_text
    "execute + watch + parallel + when_ + await_immediate + MIDI import"
    24 52 18 (mk_color 189 210 235);
  let status = if frame.playing then "PLAYING" else "PAUSED" in
  draw_text
    (Printf.sprintf
       "Score: %s | Status: %s | BPM: %d | Unit: %d ms (%s) | Meter: %d/%d | View: %d-%d"
       score.title status frame.bpm frame.unit_ms score.unit_label
       score.time_signature_num score.time_signature_den viewport.visible_start
       (min score.total_units (viewport.visible_start + viewport.visible_units)))
    24 82 18 (mk_color 220 232 244);
  draw_text "SPACE play/pause | R restart | ESC quit"
    24 (height - 40) 18 (mk_color 198 216 234);
  draw_score_selector ui

let draw_voice_row (_score : score_data) (frame : frame) (viewport : viewport) row
    (spec : score_voice) =
  let cw = viewport.cell_w in
  let y = grid_top + (row * grid_cell_h) in
  let color = color_of_index row in
  let visible_end = viewport.visible_start + viewport.visible_units in
  draw_text spec.name 26 (y + 10) 20 color;
  draw_rectangle (grid_left - 8) y ((viewport.visible_units * cw) + 16) grid_cell_h
    (Color.create 255 255 255 10);
  Array.iter
    (fun (note : note) ->
      let note_end = note.start_unit + note.duration_units in
      let draw_start = max note.start_unit viewport.visible_start in
      let draw_end = min note_end visible_end in
      if draw_start < draw_end then (
        let x = grid_left + ((draw_start - viewport.visible_start) * cw) in
        let w = max 10 (((draw_end - draw_start) * cw) - 4) in
        let base =
          Color.create (Color.r color) (Color.g color) (Color.b color) 105
        in
        draw_rectangle x (y + 8) w (grid_cell_h - 16) base;
        draw_rectangle_lines x (y + 8) w (grid_cell_h - 16) color))
    spec.notes;
  let active_notes =
    if frame.playing then
      spec.notes
      |> Array.to_list
      |> List.filter (fun note ->
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
        draw_rectangle x (y + 8 + offset) w (grid_cell_h - 16 - offset) color))
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
          (x + (w / 2)) (y + 12) 16 Color.raywhite

let draw_playhead (score : score_data) (frame : frame) (viewport : viewport) =
  let cw = viewport.cell_w in
  let step = max 0 frame.current_unit in
  if step >= viewport.visible_start && step <= viewport.visible_start + viewport.visible_units then
    let x = grid_left + ((step - viewport.visible_start) * cw) in
    draw_rectangle (x - 2) (grid_top - 12) 4
      ((Array.length score.voices * grid_cell_h) + 24)
      (Color.create 255 244 178 255)

let draw_legend (score : score_data) _height =
  let y = grid_top + (Array.length score.voices * grid_cell_h) + 24 in
  draw_rectangle_rounded
    (Rectangle.create 20.0 (float_of_int y) (float_of_int (width - 40)) 80.0)
    0.15 8 (Color.create 255 255 255 18);
  draw_rectangle_rounded_lines
    (Rectangle.create 20.0 (float_of_int y) (float_of_int (width - 40)) 80.0)
    0.15 8 (mk_color 189 210 235);
  draw_text "Interpretation" 34 (y + 12) 22 Color.raywhite;
  draw_text
    "Tempo imports MIDI events, derives its own logical unit, and schedules note_on / note_off as synchronous processes."
    34 (y + 40) 18 (mk_color 224 235 245);
  draw_text
    "FluidSynth provides MIDI parsing and pleasant SoundFont rendering, but Tempo still owns pause, restart, tempo changes and the logical timeline."
    34 (y + 64) 18 (mk_color 224 235 245)

let draw_frame score height ui (frame : frame) =
  let viewport = viewport_of_frame score frame in
  begin_drawing ();
  draw_background score height viewport;
  draw_header score height frame viewport ui;
  Array.iteri (draw_voice_row score frame viewport) score.voices;
  draw_playhead score frame viewport;
  draw_legend score height;
  end_drawing ()

let apply_audio_commands synth commands =
  List.iter
    (function
      | Panic ->
          for channel = 0 to 15 do
            Synth.all_notes_off synth ~channel
          done
      | Note_on (instrument, note) ->
          Synth.note_on synth ~channel:instrument.channel ~key:note.midi
            ~velocity:(velocity_of_volume note.volume)
      | Note_off (instrument, key) ->
          Synth.note_off synth ~channel:instrument.channel ~key)
    commands

let render_output score height ui synth bridge (frame : frame) =
  let audio_commands = List.rev bridge.pending_audio_rev in
  bridge.pending_audio_rev <- [];
  apply_audio_commands synth audio_commands;
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
      playing = true;
      bpm = score.initial_bpm;
      current_unit = -1;
    }
  in
  parallel
    ((fun () ->
       control_process score pulse restart render_request note_events transport source input ())
    :: (fun () -> audio_bridge_process note_events bridge ())
    :: (fun () -> render_process render_request transport output ())
    :: Array.to_list
         (Array.mapi
            (fun _voice_index spec -> fun () -> voice_process restart pulse note_events spec ())
            score.voices))

let () =
  let choices = score_choices () in
  let ui =
    {
      choices;
      dropdown_open = false;
      selected_index = initial_selected_index choices;
    }
  in
  let initial_score = score_of_choice ui.choices.(ui.selected_index) in
  let height = max 680 (height_of_score initial_score) in
  init_window width height "Tempo Advanced Music Score Player";
  set_target_fps 60;
  let rec run_selected () =
    let score = load_score ui.choices.(ui.selected_index) in
    let synth = Synth.create ~gain:0.7 () in
    let bridge = { pending_audio_rev = [] } in
    Array.iter
      (fun spec ->
        Synth.program_select synth ~channel:spec.instrument.channel
          ~bank:spec.instrument.bank ~preset:spec.instrument.preset)
      score.voices;
    let source = create_source (unit_ms_of_bpm score.initial_bpm) in
    let input = make_interactive_source source ui in
    draw_frame score height ui
      {
        playing = true;
        bpm = score.initial_bpm;
        unit_ms = unit_ms_of_bpm score.initial_bpm;
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
        ui.dropdown_open <- false;
        run_selected ()
  in
  run_selected ();
  close_window ()
