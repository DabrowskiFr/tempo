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
  mutable is_ready : bool;
}

type run_request =
  | Continue
  | Quit_app
  | Reload_score of int
  | Reload_soundfont of int

type lifecycle_request =
  | Request_quit
  | Request_reload_score of int
  | Request_reload_soundfont of int

exception Quit_request
exception Reload_score_request of int
exception Reload_soundfont_request of int

let clamp lo hi v = max lo (min hi v)

module Audio_bridge = struct
  type command =
    | Note_on of instrument * note
    | Note_off of instrument * int
    | Control_cc of int * int * int
    | Panic

  type t = {
    mutable pending_rev : command list;
    active_notes : int array array;
  }

  let create () =
    { pending_rev = []; active_notes = Array.init 16 (fun _ -> Array.make 128 0) }

  let collect_process cmd_bus bridge () =
    let rec loop () =
      let batch = List.rev (await cmd_bus) in
      bridge.pending_rev <- List.rev_append batch bridge.pending_rev;
      loop ()
    in
    loop ()

  let drain bridge =
    let out = List.rev bridge.pending_rev in
    bridge.pending_rev <- [];
    out

  let velocity_of_volume volume =
    let scaled = int_of_float (volume *. 127.0) in
    max 1 (min 127 scaled)

  let apply synth bridge commands =
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
         | Note_on (inst, n) ->
             let ch = clamp 0 15 inst.channel in
             let key = clamp 0 127 n.midi in
             active.(ch).(key) <- active.(ch).(key) + 1;
             Synth.note_on synth ~channel:inst.channel ~key:n.midi
               ~velocity:(velocity_of_volume n.volume)
         | Note_off (inst, key) ->
             let ch = clamp 0 15 inst.channel in
             let key = clamp 0 127 key in
             let count = active.(ch).(key) in
             if count <= 1 then (
               active.(ch).(key) <- 0;
               Synth.note_off synth ~channel:inst.channel ~key)
             else active.(ch).(key) <- count - 1
         | Control_cc (channel, control, value) ->
             Synth.control_change synth ~channel ~control ~value)

  let configure_instrument synth (spec : score_voice) =
    let channel = spec.instrument.channel in
    let preset = spec.instrument.preset in
    let attempts = [ (spec.instrument.bank, preset); (0, preset); (0, 0) ] in
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
end

module Score_reactive = struct
  let emit_all_matching arr idx_pred emitf start_idx =
    let rec loop idx =
      if idx < Array.length arr && idx_pred arr.(idx) then (
        emitf arr.(idx);
        loop (idx + 1))
      else idx
    in
    loop start_idx

  let voice_process restart tick cmd_bus (spec : score_voice) () =
    let end_sorted =
      spec.notes
      |> Array.to_list
      |> List.sort (fun (a : note) (b : note) ->
             compare (a.start_unit + a.duration_units) (b.start_unit + b.duration_units))
      |> Array.of_list
    in
    let rec run i_on i_off () =
      let unit = await tick in
      let i_off =
        emit_all_matching end_sorted
          (fun n -> n.start_unit + n.duration_units = unit)
          (fun n -> emit cmd_bus (Audio_bridge.Note_off (spec.instrument, n.midi)))
          i_off
      in
      let i_on =
        emit_all_matching spec.notes
          (fun n -> n.start_unit = unit)
          (fun n -> emit cmd_bus (Audio_bridge.Note_on (spec.instrument, n)))
          i_on
      in
      run i_on i_off ()
    in
    let rec supervise () =
      watch restart (fun () -> run 0 0 ());
      supervise ()
    in
    supervise ()

  let controls_process restart tick cmd_bus (controls : Score.control array) () =
    let rec run idx () =
      let unit = await tick in
      let idx =
        emit_all_matching controls
          (fun c -> c.start_unit = unit)
          (fun c -> emit cmd_bus (Audio_bridge.Control_cc (c.channel, c.control, c.value)))
          idx
      in
      run idx ()
    in
    let rec supervise () =
      watch restart (fun () -> run 0 ());
      supervise ()
    in
    supervise ()
end

module Transport_reactive = struct
  type env = {
    score : score_data;
    initial_state : frame;
    restart : unit signal;
    tick : int signal;
    state : (frame, frame) agg_signal;
    cmd_bus : (Audio_bridge.command, Audio_bridge.command list) agg_signal;
    lifecycle : lifecycle_request signal;
    input : host_event list signal;
    source : metronome_source;
  }

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

  let process (env : env) () =
    let state = ref env.initial_state in
    let tempo_cursor = ref 0 in
    let tempo_len = Array.length env.score.tempo_changes in
    let emit_state next =
      state := next;
      emit env.state next
    in
    let set_bpm next =
      let next = clamp 20 300 next in
      let next_unit_ms = unit_ms_of_score_bpm env.score next in
      if next <> !state.bpm || next_unit_ms <> !state.unit_ms then (
        emit_state { !state with bpm = next; unit_ms = next_unit_ms };
        env.source.unit_ms <- next_unit_ms;
        env.source.next_pulse_at <- Unix.gettimeofday () +. (float_of_int next_unit_ms /. 1000.0))
    in
    let apply_tempo_at_unit unit =
      while
        !tempo_cursor < tempo_len
        && env.score.tempo_changes.(!tempo_cursor).start_unit < unit
      do
        incr tempo_cursor
      done;
      while
        !tempo_cursor < tempo_len
        && env.score.tempo_changes.(!tempo_cursor).start_unit = unit
      do
        set_bpm env.score.tempo_changes.(!tempo_cursor).bpm;
        incr tempo_cursor
      done
    in
    let reset_tempo () =
      tempo_cursor := 0;
      set_bpm env.score.initial_bpm;
      apply_tempo_at_unit 0
    in
    let reset_playhead () =
      emit_state { !state with current_unit = -1 };
      env.source.next_pulse_at <-
        Unix.gettimeofday () +. (float_of_int env.source.unit_ms /. 1000.0);
      env.source.pending_pulses <- 0
    in
    let advance_one_unit () =
      let prev = !state.current_unit in
      let next =
        if !state.current_unit + 1 >= env.score.total_units then 0
        else !state.current_unit + 1
      in
      emit_state { !state with current_unit = next };
      let wrapped = prev >= 0 && next = 0 in
      if wrapped then reset_tempo () else apply_tempo_at_unit next;
      emit env.tick next;
      if wrapped then (
        emit env.cmd_bus Audio_bridge.Panic;
        emit env.restart ());
      wrapped
    in
    let handle = function
      | Quit ->
          emit env.lifecycle Request_quit
      | Toggle_play ->
          let playing = not !state.playing in
          emit_state { !state with playing };
          if not playing then emit env.cmd_bus Audio_bridge.Panic
      | Restart ->
          reset_playhead ();
          reset_tempo ();
          emit env.cmd_bus Audio_bridge.Panic;
          emit env.restart ()
      | Select_score idx ->
          emit env.lifecycle (Request_reload_score idx)
      | Select_soundfont idx ->
          emit env.lifecycle (Request_reload_soundfont idx)
      | Pulse ->
          if !state.playing then ignore (advance_one_unit ());
          emit_state !state
    in
    pause ();
    reset_tempo ();
    emit_state !state;
    let rec loop () =
      when_ env.input (fun () ->
          let batch = await_immediate env.input in
          List.iter handle batch;
          pause ();
          loop ())
    in
    loop ()
end

let grid_cell_h_default = 44
let grid_left = 180
let width = 1240
let grid_right_margin = 40
let default_cell_w = 24
let header_title_y = 18
let header_now_playing_y = 62
let header_meta_y = 94
let status_panel_y = 20
let status_panel_h = 92
let header_grid_gap = 22
let transport_bar_gap = 8
let transport_btn_h = 28
let transport_to_timeline_gap = 44
let selector_bottom_margin = 70
let timeline_to_selector_gap = 38
let min_row_h = 8

type layout_rect = {
  x : int;
  y : int;
  w : int;
  h : int;
}

let rect_bottom r = r.y + r.h
let rect_right r = r.x + r.w

let rects_overlap a b =
  not
    (rect_right a <= b.x || rect_right b <= a.x || rect_bottom a <= b.y || rect_bottom b <= a.y)

let status_panel_rect () =
  let panel_w = 420 in
  let panel_x = width - panel_w - 20 in
  { x = panel_x; y = status_panel_y; w = panel_w; h = status_panel_h }

let transport_rect () =
  let x = width - 440 in
  let w = 134 + 10 + 112 in
  { x; y = status_panel_y + status_panel_h + transport_bar_gap; w; h = transport_btn_h }

let grid_top () =
  let header_bottom = header_meta_y + 28 in
  let panel_bottom = rect_bottom (status_panel_rect ()) in
  let transport_bottom = rect_bottom (transport_rect ()) in
  max
    (header_bottom + header_grid_gap)
    (max (panel_bottom + header_grid_gap) (transport_bottom + transport_to_timeline_gap))

let detect_layout_issues (score : score_data) (ui : ui_state) =
  let issues = ref [] in
  let push s = issues := s :: !issues in
  let title_w = measure_text "Tempo Music Player" 44 in
  let now_playing_w =
    min 760 (measure_text (Printf.sprintf "Now playing: %s" score.title) 26)
  in
  let meter_line =
    Printf.sprintf "Meter %d/%d | View 0-0 | Scores %d"
      score.time_signature_num score.time_signature_den (Array.length ui.choices)
  in
  let meter_w = measure_text meter_line 18 in
  let title_block =
    {
      x = 24;
      y = header_title_y;
      w = max title_w (max now_playing_w meter_w);
      h = (header_meta_y - header_title_y) + 26;
    }
  in
  let panel = status_panel_rect () in
  let transport = transport_rect () in
  let timeline_head =
    { x = grid_left; y = (grid_top ()) - 34; w = width - grid_left - grid_right_margin; h = 34 }
  in
  if rects_overlap title_block panel then push "Header text overlaps status panel";
  if rects_overlap transport timeline_head then push "Transport overlaps timeline header";
  List.rev !issues

let mk_color r g b = Color.create r g b 255

let filename_stem name =
  let rec strip s =
    let ext = Filename.extension s in
    if String.length ext = 0 then s else strip (Filename.remove_extension s)
  in
  strip name

let humanize_id s =
  s
  |> String.map (function '_' | '-' -> ' ' | c -> c)
  |> String.trim

let score_label_of_path path =
  path |> Filename.basename |> filename_stem |> humanize_id

let ellipsize text ~font_size ~max_width =
  if measure_text text font_size <= max_width then text
  else
    let suffix = "..." in
    let rec loop i =
      if i <= 0 then suffix
      else
        let candidate = String.sub text 0 i ^ suffix in
        if measure_text candidate font_size <= max_width then candidate else loop (i - 1)
    in
    loop (String.length text - 1)

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

let draw_loading_screen ~title ~detail ~step ~started_at =
  let elapsed = max 0.0 (Unix.gettimeofday () -. started_at) in
  let dots = int_of_float (elapsed *. 4.0) mod 4 in
  let spinner = String.make dots '.' in
  begin_drawing ();
  clear_background (mk_color 15 18 28);
  draw_text_centered title (get_screen_width () / 2) ((get_screen_height () / 2) - 58)
    34 Color.raywhite;
  draw_text_centered detail (get_screen_width () / 2) ((get_screen_height () / 2) - 6) 20
    (mk_color 220 232 244);
  draw_text_centered
    (Printf.sprintf "Step: %s%s" step spinner)
    (get_screen_width () / 2) ((get_screen_height () / 2) + 28) 18
    (mk_color 198 216 234);
  draw_text_centered
    (Printf.sprintf "Elapsed: %.1fs" elapsed)
    (get_screen_width () / 2) ((get_screen_height () / 2) + 58) 16
    (mk_color 170 190 214);
  end_drawing ()

let transport_geometry () =
  let r = transport_rect () in
  let y = r.y in
  let x = r.x in
  let btn_h = r.h in
  let play_w = 134 in
  let restart_w = 112 in
  let gap = 10 in
  let play_x = x in
  let restart_x = x + play_w + gap in
  (play_x, restart_x, y, play_w, restart_w, btn_h)

let point_in_transport_button mx my (x, y, w, h) =
  mx >= float_of_int x
  && mx <= float_of_int (x + w)
  && my >= float_of_int y
  && my <= float_of_int (y + h)

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

let gm_program_name preset =
  let names =
    [|
      "Acoustic Grand Piano"; "Bright Acoustic Piano"; "Electric Grand Piano";
      "Honky-tonk Piano"; "Electric Piano 1"; "Electric Piano 2"; "Harpsichord";
      "Clavinet"; "Celesta"; "Glockenspiel"; "Music Box"; "Vibraphone"; "Marimba";
      "Xylophone"; "Tubular Bells"; "Dulcimer"; "Drawbar Organ"; "Percussive Organ";
      "Rock Organ"; "Church Organ"; "Reed Organ"; "Accordion"; "Harmonica";
      "Tango Accordion"; "Acoustic Guitar (nylon)"; "Acoustic Guitar (steel)";
      "Electric Guitar (jazz)"; "Electric Guitar (clean)"; "Electric Guitar (muted)";
      "Overdriven Guitar"; "Distortion Guitar"; "Guitar Harmonics";
      "Acoustic Bass"; "Electric Bass (finger)"; "Electric Bass (pick)";
      "Fretless Bass"; "Slap Bass 1"; "Slap Bass 2"; "Synth Bass 1"; "Synth Bass 2";
      "Violin"; "Viola"; "Cello"; "Contrabass"; "Tremolo Strings";
      "Pizzicato Strings"; "Orchestral Harp"; "Timpani"; "String Ensemble 1";
      "String Ensemble 2"; "SynthStrings 1"; "SynthStrings 2"; "Choir Aahs";
      "Voice Oohs"; "Synth Voice"; "Orchestra Hit"; "Trumpet"; "Trombone";
      "Tuba"; "Muted Trumpet"; "French Horn"; "Brass Section"; "SynthBrass 1";
      "SynthBrass 2"; "Soprano Sax"; "Alto Sax"; "Tenor Sax"; "Baritone Sax";
      "Oboe"; "English Horn"; "Bassoon"; "Clarinet"; "Piccolo"; "Flute";
      "Recorder"; "Pan Flute"; "Blown Bottle"; "Shakuhachi"; "Whistle";
      "Ocarina"; "Lead 1 (square)"; "Lead 2 (sawtooth)"; "Lead 3 (calliope)";
      "Lead 4 (chiff)"; "Lead 5 (charang)"; "Lead 6 (voice)";
      "Lead 7 (fifths)"; "Lead 8 (bass + lead)"; "Pad 1 (new age)";
      "Pad 2 (warm)"; "Pad 3 (polysynth)"; "Pad 4 (choir)"; "Pad 5 (bowed)";
      "Pad 6 (metallic)"; "Pad 7 (halo)"; "Pad 8 (sweep)"; "FX 1 (rain)";
      "FX 2 (soundtrack)"; "FX 3 (crystal)"; "FX 4 (atmosphere)";
      "FX 5 (brightness)"; "FX 6 (goblins)"; "FX 7 (echoes)"; "FX 8 (sci-fi)";
      "Sitar"; "Banjo"; "Shamisen"; "Koto"; "Kalimba"; "Bag pipe"; "Fiddle";
      "Shanai"; "Tinkle Bell"; "Agogo"; "Steel Drums"; "Woodblock"; "Taiko Drum";
      "Melodic Tom"; "Synth Drum"; "Reverse Cymbal"; "Guitar Fret Noise";
      "Breath Noise"; "Seashore"; "Bird Tweet"; "Telephone Ring"; "Helicopter";
      "Applause"; "Gunshot";
    |]
  in
  let p = clamp 0 127 preset in
  names.(p)

let instrument_label (inst : instrument) =
  if inst.channel = 9 then "Percussion (GM Drum Kit)"
  else
    let base = gm_program_name inst.preset in
    if inst.bank = 0 then base
    else Printf.sprintf "%s [bank %d]" base inst.bank

let height_of_score (score : score_data) =
  (grid_top ()) + (Array.length score.voices * grid_cell_h_default) + 160

let row_height_of_score (score : score_data) =
  let voices = max 1 (Array.length score.voices) in
  let list_h = 250.0 in
  let list_y =
    float_of_int (get_screen_height ()) -. list_h -. float_of_int selector_bottom_margin
  in
  let available = int_of_float list_y - (grid_top ()) - timeline_to_selector_gap in
  clamp min_row_h grid_cell_h_default (available / voices)

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
  |> List.map (fun path -> { label = score_label_of_path path; path })
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

let point_in_rect px py x y w h =
  px >= x && px <= x +. w && py >= y && py <= y +. h

let selector_geometry () =
  let list_x = 20.0 in
  let list_w = float_of_int (width - 40) in
  let list_h = 250.0 in
  let list_y =
    float_of_int (get_screen_height ()) -. list_h -. float_of_int selector_bottom_margin
  in
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
      ui.is_ready <- false;
      push (Select_score idx)
  in
  let select_soundfont_relative delta =
    if soundfont_count > 0 then
      let idx =
        (ui.selected_soundfont + delta + soundfont_count) mod soundfont_count
      in
      ui.selected_soundfont <- idx;
      ui.is_ready <- false;
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
    let play_x, restart_x, bar_y, play_w, restart_w, btn_h = transport_geometry () in
    if ui.is_ready then (
      if point_in_transport_button mx my (play_x, bar_y, play_w, btn_h) then push Toggle_play;
      if point_in_transport_button mx my (restart_x, bar_y, restart_w, btn_h) then push Restart);
    if point_in_rect mx my list_x list_y list_w list_h then (
      let row0_y = list_y +. title_h +. 4.0 in
      if my >= row0_y then (
        let rel = int_of_float ((my -. row0_y) /. item_h) in
        let idx = ui.score_scroll_col + rel in
        if idx >= 0 && idx < Array.length ui.choices then (
          ui.selected_index <- idx;
          ensure_visible idx;
          ui.is_ready <- false;
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
  if ui.is_ready && is_key_pressed Key.Space then (
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

let draw_background (score : score_data) height (viewport : viewport) row_h grid_top
    current_unit =
  let cw = viewport.cell_w in
  let visible_end = min score.total_units (viewport.visible_start + viewport.visible_units) in
  let visible_grid_h = (Array.length score.voices * row_h) + 16 in
  clear_background (mk_color 15 18 28);
  draw_rectangle_gradient_v 0 0 width height (mk_color 25 31 48) (mk_color 11 14 22);
  if score.units_per_bar > 0 then (
    let first_bar = viewport.visible_start / score.units_per_bar in
    let last_bar = visible_end / score.units_per_bar in
    let active_bar =
      if current_unit < 0 then -1 else current_unit / score.units_per_bar
    in
    for bar = first_bar to last_bar do
      let bar_start = bar * score.units_per_bar in
      let bar_end = min score.total_units ((bar + 1) * score.units_per_bar) in
      let draw_start = max bar_start viewport.visible_start in
      let draw_end = min bar_end visible_end in
      if draw_start < draw_end then (
        let x = grid_left + ((draw_start - viewport.visible_start) * cw) in
        let w = max 1 ((draw_end - draw_start) * cw) in
        let bg =
          if bar = active_bar then Color.create 255 244 178 34
          else if bar mod 2 = 0 then Color.create 255 210 120 20
          else Color.create 120 170 255 12
        in
        draw_rectangle x (grid_top - 6) w (visible_grid_h + 8) bg;
        draw_rectangle x (grid_top - 34) w 18 (Color.create 255 244 178 22)
      )
    done);
  let units_per_beat =
    let num = max 1 score.time_signature_num in
    let den = max 1 score.time_signature_den in
    let beat_units = (score.units_per_bar * den) / (num * 4) in
    max 1 beat_units
  in
  for x = viewport.visible_start to visible_end do
    let xx = grid_left + ((x - viewport.visible_start) * cw) in
    let is_bar =
      score.units_per_bar > 0 && x < score.total_units && x mod score.units_per_bar = 0
    in
    let is_beat =
      (not is_bar)
      && score.units_per_bar > 0
      && x < score.total_units
      && x mod units_per_beat = 0
    in
    draw_line xx (grid_top - 8) xx
      (grid_top + (Array.length score.voices * row_h) + 8)
      (if is_bar then Color.create 255 244 178 210
       else if is_beat then Color.create 220 230 255 60
       else Color.create 255 255 255 18);
    if is_bar then
      draw_rectangle (xx - 1) (grid_top - 8) 3
        ((Array.length score.voices * row_h) + 16)
        (Color.create 255 244 178 48);
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
  let mp = get_mouse_position () in
  let mx = Vector2.x mp in
  let my = Vector2.y mp in
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
      let hovered =
        point_in_rect mx my x y (list_w -. 20.0) (item_h -. 2.0)
      in
      let bg =
        if idx = ui.selected_index then Color.create 255 255 255 32
        else if hovered then Color.create 255 255 255 20
        else if row mod 2 = 0 then Color.create 255 255 255 10
        else Color.create 255 255 255 6
      in
      draw_rectangle (int_of_float x) (int_of_float y) (int_of_float (list_w -. 20.0))
        (int_of_float item_h - 2) bg;
      let label =
        ellipsize ui.choices.(idx).label ~font_size:18
          ~max_width:(int_of_float (list_w -. 44.0))
      in
      draw_text label (int_of_float x + 8) (int_of_float y + 6) 18 Color.raywhite)
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
  let draw_transport_bar () =
    let play_x, restart_x, bar_y, play_w, restart_w, btn_h = transport_geometry () in
    let play_bg =
      if not ui.is_ready then Color.create 120 128 140 90
      else if frame.playing then Color.create 112 222 166 170
      else Color.create 126 186 246 160
    in
    let restart_bg =
      if ui.is_ready then Color.create 255 214 120 170
      else Color.create 120 128 140 90
    in
    draw_rectangle play_x bar_y play_w btn_h play_bg;
    draw_rectangle_lines play_x bar_y play_w btn_h (Color.create 210 224 244 180);
    draw_text (if frame.playing then "PAUSE" else "PLAY")
      (play_x + 42) (bar_y + 5) 18 (mk_color 20 24 32);
    draw_rectangle restart_x bar_y restart_w btn_h restart_bg;
    draw_rectangle_lines restart_x bar_y restart_w btn_h (Color.create 210 224 244 180);
    draw_text "RESTART" (restart_x + 18) (bar_y + 5) 18 (mk_color 20 24 32)
  in
  draw_text "Tempo Music Player" 24 header_title_y 44 Color.raywhite;
  let readiness = if ui.is_ready then "READY" else "LOADING" in
  let status = if frame.playing then "PLAYING" else "PAUSED" in
  let score_line = ellipsize (humanize_id score.title) ~font_size:26 ~max_width:760 in
  draw_text
    (Printf.sprintf "Now playing: %s" score_line)
    24 header_now_playing_y 26 (mk_color 220 232 244);
  draw_text
    (Printf.sprintf "Meter %d/%d | View %d-%d | Scores %d"
       score.time_signature_num score.time_signature_den viewport.visible_start
       (min score.total_units (viewport.visible_start + viewport.visible_units))
       (Array.length ui.choices))
    24 header_meta_y 18 (mk_color 200 216 236);
  let panel = status_panel_rect () in
  let panel_w = panel.w in
  let panel_x = panel.x in
  let panel_y = panel.y in
  draw_rectangle_rounded
    (Rectangle.create (float_of_int panel_x) (float_of_int panel_y) (float_of_int panel_w)
       92.0)
    0.18 8 (Color.create 255 255 255 14);
  draw_rectangle_rounded_lines
    (Rectangle.create (float_of_int panel_x) (float_of_int panel_y) (float_of_int panel_w)
       92.0)
    0.18 8 (Color.create 190 210 236 120);
  let badge_color =
    if not ui.is_ready then Color.create 244 170 96 230
    else if frame.playing then Color.create 112 222 166 230
    else Color.create 255 214 120 230
  in
  draw_rectangle (panel_x + 14) (panel_y + 14) 110 24 badge_color;
  draw_text readiness (panel_x + 24) (panel_y + 18) 16 (mk_color 20 24 32);
  draw_text (Printf.sprintf "%s @ %d BPM" status frame.bpm) (panel_x + 138) (panel_y + 18) 18
    (mk_color 220 232 244);
  draw_text
    (ellipsize
       (Printf.sprintf "SF: %s" ui.soundfonts.(ui.selected_soundfont).label)
       ~font_size:16 ~max_width:(panel_w - 28))
    (panel_x + 14) (panel_y + 50) 16 (mk_color 200 216 236);
  draw_text
    (Printf.sprintf "Unit %d ms (%s)" frame.unit_ms score.unit_label)
    (panel_x + 14) (panel_y + 70) 16 (mk_color 200 216 236);
  draw_transport_bar ();
  draw_text
    (if ui.is_ready then
       "SPACE start/pause | R restart | Mouse wheel: score scroll | F5/F6 soundfont | ESC quit"
     else
       "Loading... wait for READY before pressing SPACE | ESC quit")
    24 (height - 40) 18 (mk_color 198 216 234);
  draw_score_selector ui

let draw_voice_row (_score : score_data) (frame : frame) (viewport : viewport) row row_h
    grid_top
    (spec : score_voice) =
  let cw = viewport.cell_w in
  let y = grid_top + (row * row_h) in
  let color = color_of_index row in
  let visible_end = viewport.visible_start + viewport.visible_units in
  let top_pad = max 1 (row_h / 6) in
  let block_h = max 2 (row_h - (2 * top_pad)) in
  let font_size = clamp 8 20 (row_h - 4) in
  let voice_label =
    Printf.sprintf "Ch %d %s" spec.instrument.channel
      (instrument_label spec.instrument)
  in
  draw_text voice_label 26 (y + max 1 (top_pad - 1)) font_size color;
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
        let base =
          Color.create (Color.r color) (Color.g color) (Color.b color) 105
        in
        draw_rectangle x (y + top_pad) w block_h base;
        draw_rectangle_lines x (y + top_pad) w block_h color))
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
        let offset =
          if row_h <= 12 then 0 else min (row_h / 3) (idx * max 1 (row_h / 6))
        in
        draw_rectangle x (y + top_pad + offset) w (max 2 (block_h - offset)) color))
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
          (x + (w / 2)) (y + max 1 (top_pad - 1)) (clamp 8 16 (row_h - 2))
          Color.raywhite

let draw_playhead (score : score_data) (frame : frame) (viewport : viewport) row_h grid_top =
  let cw = viewport.cell_w in
  let step = max 0 frame.current_unit in
  if step >= viewport.visible_start && step <= viewport.visible_start + viewport.visible_units then
    let x = grid_left + ((step - viewport.visible_start) * cw) in
    let focus_radius = 220 in
    let band_top = grid_top - 12 in
    let band_h = (Array.length score.voices * row_h) + 24 in
    let left_w = max 0 (x - focus_radius - grid_left) in
    let right_x = min (width - grid_right_margin) (x + focus_radius) in
    let right_w = max 0 ((width - grid_right_margin) - right_x) in
    if frame.playing then (
      draw_rectangle grid_left band_top left_w band_h (Color.create 6 10 16 40);
      draw_rectangle right_x band_top right_w band_h (Color.create 6 10 16 40));
    draw_rectangle (x - 6) (grid_top - 12) 12
      ((Array.length score.voices * row_h) + 24)
      (Color.create 255 244 178 46);
    draw_rectangle (x - 2) (grid_top - 12) 4
      ((Array.length score.voices * row_h) + 24)
      (Color.create 255 244 178 255)

let draw_frame score height ui (frame : frame) =
  let grid_top = grid_top () in
  let viewport = viewport_of_frame score frame in
  let row_h = row_height_of_score score in
  let layout_issues = detect_layout_issues score ui in
  begin_drawing ();
  draw_background score height viewport row_h grid_top frame.current_unit;
  draw_header score height frame viewport ui;
  Array.iteri
    (fun row spec -> draw_voice_row score frame viewport row row_h grid_top spec)
    score.voices;
  draw_playhead score frame viewport row_h grid_top;
  (match layout_issues with
  | [] -> ()
  | first :: _ ->
      draw_rectangle 20 4 520 20 (Color.create 180 38 38 210);
      draw_text (Printf.sprintf "LAYOUT WARNING: %s" first) 28 7 14 Color.raywhite);
  end_drawing ()

let render_output score height ui synth bridge (frame : frame) =
  let audio_commands = Audio_bridge.drain bridge in
  Audio_bridge.apply synth bridge audio_commands;
  draw_frame score height ui frame

let main_program score bridge source input output =
  let initial_state =
    {
      playing = false;
      bpm = score.initial_bpm;
      unit_ms = unit_ms_of_score_bpm score score.initial_bpm;
      current_unit = -1;
    }
  in
  let restart = new_signal () in
  let tick = new_signal () in
  let state =
    new_signal_agg ~initial:initial_state ~combine:(fun _latest next -> next)
  in
  let cmd_bus =
    new_signal_agg ~initial:[] ~combine:(fun acc ev -> ev :: acc)
  in
  let lifecycle = new_signal () in
  let render_process () =
    let rec loop () =
      let next_frame = await state in
      pause ();
      emit output next_frame;
      loop ()
    in
    loop ()
  in
  let lifecycle_process () =
    let loop () =
      match await lifecycle with
      | Request_quit ->
          emit cmd_bus Audio_bridge.Panic;
          pause ();
          raise Quit_request
      | Request_reload_score idx ->
          emit cmd_bus Audio_bridge.Panic;
          pause ();
          raise (Reload_score_request idx)
      | Request_reload_soundfont idx ->
          emit cmd_bus Audio_bridge.Panic;
          pause ();
          raise (Reload_soundfont_request idx)
    in
    loop ()
  in
  let voice_threads =
    Array.to_list
      (Array.mapi
         (fun _voice_index spec ->
           fun () -> Score_reactive.voice_process restart tick cmd_bus spec ())
         score.voices)
  in
  let control_threads =
    if Array.length score.controls = 0 then []
    else [ (fun () -> Score_reactive.controls_process restart tick cmd_bus score.controls ()) ]
  in
  parallel
    ([ (fun () ->
         Transport_reactive.process
           {
             score;
             initial_state;
             restart;
             tick;
             state;
             cmd_bus;
             lifecycle;
             input;
             source;
           }
           ());
       (fun () -> lifecycle_process ());
       (fun () -> Audio_bridge.collect_process cmd_bus bridge ());
       (fun () -> render_process ());
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
      is_ready = false;
    }
  in
  let initial_score = score_of_choice ui.choices.(ui.selected_index) in
  let height = max 680 (height_of_score initial_score) in
  init_window width height "Tempo Music Player";
  set_target_fps 60;
  let rec run_selected () =
    ui.is_ready <- false;
    let started_at = Unix.gettimeofday () in
    let loading_detail =
      Printf.sprintf "Preparing: %s" ui.choices.(ui.selected_index).label
    in
    draw_loading_screen ~title:"Tempo Music Player" ~detail:loading_detail
      ~step:"load score" ~started_at;
    let score = load_score ui.choices.(ui.selected_index) in
    draw_loading_screen ~title:"Tempo Music Player" ~detail:loading_detail
      ~step:"create synth" ~started_at;
    let selected_sf_idx = ui.selected_soundfont in
    ui.selected_soundfont <- selected_sf_idx;
    let soundfont_path = ui.soundfonts.(selected_sf_idx).sf_path in
    let synth = Synth.create ~soundfont:soundfont_path ~gain:0.7 () in
    draw_loading_screen ~title:"Tempo Music Player" ~detail:loading_detail
      ~step:"configure instruments" ~started_at;
    let bridge = Audio_bridge.create () in
    Array.iter (Audio_bridge.configure_instrument synth) score.voices;
    draw_loading_screen ~title:"Tempo Music Player" ~detail:loading_detail
      ~step:"finalize scene" ~started_at;
    let source = create_source (unit_ms_of_score_bpm score score.initial_bpm) in
    let input = make_interactive_source source ui in
    ui.is_ready <- true;
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
