open Tempo
open Raylib

module Synth = Tempo_fluidsynth

type note = {
  start_unit : int;
  duration_units : int;
  midi : int;
  volume : float;
}

type instrument = {
  channel : int;
  bank : int;
  preset : int;
}

type score_voice = {
  name : string;
  color : Color.t;
  instrument : instrument;
  notes : note array;
}

type score_data = {
  title : string;
  voices : score_voice array;
  total_units : int;
  unit_label : string;
  initial_bpm : int;
}

type playing_note = {
  note_id : int;
  midi : int;
  start_unit : int;
  duration_units : int;
}

type voice_runtime = {
  mutable active : playing_note option;
  mutable next_note_id : int;
}

type host_event =
  | Pulse
  | Toggle_play
  | Restart
  | Tempo_up
  | Tempo_down
  | Quit

type frame = {
  playing : bool;
  bpm : int;
  unit_ms : int;
  current_unit : int;
  voices : voice_runtime array;
}

type transport = {
  mutable playing : bool;
  mutable bpm : int;
  mutable current_unit : int;
}

type metronome_source = {
  pending_pulses : int Atomic.t;
  stop_requested : bool Atomic.t;
  mutable worker : unit Domain.t option;
  unit_ms : int Atomic.t;
}

type started_note = {
  start_tick : int;
  velocity : int;
  bank : int;
  program : int;
}

let min_bpm = 50
let max_bpm = 180
let default_bpm = 108
let grid_cell_h = 44
let grid_left = 180
let grid_top = 120
let width = 1240

let mk_color r g b = Color.create r g b 255

let default_score =
  let voices =
    [|
      {
        name = "Lead";
        color = mk_color 240 124 92;
        instrument = { channel = 0; bank = 0; preset = 0 };
        notes =
          [|
            { start_unit = 0; duration_units = 2; midi = 72; volume = 0.22 };
            { start_unit = 2; duration_units = 2; midi = 74; volume = 0.22 };
            { start_unit = 4; duration_units = 4; midi = 76; volume = 0.24 };
            { start_unit = 8; duration_units = 2; midi = 79; volume = 0.24 };
            { start_unit = 10; duration_units = 2; midi = 76; volume = 0.22 };
            { start_unit = 12; duration_units = 4; midi = 74; volume = 0.22 };
            { start_unit = 16; duration_units = 2; midi = 72; volume = 0.22 };
            { start_unit = 18; duration_units = 2; midi = 74; volume = 0.22 };
            { start_unit = 20; duration_units = 4; midi = 79; volume = 0.24 };
            { start_unit = 24; duration_units = 2; midi = 81; volume = 0.24 };
            { start_unit = 26; duration_units = 2; midi = 79; volume = 0.23 };
            { start_unit = 28; duration_units = 4; midi = 76; volume = 0.22 };
          |];
      };
      {
        name = "Counter";
        color = mk_color 94 191 161;
        instrument = { channel = 1; bank = 0; preset = 48 };
        notes =
          [|
            { start_unit = 0; duration_units = 4; midi = 64; volume = 0.18 };
            { start_unit = 4; duration_units = 4; midi = 67; volume = 0.18 };
            { start_unit = 8; duration_units = 4; midi = 69; volume = 0.18 };
            { start_unit = 12; duration_units = 4; midi = 67; volume = 0.18 };
            { start_unit = 16; duration_units = 4; midi = 65; volume = 0.18 };
            { start_unit = 20; duration_units = 4; midi = 69; volume = 0.18 };
            { start_unit = 24; duration_units = 4; midi = 67; volume = 0.18 };
            { start_unit = 28; duration_units = 4; midi = 64; volume = 0.18 };
          |];
      };
      {
        name = "Bass";
        color = mk_color 102 152 236;
        instrument = { channel = 2; bank = 0; preset = 32 };
        notes =
          [|
            { start_unit = 0; duration_units = 8; midi = 48; volume = 0.3 };
            { start_unit = 8; duration_units = 8; midi = 43; volume = 0.3 };
            { start_unit = 16; duration_units = 8; midi = 45; volume = 0.3 };
            { start_unit = 24; duration_units = 8; midi = 41; volume = 0.3 };
          |];
      };
    |]
  in
  {
    title = "Built-in score";
    voices;
    total_units = 32;
    unit_label = "sixteenth note";
    initial_bpm = default_bpm;
  }

let unit_ms_of_bpm bpm =
  max 25 (int_of_float (15000.0 /. float_of_int bpm))

let draw_text_centered text x y size color =
  let w = measure_text text size in
  draw_text text (x - (w / 2)) y size color

let velocity_of_volume volume =
  let scaled = int_of_float (volume *. 127.0) in
  max 1 (min 127 scaled)

let gcd a b =
  let rec loop x y =
    if y = 0 then abs x else loop y (x mod y)
  in
  loop a b

let gcd_list values =
  let rec loop acc = function
    | [] -> acc
    | x :: xs ->
        let x = abs x in
        if x = 0 then loop acc xs
        else if acc = 0 then loop x xs
        else loop (gcd acc x) xs
  in
  loop 0 values

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

let cell_w (score : score_data) =
  max 8 ((width - grid_left - 80) / max 1 score.total_units)

let default_midi_candidates = [ "assets/demo.mid"; "applications/simple-demos/score-player-raylib/assets/demo.mid" ]

let find_default_midi () =
  List.find_opt Sys.file_exists default_midi_candidates

let describe_unit unit_ticks division =
  if unit_ticks <= 0 || division <= 0 then "logical unit"
  else if division mod unit_ticks <> 0 then Printf.sprintf "%d MIDI ticks" unit_ticks
  else
    match division / unit_ticks with
    | 1 -> "quarter note"
    | 2 -> "eighth note"
    | 4 -> "sixteenth note"
    | 8 -> "thirty-second note"
    | n -> Printf.sprintf "1/%d of a quarter note" n

let score_of_midi_file path =
  let midi = Synth.import_midi_file path in
  let bank_msb = Array.make 16 0 in
  let bank_lsb = Array.make 16 0 in
  let programs = Array.make 16 0 in
  let active = Hashtbl.create 64 in
  let groups = Hashtbl.create 32 in
  let register_note channel bank program start_tick duration_tick key velocity =
    let group_key = (channel, bank, program) in
    let bucket =
      match Hashtbl.find_opt groups group_key with
      | Some bucket -> bucket
      | None ->
          let bucket = ref [] in
          Hashtbl.add groups group_key bucket;
          bucket
    in
    bucket := (start_tick, duration_tick, key, velocity) :: !bucket
  in
  let push_active channel key started =
    let slot = (channel, key) in
    let current = Option.value ~default:[] (Hashtbl.find_opt active slot) in
    Hashtbl.replace active slot (started :: current)
  in
  let pop_active channel key =
    let slot = (channel, key) in
    match Hashtbl.find_opt active slot with
    | Some (started :: rest) ->
        if rest = [] then Hashtbl.remove active slot
        else Hashtbl.replace active slot rest;
        Some started
    | _ -> None
  in
  List.iter
    (fun (tick, event) ->
      match event with
      | Synth.Control_change (channel, control, value) ->
          if control = 0 then bank_msb.(channel) <- value
          else if control = 32 then bank_lsb.(channel) <- value
      | Synth.Program_change (channel, program) ->
          programs.(channel) <- program
      | Synth.Note_on (channel, key, velocity) ->
          if velocity = 0 then (
            match pop_active channel key with
            | Some started ->
                register_note channel started.bank started.program started.start_tick
                  (max 1 (tick - started.start_tick)) key started.velocity
            | None -> ())
          else
            let bank = (bank_msb.(channel) * 128) + bank_lsb.(channel) in
            push_active channel key { start_tick = tick; velocity; bank; program = programs.(channel) }
      | Synth.Note_off (channel, key) -> (
          match pop_active channel key with
          | Some started ->
              register_note channel started.bank started.program started.start_tick
                (max 1 (tick - started.start_tick)) key started.velocity
          | None -> ()))
    midi.events;
  let raw_groups =
    Hashtbl.to_seq groups |> List.of_seq
    |> List.sort (fun ((ca, ba, pa), _) ((cb, bb, pb), _) ->
           match compare ca cb with
           | 0 -> (
               match compare ba bb with
               | 0 -> compare pa pb
               | n -> n)
           | n -> n)
  in
  let unit_ticks =
    let values =
      List.concat_map
        (fun (_, bucket) ->
          List.concat_map
            (fun (start_tick, duration_tick, _, _) -> [ start_tick; duration_tick ])
            !bucket)
        raw_groups
    in
    match gcd_list values with
    | 0 -> max 1 (midi.division / 4)
    | n -> n
  in
  let voices =
    Array.of_list
      (List.mapi
         (fun idx ((channel, bank, program), bucket) ->
           let notes =
             !bucket
             |> List.sort (fun (ta, _, ka, _) (tb, _, kb, _) ->
                    match compare ta tb with
                    | 0 -> compare ka kb
                    | n -> n)
             |> List.map (fun (start_tick, duration_tick, key, velocity) ->
                    {
                      start_unit = start_tick / unit_ticks;
                      duration_units = max 1 (duration_tick / unit_ticks);
                      midi = key;
                      volume = float_of_int velocity /. 127.0;
                    })
             |> Array.of_list
           in
           {
             name = Printf.sprintf "Ch %d P %d" (channel + 1) program;
             color = color_of_index idx;
             instrument = { channel; bank; preset = program };
             notes;
           })
         raw_groups)
  in
  let total_units =
    Array.fold_left
      (fun acc (voice : score_voice) ->
        Array.fold_left
          (fun acc (note : note) -> max acc (note.start_unit + note.duration_units))
          acc voice.notes)
      0 voices
  in
  let initial_bpm =
    let bpm = int_of_float (60000000.0 /. float_of_int (max 1 midi.tempo_us_per_quarter)) in
    max min_bpm (min max_bpm bpm)
  in
  {
    title = Filename.basename path;
    voices;
    total_units = max 1 total_units;
    unit_label = describe_unit unit_ticks midi.division;
    initial_bpm;
  }

let load_score () =
  match Sys.argv |> Array.to_list |> List.tl with
  | path :: _ -> score_of_midi_file path
  | [] -> (
      match find_default_midi () with
      | Some path -> score_of_midi_file path
      | None -> default_score)

let create_source unit_ms =
  {
    pending_pulses = Atomic.make 0;
    stop_requested = Atomic.make false;
    worker = None;
    unit_ms = Atomic.make unit_ms;
  }

let stop_source source =
  Atomic.set source.stop_requested true;
  Option.iter Domain.join source.worker;
  source.worker <- None

let spawn_metronome source =
  let worker () =
    while not (Atomic.get source.stop_requested) do
      let sleep_s = float_of_int (Atomic.get source.unit_ms) /. 1000.0 in
      Unix.sleepf sleep_s;
      if not (Atomic.get source.stop_requested) then (
        ignore (Atomic.fetch_and_add source.pending_pulses 1))
    done
  in
  source.worker <- Some (Domain.spawn worker)

let poll_events source =
  let events = ref [] in
  let push ev = events := ev :: !events in
  let pulses = Atomic.exchange source.pending_pulses 0 in
  if window_should_close () || is_key_pressed Key.Escape then push Quit;
  if is_key_pressed Key.Space then push Toggle_play;
  if is_key_pressed Key.R then push Restart;
  if is_key_pressed Key.Equal then push Tempo_up;
  if is_key_pressed Key.Minus then push Tempo_down;
  let events = List.rev !events in
  let pulse_events = List.init pulses (fun _ -> Pulse) in
  match events @ pulse_events with
  | [] -> None
  | batch -> Some batch

let make_input source =
  if Option.is_none source.worker then spawn_metronome source;
  fun () ->
    Unix.sleepf (1.0 /. 60.0);
    poll_events source

let copy_voice_state voices =
  Array.map
    (fun voice -> { active = voice.active; next_note_id = voice.next_note_id })
    voices

let make_frame transport voices =
  {
    playing = transport.playing;
    bpm = transport.bpm;
    unit_ms = unit_ms_of_bpm transport.bpm;
    current_unit = transport.current_unit;
    voices = copy_voice_state voices;
  }

let stop_voice (score : score_data) synth voice_index runtime =
  match runtime.active with
  | None -> ()
  | Some playing ->
      let instrument = score.voices.(voice_index).instrument in
      Synth.note_off synth ~channel:instrument.channel ~key:playing.midi

let clear_voice (score : score_data) synth voices =
  Array.iteri
    (fun voice_index runtime ->
      stop_voice score synth voice_index runtime;
      runtime.active <- None)
    voices

let reset_transport (score : score_data) synth transport voices source =
  transport.current_unit <- -1;
  clear_voice score synth voices;
  Atomic.set source.pending_pulses 0

let wait_pulses pulse count =
  let rec loop remaining =
    if remaining <= 0 then ()
    else (
      ignore (await pulse);
      loop (remaining - 1))
  in
  loop count

let start_note (score : score_data) synth runtime voice_index (note : note) =
  let spec = score.voices.(voice_index) in
  let note_id = runtime.next_note_id in
  runtime.next_note_id <- runtime.next_note_id + 1;
  runtime.active <- Some { note_id; midi = note.midi; start_unit = note.start_unit; duration_units = note.duration_units };
  Synth.note_on synth ~channel:spec.instrument.channel ~key:note.midi
    ~velocity:(velocity_of_volume note.volume);
  note_id

let finish_note (score : score_data) synth runtime voice_index (note : note) note_id =
  let spec = score.voices.(voice_index) in
  Synth.note_off synth ~channel:spec.instrument.channel ~key:note.midi;
  match runtime.active with
  | Some playing when playing.note_id = note_id -> runtime.active <- None
  | _ -> ()

let note_process pulse score synth runtime voice_index (note : note) () =
  wait_pulses pulse (note.start_unit + 1);
  let note_id = start_note score synth runtime voice_index note in
  wait_pulses pulse note.duration_units;
  finish_note score synth runtime voice_index note note_id

let score_cycle pulse score synth voices () =
  let note_processes =
    Array.to_list
      (Array.mapi
         (fun voice_index spec ->
           Array.to_list
             (Array.map
                (fun note -> note_process pulse score synth voices.(voice_index) voice_index note)
                spec.notes))
         score.voices)
    |> List.concat
  in
  parallel note_processes

let rec score_process restart pulse score synth voices () =
  watch restart (fun () -> score_cycle pulse score synth voices ());
  score_process restart pulse score synth voices ()

let control_process score pulse restart synth transport voices source input output stop () =
  let set_bpm bpm =
    let bpm = max min_bpm (min max_bpm bpm) in
    transport.bpm <- bpm;
    Atomic.set source.unit_ms (unit_ms_of_bpm bpm)
  in
  let advance_one_unit () =
    let next_unit =
      if transport.current_unit + 1 >= score.total_units then 0
      else transport.current_unit + 1
    in
    transport.current_unit <- next_unit;
    if next_unit = 0 then clear_voice score synth voices;
    next_unit
  in
  let handle = function
    | Quit ->
        clear_voice score synth voices;
        emit stop ();
        None
    | Toggle_play ->
        transport.playing <- not transport.playing;
        if not transport.playing then clear_voice score synth voices;
        None
    | Restart ->
        reset_transport score synth transport voices source;
        emit restart ();
        None
    | Tempo_up ->
        set_bpm (transport.bpm + 6);
        None
    | Tempo_down ->
        set_bpm (transport.bpm - 6);
        None
    | Pulse ->
        if transport.playing then (
          let next_unit = advance_one_unit () in
          emit pulse ();
          Some next_unit)
        else None
  in
  let rec loop () =
    when_ input (fun () ->
        let batch = await_immediate input in
        ignore (List.filter_map handle batch);
        emit output (make_frame transport voices));
    pause ();
    loop ()
  in
  loop ()

let draw_background (score : score_data) height =
  let cw = cell_w score in
  clear_background (mk_color 15 18 28);
  draw_rectangle_gradient_v 0 0 width height (mk_color 25 31 48) (mk_color 11 14 22);
  for x = 0 to score.total_units do
    let xx = grid_left + (x * cw) in
    draw_line xx (grid_top - 8) xx
      (grid_top + (Array.length score.voices * grid_cell_h) + 8)
      (Color.create 255 255 255 16)
  done

let draw_header (score : score_data) height (frame : frame) =
  draw_text "Tempo Core Score Player" 24 20 28 Color.raywhite;
  draw_text
    "execute + watch + parallel + when_ + await_immediate + MIDI import"
    24 52 18 (mk_color 189 210 235);
  let status = if frame.playing then "PLAYING" else "PAUSED" in
  draw_text
    (Printf.sprintf
       "Score: %s | Status: %s | BPM: %d | Unit: %d ms (%s) | FluidSynth SoundFont"
       score.title status frame.bpm frame.unit_ms score.unit_label)
    24 82 18 (mk_color 220 232 244);
  draw_text "SPACE play/pause | R restart | +/- tempo | ESC quit"
    24 (height - 40) 18 (mk_color 198 216 234)

let draw_voice_row (score : score_data) (frame : frame) row (spec : score_voice) =
  let cw = cell_w score in
  let y = grid_top + (row * grid_cell_h) in
  draw_text spec.name 26 (y + 10) 20 spec.color;
  draw_rectangle (grid_left - 8) y ((score.total_units * cw) + 16) grid_cell_h (Color.create 255 255 255 10);
  Array.iter
    (fun (note : note) ->
      let x = grid_left + (note.start_unit * cw) in
      let w = max 10 ((note.duration_units * cw) - 4) in
      let base = Color.create (Color.r spec.color) (Color.g spec.color) (Color.b spec.color) 105 in
      draw_rectangle x (y + 8) w (grid_cell_h - 16) base;
      draw_rectangle_lines x (y + 8) w (grid_cell_h - 16) spec.color)
    spec.notes;
  match frame.voices.(row).active with
  | None -> ()
  | Some playing ->
      let x = grid_left + (playing.start_unit * cw) in
      let w = max 10 ((playing.duration_units * cw) - 4) in
      draw_rectangle x (y + 8) w (grid_cell_h - 16) spec.color;
      draw_text_centered (Printf.sprintf "midi %d" playing.midi)
        (x + (w / 2)) (y + 12) 16 Color.raywhite

let draw_playhead (score : score_data) (frame : frame) =
  let cw = cell_w score in
  let step = max 0 frame.current_unit in
  let x = grid_left + (step * cw) in
  draw_rectangle (x - 2) (grid_top - 12) 4
    ((Array.length score.voices * grid_cell_h) + 24)
    (Color.create 255 244 178 255)

let draw_legend (score : score_data) _height =
  let y = grid_top + (Array.length score.voices * grid_cell_h) + 24 in
  draw_rectangle_rounded (Rectangle.create 20.0 (float_of_int y) (float_of_int (width - 40)) 80.0) 0.15 8 (Color.create 255 255 255 18);
  draw_rectangle_rounded_lines (Rectangle.create 20.0 (float_of_int y) (float_of_int (width - 40)) 80.0) 0.15 8 (mk_color 189 210 235);
  draw_text "Interpretation" 34 (y + 12) 22 Color.raywhite;
  draw_text
    "Tempo imports MIDI events, derives its own logical unit, and schedules note_on / note_off as synchronous processes."
    34 (y + 40) 18 (mk_color 224 235 245);
  draw_text
    "FluidSynth provides MIDI parsing and pleasant SoundFont rendering, but Tempo still owns pause, restart, tempo changes and the logical timeline."
    34 (y + 64) 18 (mk_color 224 235 245)

let draw_frame score height (frame : frame) =
  begin_drawing ();
  draw_background score height;
  draw_header score height frame;
  Array.iteri (draw_voice_row score frame) score.voices;
  draw_playhead score frame;
  draw_legend score height;
  end_drawing ()

let main_program score synth source input output =
  let stop = new_signal () in
  let restart = new_signal () in
  let pulse = new_signal () in
  let transport = { playing = true; bpm = score.initial_bpm; current_unit = -1 } in
  let voices =
    Array.init (Array.length score.voices) (fun _ -> { active = None; next_note_id = 0 })
  in
  watch stop (fun () ->
      parallel
        [
          (fun () -> control_process score pulse restart synth transport voices source input output stop ());
          (fun () -> score_process restart pulse score synth voices ());
        ])

let () =
  let score = load_score () in
  let height = height_of_score score in
  init_window width height "Tempo Core Score Player";
  set_target_fps 60;
  let synth = Synth.create ~gain:0.7 () in
  Array.iter
    (fun spec ->
      Synth.program_select synth ~channel:spec.instrument.channel
        ~bank:spec.instrument.bank ~preset:spec.instrument.preset)
    score.voices;
  let source = create_source (unit_ms_of_bpm score.initial_bpm) in
  let input = make_input source in
  (try
     execute ~input ~output:(draw_frame score height)
       (main_program score synth source)
   with Exit -> ());
  stop_source source;
  Array.iter
    (fun spec -> Synth.all_notes_off synth ~channel:spec.instrument.channel)
    score.voices;
  Synth.shutdown synth;
  close_window ()
