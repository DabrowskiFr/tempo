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

type voice = {
  name : string;
  instrument : instrument;
  notes : note array;
}

type t = {
  title : string;
  voices : voice array;
  total_units : int;
  unit_label : string;
  time_signature_num : int;
  time_signature_den : int;
  units_per_bar : int;
  initial_bpm : int;
}

let min_bpm = 50
let max_bpm = 180
let default_bpm = 108

let default =
  let voices =
    [|
      {
        name = "Lead";
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
    time_signature_num = 4;
    time_signature_den = 4;
    units_per_bar = 16;
    initial_bpm = default_bpm;
  }

type started_note = {
  start_tick : int;
  velocity : int;
  bank : int;
  program : int;
}

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

let units_per_bar ~division ~unit_ticks ~numerator ~denominator =
  let bar_ticks = (numerator * division * 4) / max 1 denominator in
  max 1 (bar_ticks / max 1 unit_ticks)

let choose_unit_ticks values division max_tick =
  let raw_gcd =
    match gcd_list values with
    | 0 -> max 1 (division / 4)
    | n -> n
  in
  let candidate_denominators = [ 64; 48; 32; 24; 16; 12; 8; 6; 4; 3; 2; 1 ] in
  let exact_candidates =
    candidate_denominators
    |> List.filter_map (fun denom ->
           if division mod denom <> 0 then None
           else
             let ticks = division / denom in
             if ticks < raw_gcd then None
             else if List.for_all (fun value -> value mod ticks = 0) values then Some ticks
             else None)
  in
  match exact_candidates with
  | ticks :: _ -> ticks
  | [] ->
      let rec coarsen ticks =
        if ticks <= 0 then max 1 raw_gcd
        else
          let total_units = max 1 (max_tick / ticks) in
          if total_units <= 1024 then ticks else coarsen (ticks * 2)
      in
      coarsen raw_gcd

let of_midi_file path =
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
            push_active channel key
              { start_tick = tick; velocity; bank; program = programs.(channel) }
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
  let max_tick =
    List.fold_left
      (fun acc (_, bucket) ->
        List.fold_left
          (fun acc (start_tick, duration_tick, _, _) ->
            max acc (start_tick + duration_tick))
          acc !bucket)
      0 raw_groups
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
    choose_unit_ticks values midi.division max_tick
  in
  let voices =
    Array.of_list
      (List.map
         (fun ((channel, bank, program), bucket) ->
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
             instrument = { channel; bank; preset = program };
             notes;
           })
         raw_groups)
  in
  let total_units =
    Array.fold_left
      (fun acc (voice : voice) ->
        Array.fold_left
          (fun acc (note : note) -> max acc (note.start_unit + note.duration_units))
          acc voice.notes)
      0 voices
  in
  let initial_bpm =
    let bpm =
      int_of_float (60000000.0 /. float_of_int (max 1 midi.tempo_us_per_quarter))
    in
    max min_bpm (min max_bpm bpm)
  in
  let time_signature_num, time_signature_den =
    match midi.time_signature with
    | Some (num, den) -> (max 1 num, max 1 den)
    | None -> (4, 4)
  in
  {
    title = Filename.basename path;
    voices;
    total_units = max 1 total_units;
    unit_label = describe_unit unit_ticks midi.division;
    time_signature_num;
    time_signature_den;
    units_per_bar =
      units_per_bar ~division:midi.division ~unit_ticks ~numerator:time_signature_num
        ~denominator:time_signature_den;
    initial_bpm;
  }

let note_count (score : t) =
  Array.fold_left (fun acc (voice : voice) -> acc + Array.length voice.notes) 0
    score.voices
