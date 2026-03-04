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

type pedal = {
  start_unit : int;
  channel : int;
  value : int;
}

type control = {
  start_unit : int;
  channel : int;
  control : int;
  value : int;
}

type tempo_change = {
  start_unit : int;
  bpm : int;
}

type t = {
  title : string;
  voices : voice array;
  pedals : pedal array;
  controls : control array;
  tempo_changes : tempo_change array;
  total_units : int;
  unit_label : string;
  time_signature_num : int;
  time_signature_den : int;
  units_per_bar : int;
  initial_bpm : int;
}

exception Parse_error of string
exception Unsupported_binary_format of string

let min_bpm = 50
let max_bpm = 180
let default_bpm = 108
let clamp lo hi v = max lo (min hi v)
let canonical_unit_label = "1/16"

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
    pedals = [||];
    controls = [||];
    tempo_changes = [||];
    total_units = 32;
    unit_label = canonical_unit_label;
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

let units_per_bar ~division ~unit_ticks ~numerator ~denominator =
  let bar_ticks = (numerator * division * 4) / max 1 denominator in
  let u = max 1 unit_ticks in
  max 1 (int_of_float (Float.round (float_of_int bar_ticks /. float_of_int u)))

let subdivision_den_of_units_per_bar ~num ~den ~units_per_bar =
  let num = max 1 num in
  let den = max 1 den in
  let upb = max 1 units_per_bar in
  let scaled = upb * den in
  if scaled mod num = 0 then Some (scaled / num) else None

let unit_label_of_subdivision_den den =
  Printf.sprintf "1/%d" (max 1 den)

let units_of_ticks ticks unit_ticks =
  int_of_float
    (Float.round (float_of_int ticks /. float_of_int (max 1 unit_ticks)))

let units_floor ticks unit_ticks =
  max 0 (ticks / max 1 unit_ticks)

let units_ceil ticks unit_ticks =
  let u = max 1 unit_ticks in
  max 0 ((ticks + u - 1) / u)

let choose_unit_ticks values division max_tick numerator denominator =
  let bar_ticks = (max 1 numerator * max 1 division * 4) / max 1 denominator in
  let max_units = 4096 in
  let quant_error ticks =
    List.fold_left
      (fun acc v ->
        let q = units_of_ticks v ticks in
        let reconstructed = q * ticks in
        acc + abs (v - reconstructed))
      0 values
  in
  let score ticks =
    let err = quant_error ticks in
    let density_penalty = max 0 ((max_tick / max 1 ticks) - 1024) in
    (err * 1000) + density_penalty
  in
  let add_candidate acc ticks =
    if ticks <= 0 then acc
    else if max_tick / ticks > max_units then acc
    else
      let upb =
        max 1
          (int_of_float
             (Float.round (float_of_int bar_ticks /. float_of_int ticks)))
      in
      if upb > 32 then acc
    else if List.mem ticks acc then acc
    else ticks :: acc
  in
  let preferred_units_per_bar = [ 32; 24; 16; 12; 8; 6; 4; 3; 2; 1 ] in
  let bar_aligned_candidates =
    preferred_units_per_bar
    |> List.filter_map (fun upb ->
           if upb <= 0 || bar_ticks mod upb <> 0 then None
           else Some (bar_ticks / upb))
  in
  let candidate_denominators = [ 64; 48; 32; 24; 16; 12; 8; 6; 4; 3; 2; 1 ] in
  let division_candidates =
    candidate_denominators
    |> List.filter_map (fun denom ->
           if division mod denom <> 0 then None
           else Some (division / denom))
  in
  let candidates =
    List.fold_left add_candidate [] (bar_aligned_candidates @ division_candidates)
  in
  match candidates with
  | ticks :: rest ->
      List.fold_left
        (fun best t -> if score t < score best then t else best)
        ticks rest
  | [] ->
      let raw_gcd =
        match gcd_list values with
        | 0 -> max 1 (division / 4)
        | n -> n
      in
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
  let controls_ticks_rev = ref [] in
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
          else if control = 64 || control = 66 || control = 67 then
            controls_ticks_rev :=
              (tick, channel, control, clamp 0 127 value) :: !controls_ticks_rev
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
  let time_signature_num, time_signature_den =
    match midi.time_signature with
    | Some (num, den) -> (max 1 num, max 1 den)
    | None -> (4, 4)
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
    choose_unit_ticks values midi.division max_tick time_signature_num
      time_signature_den
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
                    let start_unit = units_floor start_tick unit_ticks in
                    let end_unit = units_ceil (start_tick + duration_tick) unit_ticks in
                    {
                      start_unit;
                      duration_units = max 1 (end_unit - start_unit);
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
  let controls =
    !controls_ticks_rev
    |> List.sort (fun (ta, ca, cta, va) (tb, cb, ctb, vb) ->
           match compare ta tb with
           | 0 -> (
               match compare ca cb with
               | 0 -> (
                   match compare cta ctb with
                   | 0 -> compare va vb
                   | n -> n)
               | n -> n)
           | n -> n)
    |> List.map (fun (tick, channel, control, value) ->
           {
             start_unit = units_of_ticks tick unit_ticks;
             channel;
             control;
             value;
           })
    |> Array.of_list
  in
  let pedals =
    controls
    |> Array.to_list
    |> List.filter_map (fun (c : control) ->
           if c.control = 64 then
             Some { start_unit = c.start_unit; channel = c.channel; value = c.value }
           else None)
    |> Array.of_list
  in
  let tempo_changes =
    midi.tempo_changes_us_per_quarter
    |> List.sort (fun (ta, _) (tb, _) -> compare ta tb)
    |> List.filter_map (fun (tick, tempo_us_per_quarter) ->
           if tempo_us_per_quarter <= 0 then None
           else
             let bpm =
               int_of_float
                 (60000000.0 /. float_of_int (max 1 tempo_us_per_quarter))
             in
             Some
               {
                 start_unit = max 0 (units_of_ticks tick unit_ticks);
                 bpm = clamp min_bpm max_bpm bpm;
               })
    |> List.sort (fun a b -> compare a.start_unit b.start_unit)
    |> Array.of_list
  in
  let total_units =
    let note_max =
      Array.fold_left
        (fun acc (voice : voice) ->
          Array.fold_left
            (fun acc (note : note) -> max acc (note.start_unit + note.duration_units))
            acc voice.notes)
        0 voices
    in
    let with_controls =
      Array.fold_left
        (fun acc (c : control) -> max acc (c.start_unit + 1))
        note_max controls
    in
    Array.fold_left
      (fun acc (t : tempo_change) -> max acc (t.start_unit + 1))
      with_controls tempo_changes
  in
  let initial_bpm =
    let bpm =
      int_of_float (60000000.0 /. float_of_int (max 1 midi.tempo_us_per_quarter))
    in
    max min_bpm (min max_bpm bpm)
  in
  let units_per_bar =
    units_per_bar ~division:midi.division ~unit_ticks ~numerator:time_signature_num
      ~denominator:time_signature_den
  in
  let unit_label =
    match
      subdivision_den_of_units_per_bar ~num:time_signature_num ~den:time_signature_den
        ~units_per_bar
    with
    | Some subdivision_den -> unit_label_of_subdivision_den subdivision_den
    | None -> canonical_unit_label
  in
  {
    title = Filename.basename path;
    voices;
    pedals;
    controls;
    tempo_changes;
    total_units = max 1 total_units;
    unit_label;
    time_signature_num;
    time_signature_den;
    units_per_bar;
    initial_bpm;
  }

let note_count (score : t) =
  Array.fold_left (fun acc (voice : voice) -> acc + Array.length voice.notes) 0
    score.voices

let parse_int ~line ~field raw =
  match int_of_string_opt raw with
  | Some n -> n
  | None ->
      raise
        (Parse_error
           (Printf.sprintf "line %d: invalid integer for %s: %S" line field raw))

let strip_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    String.sub s 1 (len - 2)
  else s

let split_kv ~line token =
  match String.split_on_char ':' token with
  | [ key; value ] -> (String.trim key, String.trim value)
  | _ ->
      raise
        (Parse_error
           (Printf.sprintf "line %d: expected key:value token, got %S" line token))

let parse_meter ~line raw =
  match String.split_on_char '/' (String.trim raw) with
  | [ n; d ] ->
      let num = parse_int ~line ~field:"meter numerator" (String.trim n) in
      let den = parse_int ~line ~field:"meter denominator" (String.trim d) in
      if num <= 0 || den <= 0 then
        raise
          (Parse_error
             (Printf.sprintf "line %d: meter must be strictly positive" line));
      (num, den)
  | _ ->
      raise
        (Parse_error
           (Printf.sprintf "line %d: expected meter as N/D, got %S" line raw))

let parse_tempo_q ~line raw =
  let raw = String.trim raw in
  let v =
    if String.starts_with ~prefix:"q=" raw then
      String.sub raw 2 (String.length raw - 2)
    else raw
  in
  clamp min_bpm max_bpm (parse_int ~line ~field:"tempo (q=...)" (String.trim v))

let parse_unit_fraction unit_label =
  match String.split_on_char '/' (String.trim unit_label) with
  | [ "1"; d ] -> int_of_string_opt (String.trim d)
  | _ -> None

let derive_units_per_bar ~num ~den ~unit_label =
  match parse_unit_fraction unit_label with
  | Some unit_den when unit_den > 0 && unit_den mod den = 0 ->
      max 1 (num * (unit_den / den))
  | _ -> 16

let normalize_unit_label raw =
  let s = String.trim raw in
  if s = "" then canonical_unit_label
  else
    match parse_unit_fraction s with
    | Some d when d > 0 -> Printf.sprintf "1/%d" d
    | _ -> canonical_unit_label

let of_text content =
  let lines = String.split_on_char '\n' content in
  let header_seen = ref false in
  let title = ref "Untitled score" in
  let meter_num = ref 4 in
  let meter_den = ref 4 in
  let unit_label = ref canonical_unit_label in
  let units_per_bar = ref None in
  let initial_bpm = ref default_bpm in
  let voices_rev = ref [] in
  let pedals_rev = ref [] in
  let controls_rev = ref [] in
  let tempo_changes_rev = ref [] in
  let current_voice : (string * instrument * note list ref) option ref = ref None in
  let push_current () =
    match !current_voice with
    | None -> ()
    | Some (name, instrument, notes_ref) ->
        let notes =
          !notes_ref
          |> List.rev
          |> List.sort (fun (a : note) (b : note) ->
                 match compare a.start_unit b.start_unit with
                 | 0 -> compare a.midi b.midi
                 | n -> n)
          |> Array.of_list
        in
        voices_rev := { name; instrument; notes } :: !voices_rev;
        current_voice := None
  in
  List.iteri
    (fun idx raw_line ->
      let line_no = idx + 1 in
      let line = String.trim raw_line in
      if line <> "" && line.[0] <> '#' then
        if not !header_seen then
          if String.equal line "tempo-score v1" || String.equal line "tempo-score v2"
          then header_seen := true
          else
            raise
              (Parse_error
                 (Printf.sprintf
                    "line %d: expected header `tempo-score v1` or `tempo-score v2`"
                    line_no))
        else if String.starts_with ~prefix:"title:" line then
          title := strip_quotes (String.trim (String.sub line 6 (String.length line - 6)))
        else if String.starts_with ~prefix:"meter:" line then
          let raw = String.trim (String.sub line 6 (String.length line - 6)) in
          let num, den = parse_meter ~line:line_no raw in
          meter_num := num;
          meter_den := den
        else if String.starts_with ~prefix:"unit:" line then
          unit_label :=
            normalize_unit_label
              (strip_quotes (String.trim (String.sub line 5 (String.length line - 5))))
        else if String.starts_with ~prefix:"grid:" line then
          unit_label :=
            normalize_unit_label
              (strip_quotes (String.trim (String.sub line 5 (String.length line - 5))))
        else if String.starts_with ~prefix:"subdivision:" line then
          unit_label :=
            normalize_unit_label
              (strip_quotes
                 (String.trim (String.sub line 12 (String.length line - 12))))
        else if String.starts_with ~prefix:"units_per_bar:" line then
          units_per_bar :=
            Some
              (parse_int ~line:line_no ~field:"units_per_bar"
                 (String.trim
                    (String.sub line 14 (String.length line - 14))))
        else if String.starts_with ~prefix:"tempo:" line then
          initial_bpm :=
            parse_tempo_q ~line:line_no
              (String.trim (String.sub line 6 (String.length line - 6)))
        else if String.starts_with ~prefix:"bpm:" line then
          initial_bpm :=
            clamp min_bpm max_bpm
              (parse_int ~line:line_no ~field:"bpm"
                 (String.trim (String.sub line 4 (String.length line - 4))))
        else if String.starts_with ~prefix:"tempo_change " line then (
          let payload = String.sub line 13 (String.length line - 13) in
          let fields = Hashtbl.create 6 in
          String.split_on_char ' ' payload
          |> List.filter (fun t -> String.trim t <> "")
          |> List.iter (fun token ->
                 let key, value = split_kv ~line:line_no token in
                 Hashtbl.replace fields key value);
          let get key =
            match Hashtbl.find_opt fields key with
            | Some v -> v
            | None ->
                raise
                  (Parse_error
                     (Printf.sprintf
                        "line %d: missing tempo_change field %S" line_no key))
          in
          let units_per_bar_now =
            Option.value !units_per_bar
              ~default:
                (derive_units_per_bar ~num:!meter_num ~den:!meter_den
                   ~unit_label:!unit_label)
          in
          let start_unit =
            match (Hashtbl.find_opt fields "start", Hashtbl.find_opt fields "bar", Hashtbl.find_opt fields "step") with
            | Some s, _, _ -> parse_int ~line:line_no ~field:"tempo_change start" s
            | None, Some b, Some step ->
                let bar = parse_int ~line:line_no ~field:"tempo_change bar" b in
                let step = parse_int ~line:line_no ~field:"tempo_change step" step in
                ((max 1 bar - 1) * units_per_bar_now) + (max 1 step - 1)
            | _ ->
                raise
                  (Parse_error
                     (Printf.sprintf
                        "line %d: tempo_change requires start:<u> or bar:<m> step:<k>"
                        line_no))
          in
          let bpm = parse_tempo_q ~line:line_no (get "q") in
          tempo_changes_rev := { start_unit = max 0 start_unit; bpm } :: !tempo_changes_rev)
        else if String.starts_with ~prefix:"voice:" line then (
          push_current ();
          let payload = String.trim (String.sub line 6 (String.length line - 6)) in
          let parts = List.map String.trim (String.split_on_char '|' payload) in
          match parts with
          | [] ->
              raise
                (Parse_error
                   (Printf.sprintf "line %d: missing voice description" line_no))
          | name_raw :: attrs ->
              let name = strip_quotes name_raw in
              let channel = ref 0 in
              let bank = ref 0 in
              let preset = ref 0 in
              List.iter
                (fun attr_block ->
                  String.split_on_char ' ' attr_block
                  |> List.filter (fun t -> String.trim t <> "")
                  |> List.iter (fun token ->
                         let key, value = split_kv ~line:line_no token in
                         match key with
                         | "ch" | "channel" ->
                             channel :=
                               parse_int ~line:line_no ~field:"voice channel" value
                         | "bank" ->
                             bank := parse_int ~line:line_no ~field:"voice bank" value
                         | "prog" | "preset" ->
                             preset := parse_int ~line:line_no ~field:"voice preset" value
                         | _ ->
                             raise
                               (Parse_error
                                  (Printf.sprintf
                                     "line %d: unsupported voice field %S"
                                     line_no key))))
                attrs;
              current_voice :=
                Some
                  ( name,
                    { channel = !channel; bank = !bank; preset = !preset },
                    ref [] ))
        else if String.starts_with ~prefix:"note " line then (
          match !current_voice with
          | None ->
              raise
                (Parse_error
                   (Printf.sprintf
                      "line %d: note declared before any voice section" line_no))
          | Some (_name, _instrument, notes_ref) ->
              let payload = String.sub line 5 (String.length line - 5) in
              let fields = Hashtbl.create 8 in
              String.split_on_char ' ' payload
              |> List.filter (fun t -> String.trim t <> "")
              |> List.iter (fun token ->
                     let key, value = split_kv ~line:line_no token in
                     Hashtbl.replace fields key value);
              let get_opt key = Hashtbl.find_opt fields key in
              let get key =
                match get_opt key with
                | Some v -> v
                | None ->
                    raise
                      (Parse_error
                         (Printf.sprintf "line %d: missing note field %S" line_no key))
              in
              let units_per_bar_now =
                Option.value !units_per_bar
                  ~default:
                    (derive_units_per_bar ~num:!meter_num ~den:!meter_den
                       ~unit_label:!unit_label)
              in
              let start_unit =
                match (get_opt "start", get_opt "bar", get_opt "step") with
                | Some s, _, _ -> parse_int ~line:line_no ~field:"note start" s
                | None, Some b, Some step ->
                    let bar = parse_int ~line:line_no ~field:"note bar" b in
                    let step = parse_int ~line:line_no ~field:"note step" step in
                    ((max 1 bar - 1) * units_per_bar_now) + (max 1 step - 1)
                | _ ->
                    raise
                      (Parse_error
                         (Printf.sprintf
                            "line %d: note requires either start:<u> or bar:<m> step:<k>"
                            line_no))
              in
              let duration_units =
                parse_int ~line:line_no ~field:"note dur" (get "dur")
              in
              let midi = parse_int ~line:line_no ~field:"note midi" (get "midi") in
              let velocity = parse_int ~line:line_no ~field:"note vel" (get "vel") in
              notes_ref :=
                {
                  start_unit = max 0 start_unit;
                  duration_units = max 1 duration_units;
                  midi = clamp 0 127 midi;
                  volume = float_of_int (clamp 1 127 velocity) /. 127.0;
                }
                :: !notes_ref)
        else if String.starts_with ~prefix:"pedal " line then (
          let payload = String.sub line 6 (String.length line - 6) in
          let fields = Hashtbl.create 8 in
          String.split_on_char ' ' payload
          |> List.filter (fun t -> String.trim t <> "")
          |> List.iter (fun token ->
                 let key, value = split_kv ~line:line_no token in
                 Hashtbl.replace fields key value);
          let get_opt key = Hashtbl.find_opt fields key in
          let get key =
            match get_opt key with
            | Some v -> v
            | None ->
                raise
                  (Parse_error
                     (Printf.sprintf "line %d: missing pedal field %S" line_no key))
          in
          let units_per_bar_now =
            Option.value !units_per_bar
              ~default:
                (derive_units_per_bar ~num:!meter_num ~den:!meter_den
                   ~unit_label:!unit_label)
          in
          let start_unit =
            match (get_opt "start", get_opt "bar", get_opt "step") with
            | Some s, _, _ -> parse_int ~line:line_no ~field:"pedal start" s
            | None, Some b, Some step ->
                let bar = parse_int ~line:line_no ~field:"pedal bar" b in
                let step = parse_int ~line:line_no ~field:"pedal step" step in
                ((max 1 bar - 1) * units_per_bar_now) + (max 1 step - 1)
            | _ ->
                raise
                  (Parse_error
                     (Printf.sprintf
                        "line %d: pedal requires either start:<u> or bar:<m> step:<k>"
                        line_no))
          in
          let channel = parse_int ~line:line_no ~field:"pedal ch" (get "ch") in
          let value_raw =
            match Hashtbl.find_opt fields "val" with
            | Some v -> v
            | None -> get "value"
          in
          let value = parse_int ~line:line_no ~field:"pedal val" value_raw in
          pedals_rev :=
            { start_unit = max 0 start_unit; channel = clamp 0 15 channel; value = clamp 0 127 value }
            :: !pedals_rev)
        else if String.starts_with ~prefix:"control " line then (
          let payload = String.sub line 8 (String.length line - 8) in
          let fields = Hashtbl.create 8 in
          String.split_on_char ' ' payload
          |> List.filter (fun t -> String.trim t <> "")
          |> List.iter (fun token ->
                 let key, value = split_kv ~line:line_no token in
                 Hashtbl.replace fields key value);
          let get_opt key = Hashtbl.find_opt fields key in
          let get key =
            match get_opt key with
            | Some v -> v
            | None ->
                raise
                  (Parse_error
                     (Printf.sprintf "line %d: missing control field %S" line_no key))
          in
          let units_per_bar_now =
            Option.value !units_per_bar
              ~default:
                (derive_units_per_bar ~num:!meter_num ~den:!meter_den
                   ~unit_label:!unit_label)
          in
          let start_unit =
            match (get_opt "start", get_opt "bar", get_opt "step") with
            | Some s, _, _ -> parse_int ~line:line_no ~field:"control start" s
            | None, Some b, Some step ->
                let bar = parse_int ~line:line_no ~field:"control bar" b in
                let step = parse_int ~line:line_no ~field:"control step" step in
                ((max 1 bar - 1) * units_per_bar_now) + (max 1 step - 1)
            | _ ->
                raise
                  (Parse_error
                     (Printf.sprintf
                        "line %d: control requires start:<u> or bar:<m> step:<k>"
                        line_no))
          in
          let channel = parse_int ~line:line_no ~field:"control ch" (get "ch") in
          let control =
            match get_opt "cc" with
            | Some cc -> parse_int ~line:line_no ~field:"control cc" cc
            | None -> parse_int ~line:line_no ~field:"control control" (get "control")
          in
          let value_raw =
            match get_opt "val" with
            | Some v -> v
            | None -> get "value"
          in
          let value = parse_int ~line:line_no ~field:"control val" value_raw in
          controls_rev :=
            {
              start_unit = max 0 start_unit;
              channel = clamp 0 15 channel;
              control = clamp 0 127 control;
              value = clamp 0 127 value;
            }
            :: !controls_rev)
        else
          raise
            (Parse_error
               (Printf.sprintf "line %d: unsupported directive %S" line_no line)))
    lines;
  if not !header_seen then
    raise (Parse_error "missing header `tempo-score v1`");
  push_current ();
  let voices = Array.of_list (List.rev !voices_rev) in
  let pedals =
    !pedals_rev
    |> List.sort (fun (a : pedal) (b : pedal) ->
           match compare a.start_unit b.start_unit with
           | 0 -> compare a.channel b.channel
           | n -> n)
    |> Array.of_list
  in
  let controls =
    let controls = List.rev !controls_rev in
    let with_legacy_pedals =
      List.rev_map
        (fun (p : pedal) ->
          {
            start_unit = p.start_unit;
            channel = p.channel;
            control = 64;
            value = p.value;
          })
        !pedals_rev
      @ controls
    in
    with_legacy_pedals
    |> List.sort (fun (a : control) (b : control) ->
           match compare a.start_unit b.start_unit with
           | 0 -> (
               match compare a.channel b.channel with
               | 0 -> (
                   match compare a.control b.control with
                   | 0 -> compare a.value b.value
                   | n -> n)
               | n -> n)
           | n -> n)
    |> Array.of_list
  in
  let tempo_changes =
    !tempo_changes_rev
    |> List.sort (fun (a : tempo_change) (b : tempo_change) ->
           match compare a.start_unit b.start_unit with
           | 0 -> compare a.bpm b.bpm
           | n -> n)
    |> Array.of_list
  in
  let total_units =
    let note_max =
      Array.fold_left
        (fun acc (v : voice) ->
          Array.fold_left
            (fun local (n : note) -> max local (n.start_unit + n.duration_units))
            acc v.notes)
        0 voices
    in
    let with_controls =
      Array.fold_left (fun acc (c : control) -> max acc (c.start_unit + 1)) note_max
        controls
    in
    Array.fold_left (fun acc (t : tempo_change) -> max acc (t.start_unit + 1))
      with_controls tempo_changes
  in
  {
    title = !title;
    voices;
    pedals;
    controls;
    tempo_changes;
    total_units = max 1 total_units;
    unit_label = !unit_label;
    time_signature_num = !meter_num;
    time_signature_den = !meter_den;
    units_per_bar =
      Option.value !units_per_bar
        ~default:
          (derive_units_per_bar ~num:!meter_num ~den:!meter_den
             ~unit_label:!unit_label);
    initial_bpm = !initial_bpm;
  }

let of_text_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      of_text content)

let velocity_of_volume volume =
  let raw = int_of_float (volume *. 127.0) in
  clamp 1 127 raw

let maybe_quote s =
  if String.contains s '|' || String.contains s ':' then Printf.sprintf "%S" s
  else s

let to_text (score : t) =
  let b = Buffer.create 1024 in
  let add fmt = Printf.ksprintf (Buffer.add_string b) fmt in
  add "tempo-score v2\n";
  add "title: %s\n" (maybe_quote score.title);
  add "meter: %d/%d\n" score.time_signature_num score.time_signature_den;
  let unit_text =
    match parse_unit_fraction score.unit_label with
    | Some d when d > 0 -> Printf.sprintf "1/%d" d
    | _ -> canonical_unit_label
  in
  add "subdivision: %s\n" (maybe_quote unit_text);
  add "tempo: q=%d\n\n" score.initial_bpm;
  let printed_tempo_change = ref false in
  Array.iter
    (fun (t : tempo_change) ->
      if not (t.start_unit = 0 && t.bpm = score.initial_bpm) then (
        let bar = (t.start_unit / score.units_per_bar) + 1 in
        let step = (t.start_unit mod score.units_per_bar) + 1 in
        add "tempo_change bar:%d step:%d q:%d\n" bar step t.bpm;
        printed_tempo_change := true))
    score.tempo_changes;
  if !printed_tempo_change then add "\n";
  Array.iteri
    (fun idx (voice : voice) ->
      add "voice: %s | ch:%d bank:%d prog:%d\n"
        (maybe_quote voice.name)
        voice.instrument.channel voice.instrument.bank voice.instrument.preset;
      Array.iter
        (fun (n : note) ->
          let bar = (n.start_unit / score.units_per_bar) + 1 in
          let step = (n.start_unit mod score.units_per_bar) + 1 in
          add "note bar:%d step:%d dur:%d midi:%d vel:%d\n" bar step n.duration_units
            n.midi (velocity_of_volume n.volume))
        voice.notes;
      if idx < Array.length score.voices - 1 then add "\n")
    score.voices;
  if Array.length score.controls > 0 then (
    add "\n";
    Array.iter
      (fun (c : control) ->
        let bar = (c.start_unit / score.units_per_bar) + 1 in
        let step = (c.start_unit mod score.units_per_bar) + 1 in
        add "control bar:%d step:%d ch:%d cc:%d val:%d\n" bar step c.channel c.control
          c.value)
      score.controls);
  Buffer.contents b

let write_text_file ~path score =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc (to_text score);
      output_char oc '\n')

let binary_magic_v1 = "TEMPO_SCORE_BIN_V1\000"
let binary_magic_v2 = "TEMPO_SCORE_BIN_V2\000"
let binary_magic_v3 = "TEMPO_SCORE_BIN_V3\000"

type legacy_t = {
  title : string;
  voices : voice array;
  pedals : pedal array;
  total_units : int;
  unit_label : string;
  time_signature_num : int;
  time_signature_den : int;
  units_per_bar : int;
  initial_bpm : int;
}

let of_legacy (v : legacy_t) : t =
  let controls =
    v.pedals
    |> Array.map (fun (p : pedal) ->
           {
             start_unit = p.start_unit;
             channel = p.channel;
             control = 64;
             value = p.value;
           })
  in
  {
    title = v.title;
    voices = v.voices;
    pedals = v.pedals;
    controls;
    tempo_changes = [||];
    total_units = v.total_units;
    unit_label = normalize_unit_label v.unit_label;
    time_signature_num = v.time_signature_num;
    time_signature_den = v.time_signature_den;
    units_per_bar = v.units_per_bar;
    initial_bpm = v.initial_bpm;
  }

let of_binary payload =
  let magic_len = String.length binary_magic_v1 in
  if Bytes.length payload < magic_len then
    raise (Unsupported_binary_format "payload too short");
  let prefix = Bytes.sub_string payload 0 magic_len in
  if
    (not (String.equal prefix binary_magic_v1))
    && not (String.equal prefix binary_magic_v2)
    && not (String.equal prefix binary_magic_v3)
  then
    raise (Unsupported_binary_format "invalid magic/version");
  let try_decode_current () =
    try Some (Marshal.from_bytes payload magic_len : t) with _ -> None
  in
  let try_decode_legacy () =
    try Some (of_legacy (Marshal.from_bytes payload magic_len : legacy_t)) with _ -> None
  in
  match try_decode_current () with
  | Some score -> score
  | None -> (
      match try_decode_legacy () with
      | Some score -> score
      | None ->
          raise (Unsupported_binary_format "cannot decode marshal payload"))

let of_binary_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      let raw = really_input_string ic len in
      of_binary (Bytes.of_string raw))

let to_binary score =
  let body = Marshal.to_bytes score [ Marshal.No_sharing ] in
  let out = Bytes.create (String.length binary_magic_v3 + Bytes.length body) in
  Bytes.blit_string binary_magic_v3 0 out 0 (String.length binary_magic_v3);
  Bytes.blit body 0 out (String.length binary_magic_v3) (Bytes.length body);
  out

let write_binary_file ~path score =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      let payload = to_binary score in
      output_bytes oc payload)
