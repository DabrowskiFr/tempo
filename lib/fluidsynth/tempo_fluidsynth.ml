type t

type midi_event =
  | Note_on of int * int * int
  | Note_off of int * int
  | Control_change of int * int * int
  | Program_change of int * int

type midi_file = {
    division : int;
    tempo_us_per_quarter : int;
    time_signature : (int * int) option;
    events : (int * midi_event) list;
  }

external create_native : string -> float -> t = "caml_tempo_fluidsynth_create"
external shutdown : t -> unit = "caml_tempo_fluidsynth_shutdown"
external program_select : t -> channel:int -> bank:int -> preset:int -> unit = "caml_tempo_fluidsynth_program_select"
external note_on : t -> channel:int -> key:int -> velocity:int -> unit = "caml_tempo_fluidsynth_note_on"
external note_off : t -> channel:int -> key:int -> unit = "caml_tempo_fluidsynth_note_off"
external all_notes_off : t -> channel:int -> unit = "caml_tempo_fluidsynth_all_notes_off"

let default_soundfont_candidates =
  [
    Sys.getenv_opt "TEMPO_SOUNDFONT";
    Some "/Users/fredericdabrowski/Repos/tempo/tempo-dev/tempo/applications/advanced/music_score_player/assets/GeneralUser-GS.sf2";
    Some "/opt/homebrew/Cellar/fluid-synth/2.5.3/share/fluid-synth/sf2/VintageDreamsWaves-v2.sf3";
    Some "/opt/homebrew/Cellar/fluid-synth/2.5.3/share/fluid-synth/sf2/VintageDreamsWaves-v2.sf2";
  ]

let default_soundfont () =
  List.find_map
    (function
      | None -> None
      | Some path -> if Sys.file_exists path then Some path else None)
    default_soundfont_candidates

let create ?soundfont ?(gain = 0.7) () =
  let path =
    match soundfont with
    | Some path -> path
    | None -> (
        match default_soundfont () with
        | Some path -> path
        | None ->
            invalid_arg
              "No SoundFont found. Set TEMPO_SOUNDFONT or install FluidSynth soundfonts.")
  in
  create_native path gain

exception Midi_parse_error of string

type reader = {
  data : bytes;
  mutable pos : int;
}

let midi_error fmt = Printf.ksprintf (fun s -> raise (Midi_parse_error s)) fmt

let remaining r = Bytes.length r.data - r.pos

let ensure r n =
  if remaining r < n then midi_error "unexpected end of MIDI file"

let read_u8 r =
  ensure r 1;
  let v = Char.code (Bytes.get r.data r.pos) in
  r.pos <- r.pos + 1;
  v

let read_u16_be r =
  let a = read_u8 r in
  let b = read_u8 r in
  (a lsl 8) lor b

let read_u32_be r =
  let a = read_u8 r in
  let b = read_u8 r in
  let c = read_u8 r in
  let d = read_u8 r in
  (((a lsl 8) lor b) lsl 16) lor ((c lsl 8) lor d)

let read_string r n =
  ensure r n;
  let s = Bytes.sub_string r.data r.pos n in
  r.pos <- r.pos + 1 * n;
  s

let skip r n =
  ensure r n;
  r.pos <- r.pos + n

let read_vlq r =
  let rec loop acc count =
    if count >= 4 then midi_error "invalid variable-length quantity";
    let byte = read_u8 r in
    let acc = (acc lsl 7) lor (byte land 0x7f) in
    if byte land 0x80 = 0 then acc else loop acc (count + 1)
  in
  loop 0 0

let parse_track_events bytes =
  let r = { data = bytes; pos = 0 } in
  let absolute_tick = ref 0 in
  let running_status = ref None in
  let events = ref [] in
  let tempos = ref [] in
  let time_signatures = ref [] in
  let add_event tick ev = events := (tick, ev) :: !events in
  let add_tempo tick tempo = tempos := (tick, tempo) :: !tempos in
  let add_time_signature tick num den = time_signatures := (tick, (num, den)) :: !time_signatures in
  let read_data_byte () =
    let b = read_u8 r in
    if b land 0x80 <> 0 then midi_error "expected MIDI data byte";
    b
  in
  let rec loop () =
    if remaining r = 0 then ()
    else begin
      absolute_tick := !absolute_tick + read_vlq r;
      let first = read_u8 r in
      let status, first_data_opt =
        if first land 0x80 <> 0 then (
          running_status :=
            (if first < 0xF0 then Some first else !running_status);
          (first, None))
        else
          match !running_status with
          | Some status -> (status, Some first)
          | None -> midi_error "running status used before status byte"
      in
      begin
        match status land 0xF0, status with
        | _, 0xFF ->
            let meta_type = read_u8 r in
            let len = read_vlq r in
            if meta_type = 0x2F then skip r len
            else if meta_type = 0x51 && len = 3 then (
              let a = read_u8 r in
              let b = read_u8 r in
              let c = read_u8 r in
              add_tempo !absolute_tick ((a lsl 16) lor (b lsl 8) lor c))
            else if meta_type = 0x58 && len = 4 then (
              let numerator = read_u8 r in
              let denominator_power = read_u8 r in
              ignore (read_u8 r);
              ignore (read_u8 r);
              add_time_signature !absolute_tick numerator (1 lsl denominator_power))
            else skip r len
        | _, 0xF0 | _, 0xF7 ->
            let len = read_vlq r in
            skip r len
        | kind, _ ->
            let channel = status land 0x0F in
            let read_first_data () =
              match first_data_opt with
              | Some v -> v
              | None -> read_data_byte ()
            in
            begin match kind with
            | 0x80 ->
                let key = read_first_data () in
                ignore (read_data_byte ());
                add_event !absolute_tick (Note_off (channel, key))
            | 0x90 ->
                let key = read_first_data () in
                let velocity = read_data_byte () in
                add_event !absolute_tick (Note_on (channel, key, velocity))
            | 0xB0 ->
                let control = read_first_data () in
                let value = read_data_byte () in
                add_event !absolute_tick (Control_change (channel, control, value))
            | 0xC0 ->
                let program = read_first_data () in
                add_event !absolute_tick (Program_change (channel, program))
            | 0xA0 | 0xE0 ->
                ignore (read_first_data ());
                ignore (read_data_byte ())
            | 0xD0 ->
                ignore (read_first_data ())
            | _ ->
                midi_error "unsupported MIDI status 0x%02X" status
            end
      end;
      loop ()
    end
  in
  loop ();
  (!events, !tempos, !time_signatures)

let import_midi_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let read_exact n =
        let buf = really_input_string ic n in
        { data = Bytes.of_string buf; pos = 0 }
      in
      let header = read_exact 14 in
      let chunk = read_string header 4 in
      if chunk <> "MThd" then midi_error "missing MThd chunk";
      let header_len = read_u32_be header in
      if header_len <> 6 then midi_error "unexpected MIDI header length %d" header_len;
      let format = read_u16_be header in
      let track_count = read_u16_be header in
      let division = read_u16_be header in
      if format <> 0 && format <> 1 then midi_error "unsupported MIDI format %d" format;
      if division land 0x8000 <> 0 then midi_error "SMPTE MIDI timing is not supported";
      let events = ref [] in
      let tempos = ref [] in
      let time_signatures = ref [] in
      for _ = 1 to track_count do
        let chunk_header = read_exact 8 in
        let chunk_type = read_string chunk_header 4 in
        let chunk_len = read_u32_be chunk_header in
        let chunk_data = really_input_string ic chunk_len |> Bytes.of_string in
        if chunk_type = "MTrk" then (
          let track_events, track_tempos, track_time_signatures =
            parse_track_events chunk_data
          in
          events := track_events @ !events;
          tempos := track_tempos @ !tempos;
          time_signatures := track_time_signatures @ !time_signatures)
      done;
      let events =
        List.sort
          (fun (ta, _) (tb, _) -> compare ta tb)
          !events
      in
      let tempo_us_per_quarter =
        match List.sort (fun (ta, _) (tb, _) -> compare ta tb) !tempos with
        | (_, tempo) :: _ -> tempo
        | [] -> 500000
      in
      let time_signature =
        match List.sort (fun (ta, _) (tb, _) -> compare ta tb) !time_signatures with
        | (_, sig_) :: _ -> Some sig_
        | [] -> None
      in
      { division; tempo_us_per_quarter; time_signature; events })
