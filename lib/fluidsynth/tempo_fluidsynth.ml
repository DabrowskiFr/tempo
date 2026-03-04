type t

type midi_event =
  | Note_on of int * int * int
  | Note_off of int * int
  | Control_change of int * int * int
  | Program_change of int * int

type midi_file = {
    division : int;
    tempo_us_per_quarter : int;
    events : (int * midi_event) list;
  }

external create_native : string -> float -> t = "caml_tempo_fluidsynth_create"
external shutdown : t -> unit = "caml_tempo_fluidsynth_shutdown"
external program_select : t -> channel:int -> bank:int -> preset:int -> unit = "caml_tempo_fluidsynth_program_select"
external note_on : t -> channel:int -> key:int -> velocity:int -> unit = "caml_tempo_fluidsynth_note_on"
external note_off : t -> channel:int -> key:int -> unit = "caml_tempo_fluidsynth_note_off"
external all_notes_off : t -> channel:int -> unit = "caml_tempo_fluidsynth_all_notes_off"
external import_midi_file : string -> midi_file = "caml_tempo_fluidsynth_import_midi_file"

let default_soundfont_candidates =
  [
    Sys.getenv_opt "TEMPO_SOUNDFONT";
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
