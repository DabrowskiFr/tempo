(** FluidSynth backend for Tempo demos.

    This package uses FluidSynth both as a realtime SoundFont synthesizer and
    as a MIDI file importer. Tempo still owns the logical timeline of the
    playback; FluidSynth is used for pleasant sound rendering and MIDI parsing. *)

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
    tempo_changes_us_per_quarter : (int * int) list;
    time_signature_changes : (int * (int * int)) list;
    events : (int * midi_event) list;
  }

val default_soundfont : unit -> string option
(** Return a likely SoundFont path for the local machine, if one can be found. *)

val create : ?soundfont:string -> ?gain:float -> unit -> t
val shutdown : t -> unit

val program_select : t -> channel:int -> bank:int -> preset:int -> unit
val note_on : t -> channel:int -> key:int -> velocity:int -> unit
val note_off : t -> channel:int -> key:int -> unit
val all_notes_off : t -> channel:int -> unit
val control_change : t -> channel:int -> control:int -> value:int -> unit

val import_midi_file : string -> midi_file
