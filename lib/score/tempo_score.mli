(** Tempo score model and MIDI-to-Tempo conversion.

    This module defines a Tempo-oriented logical score format where:
    - one logical unit is the minimal scheduling quantum chosen for a score;
    - notes are represented as [start_unit] + [duration_units];
    - playback scheduling can be owned by Tempo (instants/inter-instants). *)

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

val default : t
(** Built-in score used as deterministic fallback/demo material. *)

val of_midi_file : string -> t
(** Convert a MIDI file into a Tempo score.

    The function keeps MIDI parsing concerns in [tempo-fluidsynth], then derives
    a Tempo logical unit so notes can be scheduled by Tempo processes. *)

val note_count : t -> int
(** Total number of notes across all voices. *)

exception Parse_error of string
(** Raised when textual score parsing fails. *)

val of_text : string -> t
(** Parse a score from the compact textual format. *)

val of_text_file : string -> t
(** Parse a score from a text file in the compact textual format. *)

val to_text : t -> string
(** Serialize a score to the compact textual format. *)

val write_text_file : path:string -> t -> unit
(** Write a score to a text file in the compact textual format. *)

exception Unsupported_binary_format of string
(** Raised when binary payload does not match tempo-score format/version. *)

val of_binary : bytes -> t
(** Parse a score from binary tempo-score payload.

    Supports current binary v3 payloads and legacy v1/v2 payloads. *)

val of_binary_file : string -> t
(** Parse a score from a binary tempo-score file. *)

val to_binary : t -> bytes
(** Serialize a score to binary tempo-score payload (v3). *)

val write_binary_file : path:string -> t -> unit
(** Write a score to a binary tempo-score file. *)
