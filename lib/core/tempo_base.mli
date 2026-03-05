(** Shared signal and instant primitives.

    This module is the common substrate used both by {!Tempo.Core} and
    {!Tempo.Low_level}. Most users should prefer {!Tempo.Core}; this module is
    exposed so that higher-level libraries can build structured APIs while
    sharing the same signal and instant vocabulary. *)

type ('emit, 'agg, 'mode) signal_core = ('emit, 'agg, 'mode) Tempo_types.signal_core
type 'a signal = ('a, 'a, Tempo_types.event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, Tempo_types.aggregate) signal_core
type kill = Tempo_types.kill
type thread = Tempo_types.thread

val new_signal : unit -> 'a signal
val new_signal_agg :
  initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal
val emit : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit
val await : ('emit, 'agg, 'mode) signal_core -> 'agg
val await_immediate : 'a signal -> 'a
val pause : unit -> unit
val peek : ('emit, 'agg, 'mode) signal_core -> 'agg option
(** Inspect the current instant-local value of a signal, if any. *)

val is_present : ('emit, 'agg, 'mode) signal_core -> bool
(** Test whether a signal is present in the current instant. *)
