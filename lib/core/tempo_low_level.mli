(** Advanced scheduler primitives.

    {!Tempo.Low_level} is intended for library and operator authors. Regular
    Tempo programs should prefer {!Tempo.Core} and {!Tempo.Constructs}. *)

type kill = Tempo_types.kill
type thread = Tempo_types.thread

val new_kill : unit -> kill
val abort_kill : kill -> unit
val with_kill : kill -> (unit -> unit) -> unit

val with_guard :
  ('emit, 'agg, 'mode) Tempo_types.signal_core -> (unit -> unit) -> unit
(** Raw building block behind {!Tempo.when_}. *)

val fork : (unit -> unit) -> thread
val join : thread -> unit
val peek : ('emit, 'agg, 'mode) Tempo_types.signal_core -> 'agg option
val is_present : ('emit, 'agg, 'mode) Tempo_types.signal_core -> bool
