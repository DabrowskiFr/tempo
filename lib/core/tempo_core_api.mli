(** Canonical synchronous programming API.

    This is the recommended entry point for most Tempo programs. *)

type ('emit, 'agg, 'mode) signal_core = ('emit, 'agg, 'mode) Tempo_types.signal_core
type 'a signal = ('a, 'a, Tempo_types.event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, Tempo_types.aggregate) signal_core

val new_signal : unit -> 'a signal
val new_signal_agg :
  initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal
val emit : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit
val await : ('emit, 'agg, 'mode) signal_core -> 'agg
val await_immediate : 'a signal -> 'a
val pause : unit -> unit

val when_ : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val watch : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val parallel : (unit -> unit) list -> unit
val loop : (unit -> unit) -> unit -> 'a
val idle : unit -> 'a
