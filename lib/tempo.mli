type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_types.signal_core

type 'a signal = ('a, 'a, Tempo_types.event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, Tempo_types.aggregate) signal_core
type kill = Tempo_types.kill

val new_signal : unit -> 'a signal
val new_signal_agg :
  initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal

val emit : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit
val await : ('emit, 'agg, 'mode) signal_core -> 'agg
val await_immediate : 'a signal -> 'a
val pause : unit -> unit
val fork : (unit -> unit) -> Tempo_types.thread
val join : Tempo_types.thread -> unit
val when_ : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val watch : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val present_then_else :
  ('emit, 'agg, 'mode) signal_core ->
  (unit -> unit) ->
  (unit -> unit) ->
  unit
val parallel : (unit -> unit) list -> unit

val execute :
  ?instants:int ->
  ?input:(unit -> 'input option) ->
  ?output:('output -> unit) ->
  ('input signal -> 'output signal -> unit) ->
  unit

module Low_level : sig
  type kill = Tempo_types.kill

  val new_kill : unit -> kill
  val abort_kill : kill -> unit
  val with_kill : kill -> (unit -> unit) -> unit
  val fork : (unit -> unit) -> Tempo_types.thread
  val join : Tempo_types.thread -> unit
end
