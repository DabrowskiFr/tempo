type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_core.signal_core

type 'a signal = 'a Tempo_core.signal

val present_then_else :
  ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> (unit -> unit) -> unit

val after_n : int -> (unit -> unit) -> unit
val every_n : int -> (unit -> unit) -> unit
val timeout : int -> on_timeout:(unit -> unit) -> (unit -> unit) -> unit
val cooldown : int -> ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val supervise_until : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val pulse_n : int -> unit signal

val loop : (unit -> unit) -> unit -> 'a
val idle : unit -> 'a
val control : (unit, unit, Tempo_types.event) signal_core -> (unit -> unit) -> 'a
val alternate : unit signal -> (unit -> unit) -> (unit -> unit) -> 'a
