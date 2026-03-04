(** Derived reactive constructs built on top of {!Tempo.Core}. *)

val present_then_else :
  ('emit, 'agg, 'mode) Tempo_types.signal_core ->
  (unit -> unit) ->
  (unit -> unit) ->
  unit

val after_n : int -> (unit -> unit) -> unit
val every_n : int -> (unit -> unit) -> unit
val timeout : int -> on_timeout:(unit -> unit) -> (unit -> unit) -> unit
val cooldown :
  int -> ('emit, 'agg, 'mode) Tempo_types.signal_core -> (unit -> unit) -> unit
val rising_edge : bool Tempo_base.signal -> unit
val falling_edge : bool Tempo_base.signal -> unit
val edge_by : ('a -> 'a -> bool) -> 'a Tempo_base.signal -> unit
val pulse_n : int -> unit Tempo_base.signal
val supervise_until :
  ('emit, 'agg, 'mode) Tempo_types.signal_core -> (unit -> unit) -> unit
