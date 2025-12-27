val fresh_signal_id : Tempo_types.scheduler_state -> int
val register_signal : Tempo_types.scheduler_state -> ('e, 'a, 'm) Tempo_types.signal_core -> unit

val fresh_event_signal : Tempo_types.scheduler_state -> 'a Tempo_types.signal
val fresh_aggregate_signal :
  Tempo_types.scheduler_state ->
  initial:'agg ->
  combine:('agg -> 'emit -> 'agg) ->
  ('emit, 'agg) Tempo_types.agg_signal

val guard_ok : Tempo_types.any_signal list -> bool
val missing_guards : Tempo_types.any_signal list -> Tempo_types.any_signal list
val finalize_signals : Tempo_types.scheduler_state -> unit
