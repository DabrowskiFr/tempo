open Effect
open Tempo_types

type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_types.signal_core

type 'a signal = ('a, 'a, event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core
type kill = Tempo_types.kill
type thread = Tempo_types.thread

let new_signal : unit -> 'a signal = fun () -> perform (New_signal ())

let new_signal_agg :
    initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal
    =
 fun ~initial ~combine -> perform (New_signal_agg (initial, combine))

let emit : type emit agg mode. (emit, agg, mode) signal_core -> emit -> unit =
 fun s v -> perform (Emit (s, v))

let await : type emit agg mode. (emit, agg, mode) signal_core -> agg =
 fun s -> perform (Await s)

let await_immediate : 'a signal -> 'a = fun s -> perform (Await_immediate s)
let pause : unit -> unit = fun () -> perform Pause
let peek s = s.value
let is_present s = s.present
