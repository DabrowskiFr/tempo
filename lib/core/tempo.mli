(** Tempo runtime.

    Recommended reading order:
    1. {!module:Core} for ordinary synchronous programs.
    2. {!module:Constructs} for higher-level reactive operators.
    3. {!module:Observe} when testing or instrumenting programs.
    4. {!module:Low_level} only to build custom operators or bridges. *)

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
val when_ : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val watch : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val parallel : (unit -> unit) list -> unit
val loop : (unit -> unit) -> unit -> 'a
val present_then_else :
  ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> (unit -> unit) -> unit

type 'input interactive_source = {
  poll : unit -> 'input option;
  wait : unit -> unit;
}
(** Host-side interactive source.

    [poll] imports any input already available.
    [wait] blocks until the host has something that may wake the runtime. *)

type wakeup
(** Wakeup token shared between the interactive runtime and external bridges. *)

val current_wakeup : unit -> wakeup option
val notify_wakeup : wakeup -> unit
val register_wakeup_poller : wakeup -> (unit -> bool) -> unit
val emit_from_host : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit

val execute :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ?output:('output -> unit)
  -> ('input signal -> 'output signal -> unit)
  -> unit

val run_interactive :
     ?output:('output -> unit)
  -> input:'input interactive_source
  -> ('input signal -> 'output signal -> unit)
  -> unit

module Core : module type of Tempo_core_api
module Constructs : module type of Tempo_constructs
module Observe : module type of Tempo_observe
module Meta : module type of Tempo_meta

module Low_level : sig
  include module type of Tempo_low_level
end
