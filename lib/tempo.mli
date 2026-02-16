(*
 * Tempo - synchronous runtime for OCaml
 * Copyright (C) 2025 Frédéric Dabrowski
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

(** This library revisits the ReactiveML programming model using
    {b algebraic effects} (as provided by OCaml 5.3), instead of the
    continuation-passing and compilation techniques used in the
    original ReactiveML implementation.
    
    Programs are executed according to a synchronous semantics: 
    computation progresses in a sequence of logical instants,
    and all observable effects within an instant are considered to occur
    simultaneously 
    
    For a given set of signal emissions in each instant, the observable
    behavior of purely functional program is deterministic. Internal scheduling and
    execution order are not observable and do not affect the final
    synchronous outcome.
    
    *)

(** {1 Signals }
    Signals are the primary communication mechanism between tasks.
    They come in two flavours:

    - {b Event signals} guarantee at most one [emit] per instant. Presence is a
      boolean flag and the value is delivered during the instant. Multiple emissions
      raise an exception.
    - {b Aggregate signals} may be emitted multiple times within the same
      instant. Values are combined using a user-provided accumulator function, and
      the final accumulated value is visible at the next instant.

    In both cases, presence information is scoped to the current instant. When a
    task waits for a signal, the scheduler suspends it across instants until
    the signal becomes present.*)

(** Signal kinds are tracked with phantom markers so that the type system can
    distinguish single-emission (event) signals from aggregate signals while
    still sharing the same primitives. *)
type event
type aggregate

(** Generalized signal type. ['emit] is the type of values passed to [emit],
    ['agg] is the value observed by [await] (equal to ['emit] for events, but
    possibly different for aggregates), and ['mode] encodes the signal flavour
    using the phantom markers below. *)
type ('emit, 'agg, 'mode) signal_core

(** A value of type ['a signal] represents a single-emission signal (at most one
    [emit] per instant) carrying values of type ['a]. Attempts to emit twice in
    the same instant raise [Invalid_argument]. *)
type 'a signal = ('a, 'a, event) signal_core

(** Aggregate signals can be emitted several times per instant; their values
    are combined using the user-provided accumulator before the aggregated value
    is made visible for the next instant. *)
type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core

(** {1 Signal creation} *)

(** [new_signal ()] creates a new event signal.

    The signal starts absent in the current instant and can be emitted at most
    once per instant. Event signals are compatible with every primitive that
    expects a signal argument; they are also the only signals that support
    {!val:await_immediate}. *)
val new_signal : unit -> 'a signal

(** [new_signal_agg ~initial ~combine] creates an aggregate signal. When the
    signal is emitted several times within the same instant, each value is
    folded into the accumulator using [combine]. The accumulator starts at
    [initial] for the first emission of the instant. Aggregate signals can be
    used with the same primitives as event signals; the sole restriction is that
    {!val:await_immediate} is unavailable because their combined value is only
    produced at the end of the instant. *)
val new_signal_agg :
  initial:'agg ->
  combine:('agg -> 'emit -> 'agg) ->
  ('emit, 'agg) agg_signal

(** {1 Synchronous operators} *)

(** {2 Emission } *)

(** [emit s v] marks [s] as present in the current instant and propagates the
    value [v].

    - For event signals, emitting twice in the same instant raises an exception.
    - For aggregate signals, [v] is combined with the current accumulator using
      the function supplied at creation time; awaiters only observe the final
      accumulator value at the end of the instant. *)
val emit : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit

(** {2 Signal status } *)

(** [await s] waits for signal [s] to be emitted.

    The continuation is always resumed in the {b next} instant, even if the
    signal is already present when [await] is executed. Example:

    {[
      let s = new_signal () in
        emit s 42;
        let v = await s in
            (* v = 42, this line runs in the instant following the emission. *)
            Format.printf "received %d@." v
    ]}
    Aggregated signals behave the same, except that the value returned by
    [await] is the accumulated one:

    {[
      let s = new_signal_agg ~initial:0 ~combine:( + ) in
        emit s 1;
        emit s 2;
        emit s 3;
        let sum = await s in
            (* sum = 6, this line runs in the instant following the emission. *)
            Format.printf "sum=%d@." sum
    ]}
*)
val await : ('emit, 'agg, 'mode) signal_core -> 'agg

(** [await_immediate s] waits for [s] but resumes as soon as the signal is
    present, within the {b current} instant. This is restricted to
    event signals because aggregate signals only deliver their combined value at
    the end of the instant. Calling it on an aggregate signal is a type error.

    {[
      let s = new_signal () in
          emit s 42;
          let v = await_immediate s in
            (* v = 42, this line still runs in the same instant. *)
            Format.printf "saw %d@." v
    ]}
*)
val await_immediate : 'a signal -> 'a

(** [is_present s] returns whether [s] is present in the current instant. *)
val is_present : ('emit, 'agg, 'mode) signal_core -> bool

(** [peek s] returns [Some v] when event signal [s] is present in the current
    instant with value [v], and [None] otherwise. *)
val peek : 'a signal -> 'a option

(** {2 Suspension } *)

(** [pause ()] suspends the current task until the next instant.

    The current task yields control and is resumed at the beginning
    of the next instant, unless it is aborted.

    {[
      let rec loop () =
        Format.printf "Looping@.";
        pause ();
        loop ()
    ]}
*)
val pause : unit -> unit

(** [when_ g body] executes [body] under a presence guard.

    The body is executed only when signal [g] is present in the current
    instant. If [g] is absent, the task is blocked intra-instant and may
    be resumed later in the same instant if [g] is emitted.

    Nested calls to [when_] correspond to a conjunction of guards.*)
val when_ : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit

(** {2 Cancellation } *)

(** [watch s body] runs [body] while [s] stays absent.

    If [s] becomes present during an instant and [body] has not yet finished,
    the runtime interrupts [body] before the next instant so it never resumes. *)

val watch : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit

(** {2 Concurrency}

    {!val:parallel} launches several behaviors concurrently within the same
    synchronous instant semantics. The call returns only when every branch has
    completed. If [parallel] itself is guarded with {!val:when_}, the entire
    composition is suspended whenever the guard is absent; none of the branches
    progress until the guard holds again. Likewise, wrapping [parallel] in
    {!val:watch} causes every branch to be stopped together as soon as the
    watched signal fires. *)

(** [parallel procs] runs each process concurrently (using {!val:Low_level.fork})
    and waits for all of them to finish (using {!val:Low_level.join}). *)
val parallel : (unit -> unit) list -> unit

(* val (||) : (unit -> unit) -> (unit -> unit) -> unit *)
(** {1 Execution} *)

(** [execute ?instants ?input ?output main] starts the synchronous execution of a
    top-level process. The callback [main input_signal output_signal] receives:

    - [input_signal] : a regular event signal that the runtime marks present at
      the beginning of an instant whenever [input ()] returns [Some payload].
    - [output_signal] : a regular event signal that is flushed via [output]
      once per instant if user code emits it.

    [input] defaults to a function that never produces values, [output] defaults
    to a no-op. *)
val execute :
  ?instants:int ->
  ?input:(unit -> 'input option) ->
  ?output:('output -> unit) ->
  ('input signal -> 'output signal -> unit) ->
  unit

(** [execute_trace ~inputs main] runs [main] with a deterministic input script and
    returns every output emission in order. If [instants] is omitted, execution
    runs for [List.length inputs] logical instants. *)
val execute_trace :
  ?instants:int ->
  inputs:'input option list ->
  ('input signal -> 'output signal -> unit) ->
  'output list

type ('input, 'output) timeline_instant = {
  instant : int;
  input : 'input option;
  output : 'output option;
}

(** [execute_timeline ~inputs main] runs [main] with scripted inputs and returns
    a per-instant trace containing consumed input and emitted output. *)
val execute_timeline :
  ?instants:int ->
  inputs:'input option list ->
  ('input signal -> 'output signal -> unit) ->
  ('input, 'output) timeline_instant list

(** {1 Low level operators }

    These primitives expose the raw scheduling machinery (kills, threads, manual
    joins, etc.). They are intentionally low level and can break causal ordering
    if misused. Their purpose is to build higher-level combinators such as
    {!val:watch}, {!val:present_then_else}, or {!val:parallel}. *)

(** {1 Derived operators } *)

(** [present_then s then_branch else_branch] implements the classic Boussinot
    [present] statement. If [s] is present in the current instant,
    [then_branch] runs immediately within the same instant; otherwise,
    [else_branch] is scheduled in the next instant. *)
val present_then_else :
  ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> (unit -> unit) -> unit

(** {1 Core API}

    [Core] is the minimal synchronous kernel. It contains only execution,
    signals, waiting, logical suspension, control guards, cancellation and
    synchronous parallel composition. *)
module Core : sig
  type nonrec event = event
  type nonrec aggregate = aggregate
  type nonrec ('emit, 'agg, 'mode) signal_core = ('emit, 'agg, 'mode) signal_core
  type nonrec 'a signal = 'a signal
  type nonrec ('emit, 'agg) agg_signal = ('emit, 'agg) agg_signal

  val new_signal : unit -> 'a signal
  val new_signal_agg :
    initial:'agg ->
    combine:('agg -> 'emit -> 'agg) ->
    ('emit, 'agg) agg_signal
  val emit : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit
  val await : ('emit, 'agg, 'mode) signal_core -> 'agg
  val await_immediate : 'a signal -> 'a
  val pause : unit -> unit
  val when_ : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
  val watch : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
  val parallel : (unit -> unit) list -> unit
  val execute :
    ?instants:int ->
    ?input:(unit -> 'input option) ->
    ?output:('output -> unit) ->
    ('input signal -> 'output signal -> unit) ->
    unit
end


(* [do_every s body] starts executing [body] immediately and restarts it
    whenever [s] is present. If [body] terminates normally, the next
    iteration begins in the following instant to avoid zero-delay loops. *)
(* val do_every : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit *)

(* [every_do s body] waits for [s] to become present, then executes [body]
    starting in the next instant. If [s] becomes present while [body] is still
    running, the current instance is interrupted and a new iteration starts. *)
(* val every_do : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit *)

(* [trap body] installs a lexical trap. The function [body] receives an
    abstract [trap] handle that can be passed around; invoking [exit handle]
    interrupts all tasks spawned within the trap and terminates the trap
    body. If the task performing the exit is the one currently executing the
    sequential body, control jumps to the end of the trap immediately. *)
(* val trap : (trap -> unit) -> unit *)

(* [exit handle] triggers the termination of the surrounding [trap]. Subsequent
    calls are ignored. *)
(* val exit : trap -> unit *)

(** {1 Low-level building blocks }

    The following module exposes the raw scheduler primitives (kills,
    fork/join). Use these to build custom control operators; otherwise prefer
    the high-level functions available at the top of this interface. *)

(** Abstract handle representing a cancellation scope (used by {!module:Low_level}). *)
type kill

(** Identifier for a forked thread (used by {!module:Low_level}). *)
type thread

module Low_level : sig
  val new_kill : unit -> kill
  val with_kill : kill -> (unit -> unit) -> unit
  val abort_kill : kill -> unit

  val fork : (unit -> unit) -> thread
  val join : thread -> unit
end

type 'a state

(** [new_state initial] creates a mutable synchronous state cell. *)
val new_state : 'a -> 'a state

(** [get_state st] reads the current value. *)
val get_state : 'a state -> 'a

(** [set_state st v] updates the current value and emits an update event. *)
val set_state : 'a state -> 'a -> unit

(** [modify_state st f] applies [f] to the current value and stores the result. *)
val modify_state : 'a state -> ('a -> 'a) -> unit

(** [await_state st] waits for the next state update and returns the new value. *)
val await_state : 'a state -> 'a

module State : sig
  type 'a t = 'a state

  (** Minimal state cell example:
      {[
        let score = State.create 0 in
        State.modify score (fun s -> s + 10);
        let now = State.get score in
        Format.printf "score=%d@." now
      ]} *)
  val create : 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
  val modify : 'a t -> ('a -> 'a) -> unit
  val await : 'a t -> 'a
  val update_and_get : 'a t -> ('a -> 'a) -> 'a
end

module Dynamic : sig
  type handle

  (** [spawn p] starts [p] as a managed process.
      Example:
      {[
        let h = Dynamic.spawn (fun () -> while true do pause () done) in
        (* ... later ... *)
        Dynamic.stop h;
        Dynamic.join h
      ]} *)
  val spawn : (unit -> unit) -> handle

  (** [stop h] requests termination of [h]. *)
  val stop : handle -> unit

  (** [join h] waits for process completion. *)
  val join : handle -> unit

  (** [spawn_many ps] starts all processes and returns their handles. *)
  val spawn_many : (unit -> unit) list -> handle list
end

module Game : sig
  (** [after_n n body] executes [body] after [n] logical instants. *)
  val after_n : int -> (unit -> unit) -> unit

  (** [every_n n body] executes [body] every [n] logical instants.
      Example:
      {[
        Game.every_n 30 (fun () -> emit heartbeat ());
        Game.after_n 120 (fun () -> emit end_round ())
      ]} *)
  val every_n : int -> (unit -> unit) -> unit

  (** [timeout n ~on_timeout body] runs [body] with a timeout of [n] instants.
      If the timeout elapses first, [body] is preempted and [on_timeout] runs. *)
  val timeout : int -> on_timeout:(unit -> unit) -> (unit -> unit) -> unit

  (** [cooldown n s handler] reacts to [s] with [handler], then ignores further
      occurrences of [s] for [n] instants. *)
  val cooldown :
    int -> ('emit, 'agg, 'mode) signal_core -> ('agg -> unit) -> unit
end

module Reactive : sig
  (** [rising_edge level input edge] emits [edge] when [level v] transitions from
      [false] to [true] between two consecutive values received on [input]. *)
  val rising_edge : ('a -> bool) -> 'a signal -> unit signal -> unit

  (** [falling_edge level input edge] emits [edge] when [level v] transitions
      from [true] to [false] between two consecutive values on [input]. *)
  val falling_edge : ('a -> bool) -> 'a signal -> unit signal -> unit

  (** [edge_by pred input edge] emits [edge] whenever [pred prev curr] holds for
      two consecutive values read from [input]. *)
  val edge_by : ('a -> 'a -> bool) -> 'a signal -> unit signal -> unit

  (** [hold_last initial s] stores the latest value seen on [s] in a state cell,
      initialized with [initial]. *)
  val hold_last : 'a -> 'a signal -> 'a state

  (** [sample_on st trigger] emits [(current_state, trigger_value)] whenever
      [trigger] occurs. *)
  val sample_on : 'a state -> 'b signal -> ('a * 'b) signal

  (** [toggle_on ?initial trigger] returns a boolean state that toggles on each
      occurrence of [trigger]. *)
  val toggle_on : ?initial:bool -> unit signal -> bool state

  (** [pulse_n n] creates an event signal that emits [()] every [n] logical
      instants.
      Example:
      {[
        let tick = Reactive.pulse_n 10 in
        while true do
          let () = await tick in
          emit spawn_enemy ()
        done
      ]} *)
  val pulse_n : int -> unit signal

  (** [supervise_until stop procs] runs [procs] in synchronous parallel and
      aborts all of them when [stop] becomes present. *)
  val supervise_until :
    ('emit, 'agg, 'mode) signal_core -> (unit -> unit) list -> unit
end

module App : sig
  type 'msg dispatch = 'msg -> unit
  type 'msg command = dispatch:'msg dispatch -> unit

  (** Example command pipeline:
      {[
        type msg = Boot | Tick | Click

        let update model = function
          | Boot -> (model, App.tick_every 30 ~tick:Tick)
          | Tick -> ({ model with frame = model.frame + 1 }, App.none)
          | Click -> (model, App.emit Tick)
      ]} *)
  val none : 'msg command
  val emit : 'msg -> 'msg command
  val after_n : int -> 'msg -> 'msg command
  val every_n : int -> 'msg -> 'msg command
  val tick_every : int -> tick:'msg -> 'msg command
  val tick_if : bool -> int -> tick:'msg -> 'msg command
  val command_if : bool -> 'msg command -> 'msg command
  val command_when : bool -> then_:'msg command -> else_:'msg command -> 'msg command
  val batch : 'msg command list -> 'msg command

  (** [boot_once_input ~boot input] returns an input function that emits [boot]
      exactly once on the first poll, then delegates to [input]. *)
  val boot_once_input : boot:'msg -> (unit -> 'msg option) -> unit -> 'msg option

  (** [input_union inputs] polls input producers in order and returns the first
      available message. *)
  val input_union : (unit -> 'msg option) list -> unit -> 'msg option

  (** [with_boot_and_tick ~boot ~tick ~tick_every ~input] returns:
      - an input function that injects [boot] exactly once,
      - the command to schedule periodic [tick] messages after boot. *)
  val with_boot_and_tick :
    boot:'msg ->
    tick:'msg ->
    tick_every:int ->
    input:(unit -> 'msg option) ->
    (unit -> 'msg option) * 'msg command

  type ('model, 'msg) program = {
    init : 'model;
    update : 'model -> 'msg -> 'model * 'msg command;
  }

  val run :
    ?instants:int ->
    ?input:(unit -> 'msg option) ->
    ?on_model:('model -> unit) ->
    ('model, 'msg) program ->
    unit

  val run_with_view :
    ?instants:int ->
    ?input:(unit -> 'msg option) ->
    ?equal_view:('view -> 'view -> bool) ->
    view:('model -> 'view) ->
    ?output:('view -> unit) ->
    ('model, 'msg) program ->
    unit
end

module Loop : sig
  type ('input, 'output, 'state) config = {
    init : 'state;
    input : unit -> 'input option;
    step : 'state -> 'input option -> 'state * 'output option;
    output : 'output -> unit;
  }

  (** Canonical fixed logical loop over Tempo instants.
      Example:
      {[
        let cfg =
          {
            Loop.init = 0;
            input = (fun () -> Some 1);
            step = (fun st inp ->
              let st' = st + Option.value inp ~default:0 in
              (st', Some st'));
            output = (fun n -> Format.printf "%d@." n);
          }
        in
        Loop.run ~instants:5 cfg
      ]} *)
  val run : ?instants:int -> ('input, 'output, 'state) config -> unit
end

module Scene : sig
  type 'id t

  val create :
    ?equal:('id -> 'id -> bool) ->
    on_enter:('id -> unit) ->
    on_exit:('id -> unit) ->
    unit ->
    'id t
  val request : 'id t -> 'id -> unit
  val current : 'id t -> 'id option
  val process : 'id t -> unit
end

module Resource : sig
  type t

  val create : acquire:(unit -> unit) -> release:(unit -> unit) -> t
  val acquire : t -> unit
  val release : t -> unit
  val with_resource : t -> (unit -> 'a) -> 'a

  type manager

  val create_manager : unit -> manager
  val register : manager -> t -> unit
  val release_all : manager -> unit
end

module Input_map : sig
  type ('raw, 'action) t

  val create : ?default:'action -> ('raw * 'action) list -> ('raw, 'action) t
  val bind : ('raw, 'action) t -> 'raw -> 'action -> unit
  val resolve : ('raw, 'action) t -> 'raw -> 'action option
end

module Event_bus : sig
  type 'a channel = ('a, 'a list) agg_signal

  (** Batch event collection per logical instant.
      {[
        let bus = Event_bus.channel () in
        parallel
          [
            (fun () -> emit frame_done ());
            (fun () -> Event_bus.publish bus `Move_left);
            (fun () -> Event_bus.publish bus `Jump);
          ];
        let intents = Event_bus.await_batch bus in
        (* intents = [`Move_left; `Jump] at next instant *)
        ignore intents
      ]} *)
  val channel : unit -> 'a channel
  val publish : 'a channel -> 'a -> unit
  val await_batch : 'a channel -> 'a list
end

module Fixed_step : sig
  type accumulator = { leftover : float }

  val empty : accumulator
  val step : fixed_dt:float -> delta:float -> accumulator -> int * accumulator
  val alpha : fixed_dt:float -> accumulator -> float
end

module Rng : sig
  type t

  val create : int -> t
  val int : t -> int -> int
  val float : t -> float -> float
  val bool : t -> bool
  val split : t -> t
end

module Netcode : sig
  type 's snapshot = {
    frame : int;
    state : 's;
  }

  val snapshot : frame:int -> 's -> 's snapshot
  val rollback : 's state -> 's snapshot -> unit
end

module Profiler : sig
  type t
  type sample = {
    name : string;
    duration_ns : int64;
  }

  val create : unit -> t
  val clear : t -> unit
  val measure : t -> name:string -> (unit -> 'a) -> 'a
  val snapshot : t -> sample list
end

type inspector_snapshot = {
  instant : int;
  current_tasks : int;
  blocked_tasks : int;
  next_tasks : int;
  signal_count : int;
}

val execute_inspect :
  ?instants:int ->
  ?input:(unit -> 'input option) ->
  ?output:('output -> unit) ->
  on_instant:(inspector_snapshot -> unit) ->
  ('input signal -> 'output signal -> unit) ->
  unit

module Entity_set : sig
  type ('id, 'entity) t

  val create : unit -> ('id, 'entity) t
  val spawn :
    ('id, 'entity) t ->
    id:'id ->
    entity:'entity ->
    process:('entity -> unit) ->
    unit
  val despawn : ('id, 'entity) t -> 'id -> unit
  val broadcast : ('id, 'entity) t -> ('entity -> unit) -> unit
  val ids : ('id, 'entity) t -> 'id list
  val despawn_all : ('id, 'entity) t -> unit
end

module Timeline_json : sig
  type 'a serializer = 'a -> string

  (** Serialize deterministic execution traces for tests and release reports.
      {[
        let json =
          Timeline_json.of_timeline_with
            ~input_to_string:string_of_int
            ~output_to_string:string_of_int
            timeline
      ]} *)
  val of_timeline_with :
    input_to_string:'input serializer ->
    output_to_string:'output serializer ->
    ('input, 'output) timeline_instant list ->
    string

  val of_timeline :
    ('input -> string) ->
    ('output -> string) ->
    ('input, 'output) timeline_instant list ->
    string
end

module Tick_tags : sig
  type t

  val create : unit -> t
  val mark : t -> string -> unit
  val all : t -> string list
  val clear : t -> unit
end

module Runtime_snapshot : sig
  type t

  val capture : ?tags:Tick_tags.t -> frame:int -> unit -> t
  val restore : t -> unit
  val frame : t -> int
  val tags : t -> string list
end

module Error_bus : sig
  type error = {
    instant : int option;
    exn : exn;
    backtrace : string;
  }

  val signal : unit -> error signal
  val safe : error signal -> (unit -> unit) -> unit
  val execute_safe :
    ?instants:int ->
    ?input:(unit -> 'input option) ->
    ?output:('output -> unit) ->
    errors:error signal ->
    ('input signal -> 'output signal -> unit) ->
    unit
end

val version_string : string
val api_level : int
val require_api_level : int -> unit

module Dev_hud : sig
  val to_lines : inspector_snapshot -> string list
  val to_string : inspector_snapshot -> string
end

(** {1 Layer 2 API}

    [Layer2] groups all higher-level modules built on top of [Core]. *)
module Layer2 : sig
  module Dynamic = Dynamic
  module Game = Game
  module Reactive = Reactive
  module App = App
  module Loop = Loop
  module Scene = Scene
  module Resource = Resource
  module Input_map = Input_map
  module Event_bus = Event_bus
  module Fixed_step = Fixed_step
  module Rng = Rng
  module Netcode = Netcode
  module Profiler = Profiler
  module Entity_set = Entity_set
  module State = State
  module Timeline_json = Timeline_json
  module Tick_tags = Tick_tags
  module Runtime_snapshot = Runtime_snapshot
  module Error_bus = Error_bus
  module Dev_hud = Dev_hud
end
