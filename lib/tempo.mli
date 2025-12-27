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
    continuation-passing and compilation techniques used in the original
    ReactiveML implementation.

    Programs are executed according to a synchronous semantics: computation
    progresses in a sequence of logical instants, and all observable effects
    within an instant are considered to occur simultaneously

    For a given set of signal emissions in each instant, the observable behavior
    of purely functional program is deterministic. Internal scheduling and
    execution order are not observable and do not affect the final synchronous
    outcome. *)

(** {1 Signals}
    Signals are the primary communication mechanism between tasks. They come in
    two flavours:

    - {b Event signals} guarantee at most one [emit] per instant. Presence is a
      boolean flag and the value is delivered during the instant. Multiple
      emissions raise an exception.
    - {b Aggregate signals} may be emitted multiple times within the same
      instant. Values are combined using a user-provided accumulator function,
      and the final accumulated value is visible at the next instant.

    In both cases, presence information is scoped to the current instant. When a
    task waits for a signal, the scheduler suspends it across instants until the
    signal becomes present.*)

type event
(** Signal kinds are tracked with phantom markers so that the type system can
    distinguish single-emission (event) signals from aggregate signals while
    still sharing the same primitives. *)

type aggregate

type ('emit, 'agg, 'mode) signal_core
(** Generalized signal type. ['emit] is the type of values passed to [emit],
    ['agg] is the value observed by [await] (equal to ['emit] for events, but
    possibly different for aggregates), and ['mode] encodes the signal flavour
    using the phantom markers below. *)

type 'a signal = ('a, 'a, event) signal_core
(** A value of type ['a signal] represents a single-emission signal (at most one
    [emit] per instant) carrying values of type ['a]. Attempts to emit twice in
    the same instant raise [Invalid_argument]. *)

type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core
(** Aggregate signals can be emitted several times per instant; their values are
    combined using the user-provided accumulator before the aggregated value is
    made visible for the next instant. *)

(** {1 Signal creation} *)

val new_signal : unit -> 'a signal
(** [new_signal ()] creates a new event signal.

    The signal starts absent in the current instant and can be emitted at most
    once per instant. Event signals are compatible with every primitive that
    expects a signal argument; they are also the only signals that support
    {!val:await_immediate}. *)

val new_signal_agg :
  initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal
(** [new_signal_agg ~initial ~combine] creates an aggregate signal. When the
    signal is emitted several times within the same instant, each value is
    folded into the accumulator using [combine]. The accumulator starts at
    [initial] for the first emission of the instant. Aggregate signals can be
    used with the same primitives as event signals; the sole restriction is that
    {!val:await_immediate} is unavailable because their combined value is only
    produced at the end of the instant. *)

(** {1 Synchronous operators} *)

(** {2 Emission} *)

val emit : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit
(** [emit s v] marks [s] as present in the current instant and propagates the
    value [v].

    - For event signals, emitting twice in the same instant raises an exception.
    - For aggregate signals, [v] is combined with the current accumulator using
      the function supplied at creation time; awaiters only observe the final
      accumulator value at the end of the instant. *)

(** {2 Signal status} *)

val await : ('emit, 'agg, 'mode) signal_core -> 'agg
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
    ]} *)

val await_immediate : 'a signal -> 'a
(** [await_immediate s] waits for [s] but resumes as soon as the signal is
    present, within the {b current} instant. This is restricted to event signals
    because aggregate signals only deliver their combined value at the end of
    the instant. Calling it on an aggregate signal is a type error.

    {[
      let s = new_signal () in
      emit s 42;
      let v = await_immediate s in
      (* v = 42, this line still runs in the same instant. *)
      Format.printf "saw %d@." v
    ]} *)

(** {2 Suspension} *)

val pause : unit -> unit
(** [pause ()] suspends the current task until the next instant.

    The current task yields control and is resumed at the beginning of the next
    instant, unless it is aborted.

    {[
      let rec loop () =
        Format.printf "Looping@.";
        pause ();
        loop ()
    ]} *)

val when_ : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
(** [when_ g body] executes [body] under a presence guard.

    The body is executed only when signal [g] is present in the current instant.
    If [g] is absent, the task is blocked intra-instant and may be resumed later
    in the same instant if [g] is emitted.

    Nested calls to [when_] correspond to a conjunction of guards.*)

(** {2 Cancellation} *)

(** [watch s body] runs [body] while [s] stays absent.

    If [s] becomes present during an instant and [body] has not yet finished,
    the runtime interrupts [body] before the next instant so it never resumes.
*)

val watch : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit

(** {2 Concurrency}

    {!val:parallel} launches several behaviors concurrently within the same
    synchronous instant semantics. The call returns only when every branch has
    completed. If [parallel] itself is guarded with {!val:when_}, the entire
    composition is suspended whenever the guard is absent; none of the branches
    progress until the guard holds again. Likewise, wrapping [parallel] in
    {!val:watch} causes every branch to be stopped together as soon as the
    watched signal fires. *)

val parallel : (unit -> unit) list -> unit
(** [parallel procs] runs each process concurrently (using
    {!val:Low_level.fork}) and waits for all of them to finish (using
    {!val:Low_level.join}). *)

(* val (||) : (unit -> unit) -> (unit -> unit) -> unit *)
(** {1 Execution} *)

val execute :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ?output:('output -> unit)
  -> ('input signal -> 'output signal -> unit)
  -> unit
(** [execute ?instants ?input ?output main] starts the synchronous execution of
    a top-level process. The callback [main input_signal output_signal]
    receives:

    - [input_signal] : a regular event signal that the runtime marks present at
      the beginning of an instant whenever [input ()] returns [Some payload].
    - [output_signal] : a regular event signal that is flushed via [output] once
      per instant if user code emits it.

    [input] defaults to a function that never produces values, [output] defaults
    to a no-op. *)

(** {1 Low level operators}

    These primitives expose the raw scheduling machinery (kills, threads, manual
    joins, etc.). They are intentionally low level and can break causal ordering
    if misused. Their purpose is to build higher-level combinators such as
    {!val:watch}, {!val:present_then_else}, or {!val:parallel}. *)

(** {1 Derived operators} *)

val present_then_else :
  ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> (unit -> unit) -> unit
(** [present_then s then_branch else_branch] implements the classic Boussinot
    [present] statement. If [s] is present in the current instant, [then_branch]
    runs immediately within the same instant; otherwise, [else_branch] is
    scheduled in the next instant. *)

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

(** {1 Low-level building blocks}

    The following module exposes the raw scheduler primitives (kills,
    fork/join). Use these to build custom control operators; otherwise prefer
    the high-level functions available at the top of this interface. *)

type kill
(** Abstract handle representing a cancellation scope (used by
    {!module:Low_level}). *)

type thread
(** Identifier for a forked thread (used by {!module:Low_level}). *)

module Low_level : sig
  val new_kill : unit -> kill
  val with_kill : kill -> (unit -> unit) -> unit
  val abort_kill : kill -> unit
  val fork : (unit -> unit) -> thread
  val join : thread -> unit
end
