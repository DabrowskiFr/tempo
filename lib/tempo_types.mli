(*---------------------------------------------------------------------------
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
 *---------------------------------------------------------------------------*)

type kill = { alive : bool ref; mutable cleanup : (unit -> unit) option }

type thread = int

type event
type aggregate

type ('emit, 'agg, 'mode) signal_core =
  { s_id : int
  ; mutable present : bool
  ; mutable value : 'agg option
  ; mutable awaiters : ('agg -> unit) list
  ; mutable guard_waiters : task list
  ; kind : ('emit, 'agg, 'mode) signal_kind
  }
and ('emit, 'agg, 'mode) signal_kind =
  | Event_signal : ('a, 'a, event) signal_kind
  | Aggregate_signal :
      { combine : 'agg -> 'emit -> 'agg
      ; initial : 'agg
      } -> ('emit, 'agg, aggregate) signal_kind
and task =
  {
    t_id            : int;
    guards          : any_signal list;
    kills           : kill list;
  thread         : thread;
  run             : unit -> unit;
  mutable queued  : bool;
  mutable blocked : bool;
  }
and any_signal = Any : ('emit, 'agg, 'mode) signal_core -> any_signal

type 'a signal = ('a, 'a, event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core

type _ Effect.t +=
  | New_signal  : unit -> ('a, 'a, event) signal_core Effect.t
  | New_signal_agg :
      'agg * ('agg -> 'emit -> 'agg) -> ('emit, 'agg, aggregate) signal_core Effect.t
  | Emit        : ('emit, 'agg, 'mode) signal_core * 'emit -> unit Effect.t
  | Await       : ('emit, 'agg, 'mode) signal_core -> 'agg Effect.t
  | Await_immediate : ('a, 'a, event) signal_core -> 'a Effect.t
  | Pause       : unit Effect.t
  | Fork        : (unit -> unit) -> thread Effect.t
  | Join        : thread -> unit Effect.t
  | With_guard  : ('emit, 'agg, 'mode) signal_core * (unit -> unit) -> unit Effect.t
  | With_kill   : kill * (unit -> unit) -> unit Effect.t

exception Aborted
