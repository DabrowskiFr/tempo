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

type kill_context =
  | KEmpty
  | KNode of {
      kill : kill
    ; parent : kill_context
    ; mutable checked_epoch : int
    ; mutable alive_cached : bool
  }
type thread = int
type event
type aggregate

type 'agg awaiter = { resume : 'agg -> unit; kill_ctx : kill_context }
type join_waiter = { resume : unit -> unit; kill_ctx : kill_context }
type kill_watcher = { kill : kill; kill_ctx : kill_context }

type ('emit, 'agg, 'mode) signal_core = {
    s_id : int
  ; mutable tracked : bool
  ; mutable present : bool
  ; mutable value : 'agg option
  ; mutable awaiters : 'agg awaiter list
  ; mutable awaiters_kill_epoch : int
  ; mutable guard_waiters : task list
  ; mutable kill_watchers : kill_watcher list
  ; mutable kill_watchers_kill_epoch : int
  ; kind : ('emit, 'agg, 'mode) signal_kind
}

and ('emit, 'agg, 'mode) signal_kind =
  | Event_signal : ('a, 'a, event) signal_kind
  | Aggregate_signal : {
        combine : 'agg -> 'emit -> 'agg
      ; initial : 'agg
    }
      -> ('emit, 'agg, aggregate) signal_kind

and registered_missing_state =
  | Missing_none
  | Missing_one of int
  | Missing_many of (int, unit) Hashtbl.t

and task_guard_meta = {
    mutable guards : any_signal list
  ; mutable pending_guards : int
  ; mutable registered_missing : registered_missing_state
  ; mutable guard_registration_instant : int
  ; mutable guards_checked_epoch : int
  ; mutable guards_ok_cached : bool
}

and task = {
    mutable t_id : int
  ; mutable guard_meta : task_guard_meta option
  ; mutable kill_ctx : kill_context
  ; mutable thread : thread
  ; mutable run : unit -> unit
  ; mutable queued : bool
  ; mutable blocked : bool
}

and any_signal = Any : ('emit, 'agg, 'mode) signal_core -> any_signal

type 'a signal = ('a, 'a, event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core

type thread_state = {
    mutable active : int
  ; mutable completed : bool
  ; mutable waiters : join_waiter list
}

type thread_table = {
    mutable states : thread_state option array
  ; mutable waiters_pruned_kill_epoch : int
}

type debug_info = {
    mutable sig_counter : int
  ; mutable task_counter : int
  ; mutable step_counter : int
  ; mutable instant_counter : int
}

type scheduler_state = {
    current : task Queue.t
  ; mutable next_instant : task list
  ; mutable blocked : task list
  ; mutable free_tasks : task list
  ; mutable free_task_count : int
  ; mutable signals : any_signal list
  ; mutable thread_counter : int
  ; threads : thread_table
  ; debug : debug_info
}

type _ Effect.t +=
  | New_signal : unit -> ('a, 'a, event) signal_core Effect.t
  | New_signal_agg :
      'agg * ('agg -> 'emit -> 'agg)
      -> ('emit, 'agg, aggregate) signal_core Effect.t
  | Emit : ('emit, 'agg, 'mode) signal_core * 'emit -> unit Effect.t
  | Await : ('emit, 'agg, 'mode) signal_core -> 'agg Effect.t
  | Await_immediate : ('a, 'a, event) signal_core -> 'a Effect.t
  | Pause : unit Effect.t
  | Fork : (unit -> unit) -> thread Effect.t
  | Join : thread -> unit Effect.t
  | Register_kill_watcher :
      ('emit, 'agg, 'mode) signal_core * kill -> unit Effect.t
  | With_guard :
      ('emit, 'agg, 'mode) signal_core * (unit -> unit)
      -> unit Effect.t
  | With_kill : kill * (unit -> unit) -> unit Effect.t

exception Aborted
