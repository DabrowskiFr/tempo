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

open Tempo_types

val kills_alive : kill list -> bool

val enqueue_now : scheduler_state -> task -> unit

val enqueue_next : scheduler_state -> task -> unit

val block_on_guards : scheduler_state -> task -> unit

val spawn_now :
  scheduler_state -> thread -> any_signal list -> kill list -> (unit -> unit) -> task

val spawn_next :
  scheduler_state -> thread -> any_signal list -> kill list -> (unit -> unit) -> task

val wake_guard_waiters :
  scheduler_state -> ('e, 'a, 'm) signal_core -> unit
