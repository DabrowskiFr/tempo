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

type thread_state = Tempo_types.thread_state

val ensure :
     (Tempo_types.thread, thread_state) Hashtbl.t
  -> Tempo_types.thread
  -> thread_state

val find :
     (Tempo_types.thread, thread_state) Hashtbl.t
  -> Tempo_types.thread
  -> thread_state

val add_join_waiter :
     (Tempo_types.thread, thread_state) Hashtbl.t
  -> Tempo_types.thread
  -> (unit -> unit)
  -> unit

val new_thread_id : Tempo_types.scheduler_state -> Tempo_types.thread

val finish_task :
  (Tempo_types.thread, thread_state) Hashtbl.t -> Tempo_types.thread -> unit
