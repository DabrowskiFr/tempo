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

let ensure (threads : (Tempo_types.thread, thread_state) Hashtbl.t)
    (thread : Tempo_types.thread) =
  match Hashtbl.find_opt threads thread with
  | Some ts -> ts
  | None ->
      let ts = Tempo_types.{ active = 0; completed = false; waiters = [] } in
      Hashtbl.add threads thread ts;
      ts

let find (threads : (Tempo_types.thread, thread_state) Hashtbl.t)
    (thread : Tempo_types.thread) =
  match Hashtbl.find_opt threads thread with
  | Some ts -> ts
  | None -> invalid_arg (Printf.sprintf "unknown thread %d" thread)

let add_join_waiter threads (thread : Tempo_types.thread) waiter =
  let state = find threads thread in
  if state.completed then waiter ()
  else state.waiters <- waiter :: state.waiters

let new_thread_id (st : Tempo_types.scheduler_state) =
  let id = st.thread_counter in
  st.thread_counter <- id + 1;
  let _ = ensure st.threads id in
  id

let finish_task threads (thread : Tempo_types.thread) =
  let rec resume_waiters waiters =
    match waiters with
    | [] -> ()
    | f :: rest ->
        f ();
        resume_waiters rest
  in
  let state = find threads thread in
  state.active <- state.active - 1;
  if state.active = 0 then (
    state.completed <- true;
    let waiters = List.rev state.waiters in
    state.waiters <- [];
    resume_waiters waiters)
