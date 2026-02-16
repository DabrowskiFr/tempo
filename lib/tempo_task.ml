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

let kills_alive kills = List.for_all (fun k -> !(k.alive)) kills
let task_kills_alive t = if t.has_kills then kills_alive t.kills else true

let guard_ok guards = List.for_all (fun (Any g) -> g.present) guards
let task_guard_ok t =
  match t.guard_single with
  | Some (Any g) -> g.present
  | None -> guard_ok t.guards

let enqueue_now st t =
  if (not t.queued) && task_kills_alive t then (
    t.queued <- true;
    t.blocked <- false;
    Queue.add t st.current)

let enqueue_next st t =
  if task_kills_alive t then st.next_instant <- t :: st.next_instant

let create_task st thread guards kills run =
  let state = Tempo_thread.ensure st.threads thread in
  if state.completed && state.active = 0 then state.completed <- false;
  state.active <- state.active + 1;
  let t_id = st.debug.task_counter in
  st.debug.task_counter <- st.debug.task_counter + 1;
  let guard_single =
    match guards with
    | [ g ] -> Some g
    | _ -> None
  in
  { t_id
  ; thread
  ; thread_state = state
  ; guards
  ; guard_single
  ; kills
  ; has_kills = (kills <> [])
  ; run
  ; queued = false
  ; blocked = false
  }

let spawn_now st thread guards kills run =
  let t = create_task st thread guards kills run in
  enqueue_now st t;
  t

let spawn_next st thread guards kills run =
  let t = create_task st thread guards kills run in
  enqueue_next st t;
  t

let block_on_guards st t =
  assert (task_kills_alive t);
  if not t.blocked then (
    t.blocked <- true;
    st.blocked <- t :: st.blocked);
  let missing_guards guards =
    List.filter (fun (Any s) -> not s.present) guards
  in
  let miss = missing_guards t.guards in
  List.iter
    (fun (Tempo_types.Any s) -> Stack.push t s.guard_waiters)
    miss

let wake_guard_waiters st s =
  while not (Stack.is_empty s.guard_waiters) do
    let t = Stack.pop s.guard_waiters in
    if task_guard_ok t && task_kills_alive t then enqueue_now st t
  done
