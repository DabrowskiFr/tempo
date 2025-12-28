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

let enqueue_now st t =
  if (not t.queued && kills_alive t.kills) then begin
    t.queued <- true;
    t.blocked <- false;
    Queue.add t st.current
  end

let enqueue_next st t =
  if (kills_alive t.kills) then
  st.next_instant <- t :: st.next_instant

let create_task st thread guards kills run =
  let state = Tempo_thread.ensure st.threads thread in
  if state.completed && state.active = 0 then state.completed <- false;
  state.active <- state.active + 1;
  let t_id = st.debug.task_counter in
  st.debug.task_counter <- st.debug.task_counter + 1;
  { t_id; thread; guards; kills; run; queued = false; blocked = false }

let spawn_now st thread guards kills run =
  let t = create_task st thread guards kills run in
  enqueue_now st t; t

let spawn_next st thread guards kills run =
  let t = create_task st thread guards kills run in
  enqueue_next st t; t

let block_on_guards st t =
  assert (kills_alive t.kills);
  if not t.blocked then begin
    t.blocked <- true;
    st.blocked <- t :: st.blocked
  end;
  let missing_guards guards =
    List.filter (fun (Any s) -> not s.present) guards
  in
  let miss = missing_guards t.guards in
  List.iter (fun (Tempo_types.Any s) -> s.guard_waiters <- t :: s.guard_waiters) miss

let wake_guard_waiters st s =
  let guard_ok guards = List.for_all (fun (Any g) -> g.present) guards in
  List.iter
    (fun t ->
      if guard_ok t.guards && kills_alive t.kills then enqueue_now st t)
    s.guard_waiters;
  s.guard_waiters <- []
