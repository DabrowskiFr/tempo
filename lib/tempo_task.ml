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

let kill_epoch = ref 0
let guard_epoch = ref 0

let bump_kill_epoch () =
  kill_epoch := !kill_epoch + 1

let bump_guard_epoch () =
  guard_epoch := !guard_epoch + 1

let guards_ok_list guards =
  List.for_all (fun (Any s) -> s.present) guards

let kills_alive kills =
  List.for_all (fun k -> !(k.alive)) kills

let task_guards_ok (t : task) =
  if t.guards = [] then true
  else if t.guards_checked_epoch = !guard_epoch then t.guards_ok_cached
  else
    let ok = guards_ok_list t.guards in
    t.guards_checked_epoch <- !guard_epoch;
    t.guards_ok_cached <- ok;
    ok

let task_kills_alive (t : task) =
  if t.kills = [] then true
  else if t.kills_checked_epoch = !kill_epoch then t.kills_alive_cached
  else
    let alive = kills_alive t.kills in
    t.kills_checked_epoch <- !kill_epoch;
    t.kills_alive_cached <- alive;
    alive

let enqueue_now st t =
  if not t.queued then (
    t.queued <- true;
    t.blocked <- false;
    Queue.add t st.current)

let enqueue_next st t =
  st.next_instant <- t :: st.next_instant

let infer_guard_cache parent guards =
  let epoch = !guard_epoch in
  let default () =
    if guards = [] then (epoch, true) else (-1, false)
  in
  match parent with
  | None -> default ()
  | Some p ->
      if guards == p.guards then
        if p.guards = [] then (epoch, true)
        else if p.guards_checked_epoch = epoch then (epoch, p.guards_ok_cached)
        else default ()
      else
        match guards with
        | Any s :: gs when gs == p.guards ->
            if p.guards = [] then (epoch, s.present)
            else if p.guards_checked_epoch = epoch && p.guards_ok_cached then
              (epoch, s.present)
            else default ()
        | _ -> default ()

let infer_kill_cache parent kills =
  let epoch = !kill_epoch in
  let default () =
    if kills = [] then (epoch, true) else (-1, true)
  in
  match parent with
  | None -> default ()
  | Some p ->
      if kills == p.kills then
        if p.kills = [] then (epoch, true)
        else if p.kills_checked_epoch = epoch then (epoch, p.kills_alive_cached)
        else default ()
      else
        match kills with
        | k :: ks when ks == p.kills ->
            if p.kills = [] then (epoch, !(k.alive))
            else if p.kills_checked_epoch = epoch then
              (epoch, p.kills_alive_cached && !(k.alive))
            else default ()
        | _ -> default ()

let create_task ?parent st thread guards kills run =
  let state = Tempo_thread.ensure st.threads thread in
  if state.completed && state.active = 0 then state.completed <- false;
  state.active <- state.active + 1;
  let t_id = st.debug.task_counter in
  st.debug.task_counter <- st.debug.task_counter + 1;
  let guards_checked_epoch, guards_ok_cached = infer_guard_cache parent guards in
  let kills_checked_epoch, kills_alive_cached = infer_kill_cache parent kills in
  {
    t_id
  ; thread
  ; guards
  ; kills
  ; guards_checked_epoch
  ; guards_ok_cached
  ; kills_checked_epoch
  ; kills_alive_cached
  ; run
  ; queued = false
  ; blocked = false
  }

let spawn_now ?parent st thread guards kills run =
  let t = create_task ?parent st thread guards kills run in
  enqueue_now st t;
  t

let spawn_next ?parent st thread guards kills run =
  let t = create_task ?parent st thread guards kills run in
  enqueue_next st t;
  t

let block_on_guards (st : scheduler_state) (t : task) =
  if not t.blocked then (
    t.blocked <- true;
    st.blocked <- t :: st.blocked);
  t.guards_checked_epoch <- !guard_epoch;
  t.guards_ok_cached <- false;
  let missing_guards guards =
    List.filter (fun (Any s) -> not s.present) guards
  in
  let miss = missing_guards t.guards in
  List.iter
    (fun (Tempo_types.Any s) -> s.guard_waiters <- t :: s.guard_waiters)
    miss

let block_on_guards_with_missing (st : scheduler_state) (t : task) miss =
  if not t.blocked then (
    t.blocked <- true;
    st.blocked <- t :: st.blocked);
  t.guards_checked_epoch <- !guard_epoch;
  t.guards_ok_cached <- false;
  List.iter
    (fun (Tempo_types.Any s) -> s.guard_waiters <- t :: s.guard_waiters)
    miss

let wake_guard_waiters st s =
  List.iter
    (fun t -> if task_guards_ok t && task_kills_alive t then enqueue_now st t)
    s.guard_waiters;
  s.guard_waiters <- []
