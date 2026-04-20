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

let current_kill_epoch () = !kill_epoch

let bump_guard_epoch () =
  guard_epoch := !guard_epoch + 1

let guards_ok_list guards =
  List.for_all (fun (Any s) -> s.present) guards

let kills_alive kills =
  List.for_all (fun k -> !(k.alive)) kills

let empty_kill_context = KEmpty

let push_kill_context (k : kill) (parent : kill_context) =
  KNode { kill = k; parent; checked_epoch = -1; alive_cached = true }

let rec kill_context_alive (ctx : kill_context) =
  match ctx with
  | KEmpty -> true
  | KNode node ->
      if node.checked_epoch = !kill_epoch then node.alive_cached
      else
        let alive = !(node.kill.alive) && kill_context_alive node.parent in
        node.checked_epoch <- !kill_epoch;
        node.alive_cached <- alive;
        alive

let rec kill_context_depth (ctx : kill_context) =
  match ctx with
  | KEmpty -> 0
  | KNode node -> 1 + kill_context_depth node.parent

let kill_effectively_alive (k : kill) = !(k.alive)

let task_guards (t : task) =
  match t.guard_meta with
  | None -> []
  | Some gm -> gm.guards

let task_kill_ctx (t : task) = t.kill_ctx

let task_has_guards (t : task) =
  match t.guard_meta with
  | None -> false
  | Some _ -> true

let task_guards_count (t : task) =
  match t.guard_meta with
  | None -> 0
  | Some gm -> List.length gm.guards

let task_kills_count (t : task) =
  kill_context_depth t.kill_ctx

let task_guards_ok (t : task) =
  match t.guard_meta with
  | None -> true
  | Some gm ->
      if gm.guards_checked_epoch = !guard_epoch then gm.guards_ok_cached
      else
        let ok = guards_ok_list gm.guards in
        gm.guards_checked_epoch <- !guard_epoch;
        gm.guards_ok_cached <- ok;
        ok

let task_kills_alive (t : task) =
  kill_context_alive t.kill_ctx

let enqueue_now st t =
  if not t.queued then (
    t.queued <- true;
    t.blocked <- false;
    Queue.add t st.current)

let enqueue_next st t =
  st.next_instant <- t :: st.next_instant

let ensure_signal_tracked : type e a m.
    scheduler_state -> (e, a, m) signal_core -> unit =
 fun st s ->
  if not s.tracked then begin
    s.tracked <- true;
    st.signals <- Any s :: st.signals
  end

let guard_meta_exn (t : task) =
  match t.guard_meta with
  | Some gm -> gm
  | None -> invalid_arg "task guard metadata missing"

let clear_registered_missing (t : task) =
  match t.guard_meta with
  | None -> ()
  | Some gm ->
      (match gm.registered_missing with
      | Missing_many tbl -> Hashtbl.reset tbl
      | Missing_none | Missing_one _ -> ());
      gm.registered_missing <- Missing_none;
      gm.pending_guards <- 0

let registered_missing_mem (gm : task_guard_meta) sid =
  match gm.registered_missing with
  | Missing_none -> false
  | Missing_one one -> Int.equal sid one
  | Missing_many tbl -> Hashtbl.mem tbl sid

let set_registered_missing_from_unique (t : task)
    (unique_missing : (int, any_signal) Hashtbl.t) =
  let gm = guard_meta_exn t in
  let count = Hashtbl.length unique_missing in
  gm.pending_guards <- count;
  match count with
  | 0 -> clear_registered_missing t
  | 1 ->
      let sid = ref (-1) in
      Hashtbl.iter (fun key _ -> sid := key) unique_missing;
      (match gm.registered_missing with
      | Missing_many tbl -> Hashtbl.reset tbl
      | Missing_none | Missing_one _ -> ());
      gm.registered_missing <- Missing_one !sid
  | _ ->
      let tbl =
        match gm.registered_missing with
        | Missing_many tbl ->
            Hashtbl.reset tbl;
            tbl
        | Missing_none | Missing_one _ -> Hashtbl.create (max 4 count)
      in
      Hashtbl.iter (fun sid _ -> Hashtbl.replace tbl sid ()) unique_missing;
      gm.registered_missing <- Missing_many tbl

let register_missing_guards (st : scheduler_state) (t : task)
    (missing : any_signal list) =
  let gm = guard_meta_exn t in
  let unique_missing : (int, any_signal) Hashtbl.t =
    Hashtbl.create (max 4 (List.length missing))
  in
  List.iter
    (fun ((Any s) as signal) ->
      if not (Hashtbl.mem unique_missing s.s_id) then
        Hashtbl.add unique_missing s.s_id signal)
    missing;
  if gm.guard_registration_instant <> st.debug.instant_counter then begin
    gm.guard_registration_instant <- st.debug.instant_counter;
    clear_registered_missing t
  end;
  Hashtbl.iter
    (fun sid signal ->
      if not (registered_missing_mem gm sid) then begin
        let Any s = signal in
        ensure_signal_tracked st s;
        s.guard_waiters <- t :: s.guard_waiters
      end)
    unique_missing;
  set_registered_missing_from_unique t unique_missing

let infer_guard_cache parent guards =
  let epoch = !guard_epoch in
  let default () =
    if guards = [] then (epoch, true) else (-1, false)
  in
  match parent with
  | None -> default ()
  | Some p ->
      let p_guards = task_guards p in
      if guards == p_guards then
        if p_guards = [] then (epoch, true)
        else
          (match p.guard_meta with
          | Some pgm when pgm.guards_checked_epoch = epoch ->
              (epoch, pgm.guards_ok_cached)
          | _ -> default ())
      else
        match guards with
        | Any s :: gs when gs == p_guards ->
            if p_guards = [] then (epoch, s.present)
            else
              (match p.guard_meta with
              | Some pgm
                when pgm.guards_checked_epoch = epoch && pgm.guards_ok_cached ->
                  (epoch, s.present)
              | _ -> default ())
        | _ -> default ()

let task_pool_limit = 0

let noop_run () = ()

let fresh_task () =
  {
    t_id = -1
  ; guard_meta = None
  ; kill_ctx = empty_kill_context
  ; thread = -1
  ; run = noop_run
  ; queued = false
  ; blocked = false
  }

let acquire_task_slot (st : scheduler_state) =
  match st.free_tasks with
  | t :: rest ->
      st.free_tasks <- rest;
      st.free_task_count <- st.free_task_count - 1;
      t
  | [] -> fresh_task ()

let make_guard_meta ?parent guards =
  if guards = [] then None
  else
    let guards_checked_epoch, guards_ok_cached =
      infer_guard_cache parent guards
    in
    Some
      {
        guards
      ; pending_guards = 0
      ; registered_missing = Missing_none
      ; guard_registration_instant = -1
      ; guards_checked_epoch
      ; guards_ok_cached
      }

let create_task ?parent st thread guards kill_ctx run =
  let state = Tempo_thread.ensure st.threads thread in
  if state.completed && state.active = 0 then state.completed <- false;
  state.active <- state.active + 1;
  let t_id = st.debug.task_counter in
  st.debug.task_counter <- st.debug.task_counter + 1;
  let guard_meta = make_guard_meta ?parent guards in
  if task_pool_limit <= 0 then
    {
      t_id
    ; guard_meta
    ; kill_ctx
    ; thread
    ; run
    ; queued = false
    ; blocked = false
    }
  else
    let t = acquire_task_slot st in
    clear_registered_missing t;
    t.t_id <- t_id;
    t.guard_meta <- guard_meta;
    t.kill_ctx <- kill_ctx;
    t.thread <- thread;
    t.run <- run;
    t.queued <- false;
    t.blocked <- false;
    t

let spawn_now ?parent st thread guards kill_ctx run =
  let t = create_task ?parent st thread guards kill_ctx run in
  enqueue_now st t;
  t

let spawn_next ?parent st thread guards kill_ctx run =
  let t = create_task ?parent st thread guards kill_ctx run in
  enqueue_next st t;
  t

let recycle_task (st : scheduler_state) (t : task) =
  if st.free_task_count < task_pool_limit then begin
    clear_registered_missing t;
    t.guard_meta <- None;
    t.kill_ctx <- empty_kill_context;
    t.thread <- -1;
    t.run <- noop_run;
    t.queued <- false;
    t.blocked <- false;
    st.free_tasks <- t :: st.free_tasks;
    st.free_task_count <- st.free_task_count + 1
  end

let block_on_guards (st : scheduler_state) (t : task) =
  if not t.blocked then (
    t.blocked <- true;
    st.blocked <- t :: st.blocked);
  match t.guard_meta with
  | None -> ()
  | Some gm ->
      gm.guards_checked_epoch <- !guard_epoch;
      gm.guards_ok_cached <- false;
      let miss = List.filter (fun (Any s) -> not s.present) gm.guards in
      register_missing_guards st t miss

let block_on_guards_with_missing (st : scheduler_state) (t : task) miss =
  if not t.blocked then (
    t.blocked <- true;
    st.blocked <- t :: st.blocked);
  match t.guard_meta with
  | None -> ()
  | Some gm ->
      gm.guards_checked_epoch <- !guard_epoch;
      gm.guards_ok_cached <- false;
      register_missing_guards st t miss

let wake_guard_waiters st s =
  List.iter
    (fun t ->
      match t.guard_meta with
      | None -> ()
      | Some gm ->
          if gm.guard_registration_instant = st.debug.instant_counter then begin
            (match gm.registered_missing with
            | Missing_none -> ()
            | Missing_one sid ->
                if Int.equal sid s.s_id then begin
                  gm.registered_missing <- Missing_none;
                  if gm.pending_guards > 0 then
                    gm.pending_guards <- gm.pending_guards - 1
                end
            | Missing_many tbl ->
                if Hashtbl.mem tbl s.s_id then begin
                  Hashtbl.remove tbl s.s_id;
                  if gm.pending_guards > 0 then
                    gm.pending_guards <- gm.pending_guards - 1
                end);
            if gm.pending_guards = 0 then begin
              (match gm.registered_missing with
              | Missing_many tbl -> Hashtbl.reset tbl
              | Missing_none | Missing_one _ -> ());
              gm.registered_missing <- Missing_none;
              gm.guards_checked_epoch <- !guard_epoch;
              gm.guards_ok_cached <- true;
              if task_kills_alive t then enqueue_now st t
            end
          end else if task_guards_ok t && task_kills_alive t then
            enqueue_now st t)
    s.guard_waiters;
  s.guard_waiters <- []
