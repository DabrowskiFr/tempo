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
type thread_table = Tempo_types.thread_table

let rec kill_ctx_alive = function
  | Tempo_types.KEmpty -> true
  | Tempo_types.KNode node -> !(node.kill.Tempo_types.alive) && kill_ctx_alive node.parent

let create () : thread_table =
  { states = Array.make 64 None; waiters_pruned_kill_epoch = -1 }

let ensure_capacity (threads : thread_table) (thread : Tempo_types.thread) =
  let len = Array.length threads.states in
  if thread < len then ()
  else
    let new_len = ref len in
    while thread >= !new_len do
      new_len := !new_len * 2
    done;
    let grown = Array.make !new_len None in
    Array.blit threads.states 0 grown 0 len;
    threads.states <- grown

let ensure (threads : thread_table) (thread : Tempo_types.thread) =
  if thread < 0 then invalid_arg (Printf.sprintf "unknown thread %d" thread);
  ensure_capacity threads thread;
  match threads.states.(thread) with
  | Some ts -> ts
  | None ->
      let ts = Tempo_types.{ active = 0; suspended = 0; completed = false; waiters = [] } in
      threads.states.(thread) <- Some ts;
      ts

let find_opt (threads : thread_table) (thread : Tempo_types.thread) =
  if thread < 0 || thread >= Array.length threads.states then None
  else threads.states.(thread)

let find (threads : thread_table) (thread : Tempo_types.thread) =
  match find_opt threads thread with
  | Some ts -> ts
  | None -> invalid_arg (Printf.sprintf "unknown thread %d" thread)

let add_join_waiter (threads : thread_table) (thread : Tempo_types.thread)
    (waiter_thread : Tempo_types.thread)
    (kill_ctx : Tempo_types.kill_context) waiter =
  match find_opt threads thread with
  | None ->
      if kill_ctx_alive kill_ctx then waiter ()
  | Some state ->
      if state.Tempo_types.completed then (
        if kill_ctx_alive kill_ctx then waiter ())
      else if kill_ctx_alive kill_ctx then
        state.Tempo_types.waiters <-
          Tempo_types.{ resume = waiter; kill_ctx; thread = waiter_thread }
          :: state.Tempo_types.waiters

let can_complete (state : thread_state) =
  state.Tempo_types.active = 0 && state.Tempo_types.suspended = 0

let rec release_waiters (threads : thread_table) (waiters : Tempo_types.join_waiter list)
    =
  match waiters with
  | [] -> ()
  | (w : Tempo_types.join_waiter) :: rest ->
      mark_resumed threads w.thread;
      if kill_ctx_alive w.kill_ctx then w.resume ();
      release_waiters threads rest

and complete_if_idle (threads : thread_table) (thread : Tempo_types.thread)
    (state : thread_state) =
  if can_complete state then (
    state.Tempo_types.completed <- true;
    let waiters = state.Tempo_types.waiters in
    state.Tempo_types.waiters <- [];
    threads.states.(thread) <- None;
    match waiters with
    | [] -> ()
    | [ w ] ->
        mark_resumed threads w.thread;
        if kill_ctx_alive w.kill_ctx then w.resume ()
    | _ ->
        (* Preserve FIFO semantics only when needed. *)
        release_waiters threads (List.rev waiters))

and mark_suspended (threads : thread_table) (thread : Tempo_types.thread) =
  let state = find threads thread in
  state.Tempo_types.suspended <- state.Tempo_types.suspended + 1

and mark_resumed (threads : thread_table) (thread : Tempo_types.thread) =
  let state = find threads thread in
  if state.Tempo_types.suspended <= 0 then
    invalid_arg (Printf.sprintf "thread %d has no suspended continuation" thread)
  else (
    state.Tempo_types.suspended <- state.Tempo_types.suspended - 1;
    complete_if_idle threads thread state)

let prune_dead_join_waiters (threads : thread_table) current_kill_epoch =
  if threads.waiters_pruned_kill_epoch = current_kill_epoch then ()
  else begin
    threads.waiters_pruned_kill_epoch <- current_kill_epoch;
    Array.iter
      (fun (slot : thread_state option) ->
        match slot with
        | None -> ()
        | Some state ->
        if state.Tempo_types.waiters <> [] then
          state.Tempo_types.waiters <-
            List.fold_left
              (fun acc (w : Tempo_types.join_waiter) ->
                if kill_ctx_alive w.kill_ctx then w :: acc
                else (
                  mark_resumed threads w.thread;
                  acc))
              [] state.Tempo_types.waiters
            |> List.rev)
      threads.states
  end

let new_thread_id (st : Tempo_types.scheduler_state) =
  let id = st.thread_counter in
  st.thread_counter <- id + 1;
  id

let finish_task (threads : thread_table) (thread : Tempo_types.thread) =
  let state = find threads thread in
  state.Tempo_types.active <- state.Tempo_types.active - 1;
  complete_if_idle threads thread state
