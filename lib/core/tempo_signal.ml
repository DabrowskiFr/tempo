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

let fresh_signal_id (st : Tempo_types.scheduler_state) =
  let id = st.debug.sig_counter in
  st.debug.sig_counter <- id + 1;
  id

let abort_kill (k : Tempo_types.kill) =
  if !(k.alive) then k.alive := false;
  match k.cleanup with
  | None -> ()
  | Some cleanup ->
      k.cleanup <- None;
      cleanup ()

let fresh_event_signal (st : Tempo_types.scheduler_state) :
    'a Tempo_types.signal =
  let s =
    Tempo_types.
      {
        s_id = fresh_signal_id st
      ; tracked = false
      ; present = false
      ; value = None
      ; awaiters = []
      ; awaiters_kill_epoch = -1
      ; guard_waiters = []
      ; kill_watchers = []
      ; kill_watchers_kill_epoch = -1
      ; kind = Tempo_types.Event_signal
      }
  in
  s

let fresh_aggregate_signal (st : Tempo_types.scheduler_state) ~initial ~combine
    =
  let s =
    Tempo_types.
      {
        s_id = fresh_signal_id st
      ; tracked = false
      ; present = false
      ; value = None
      ; awaiters = []
      ; awaiters_kill_epoch = -1
      ; guard_waiters = []
      ; kill_watchers = []
      ; kill_watchers_kill_epoch = -1
      ; kind = Tempo_types.Aggregate_signal { combine; initial }
      }
  in
  s

let guard_ok guards = List.for_all (fun (Tempo_types.Any s) -> s.present) guards

let missing_guards guards =
  List.filter (fun (Tempo_types.Any s) -> not s.present) guards

let register_awaiter
    : type emit agg mode.
      Tempo_types.scheduler_state ->
      (emit, agg, mode) Tempo_types.signal_core ->
      agg Tempo_types.awaiter ->
      unit
    =
 fun st s awaiter ->
  Tempo_task.ensure_signal_tracked st s;
  if s.awaiters = [] then
    s.awaiters_kill_epoch <- Tempo_task.current_kill_epoch ();
  s.awaiters <- awaiter :: s.awaiters

let register_kill_watcher
    : type emit agg mode.
      (emit, agg, mode) Tempo_types.signal_core ->
      Tempo_types.kill ->
      Tempo_types.kill_context ->
      unit
    =
 fun s k kill_ctx ->
  if Tempo_task.kill_effectively_alive k && Tempo_task.kill_context_alive kill_ctx then begin
    if s.kill_watchers = [] then
      s.kill_watchers_kill_epoch <- Tempo_task.current_kill_epoch ();
    s.kill_watchers <- Tempo_types.{ kill = k; kill_ctx } :: s.kill_watchers
  end

let update_signal : type emit agg mode.
    Tempo_types.scheduler_state -> (emit, agg, mode) signal_core -> emit -> unit
    =
 fun st s v ->
  Tempo_task.ensure_signal_tracked st s;
  let was_present = s.present in
  let kill_epoch = Tempo_task.current_kill_epoch () in
  (match s.kind with
  | Event_signal ->
      if s.present then invalid_arg "Emit : multiple emission";
      let resumes = s.awaiters in
      s.present <- true;
      s.value <- Some v;
      s.awaiters <- [];
      s.awaiters_kill_epoch <- kill_epoch;
      List.iter
        (fun (aw : agg Tempo_types.awaiter) ->
          if Tempo_task.kill_context_alive aw.kill_ctx then aw.resume v)
        resumes
  | Aggregate_signal { combine; initial } ->
      s.present <- true;
      let acc =
        match s.value with
        | None -> combine initial v
        | Some agg -> combine agg v
      in
      s.value <- Some acc);
  if not was_present then Tempo_task.bump_guard_epoch ();
  Tempo_task.wake_guard_waiters st s

(* Emits an event signal initiated by the host (outside the effect handler), 
   waking awaiters immediately. *)
let emit_event_from_host : type a.
    Tempo_types.scheduler_state -> (a, a, event) signal_core -> a -> unit =
 fun st s value ->
  Tempo_task.ensure_signal_tracked st s;
  let was_present = s.present in
  let kill_epoch = Tempo_task.current_kill_epoch () in
  if s.present then invalid_arg "Emit : multiple emission";
  s.present <- true;
  s.value <- Some value;
  let resumes = s.awaiters in
  s.awaiters <- [];
  s.awaiters_kill_epoch <- kill_epoch;
  List.iter
    (fun (aw : a Tempo_types.awaiter) ->
      if Tempo_task.kill_context_alive aw.kill_ctx then aw.resume value)
    resumes;
  if not was_present then Tempo_task.bump_guard_epoch ();
  Tempo_task.wake_guard_waiters st s

let finalize_signals (st : Tempo_types.scheduler_state) =
  Tempo_task.bump_guard_epoch ();
  let prune_dead_awaiters : type emit agg mode.
      (emit, agg, mode) signal_core -> unit =
   fun s ->
    let epoch = Tempo_task.current_kill_epoch () in
    if s.awaiters <> [] && s.awaiters_kill_epoch <> epoch then begin
      s.awaiters <-
        List.filter
          (fun (aw : agg Tempo_types.awaiter) ->
            Tempo_task.kill_context_alive aw.kill_ctx)
          s.awaiters;
      s.awaiters_kill_epoch <- epoch
    end
  in
  let prune_dead_kill_watchers : type emit agg mode.
      (emit, agg, mode) signal_core -> unit =
   fun s ->
    let epoch = Tempo_task.current_kill_epoch () in
    if
      s.kill_watchers <> []
      && s.kill_watchers_kill_epoch <> epoch
    then begin
      s.kill_watchers <-
        List.filter
          (fun (w : Tempo_types.kill_watcher) ->
            Tempo_task.kill_effectively_alive w.kill
            && Tempo_task.kill_context_alive w.kill_ctx)
          s.kill_watchers;
      s.kill_watchers_kill_epoch <- epoch
    end
  in
  let kept_rev = ref [] in
  List.iter
    (fun ((Tempo_types.Any s) as any) ->
      if s.present then begin
        let watchers = s.kill_watchers in
        s.kill_watchers <- [];
        let alive_watchers, _ =
          List.partition
            (fun (w : Tempo_types.kill_watcher) ->
              Tempo_task.kill_effectively_alive w.kill
              && Tempo_task.kill_context_alive w.kill_ctx)
            watchers
        in
        if alive_watchers <> [] then Tempo_task.bump_kill_epoch ();
        s.kill_watchers_kill_epoch <- Tempo_task.current_kill_epoch ();
        List.iter (fun (w : Tempo_types.kill_watcher) -> abort_kill w.kill) alive_watchers
      end else
        prune_dead_kill_watchers s;
      if not s.present then prune_dead_awaiters s;
      (match s.kind with
      | Tempo_types.Aggregate_signal _ when s.present ->
          let delivered =
            match s.value with
            | Some value -> value
            | None -> failwith "aggregate signal flagged present but no value"
          in
          let resumes = s.awaiters in
          s.awaiters <- [];
          s.awaiters_kill_epoch <- Tempo_task.current_kill_epoch ();
          List.iter
            (fun (aw : _ Tempo_types.awaiter) ->
              if Tempo_task.kill_context_alive aw.kill_ctx then aw.resume delivered)
            resumes
      | _ -> ());
      s.present <- false;
      s.value <- None;
      s.guard_waiters <- [];
      if s.awaiters <> [] || s.kill_watchers <> [] then kept_rev := any :: !kept_rev
      else s.tracked <- false)
    st.signals;
  st.signals <- List.rev !kept_rev
