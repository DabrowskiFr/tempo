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

open Effect.Deep
open Tempo_types
open Tempo_thread 
open Tempo_task
open Tempo_signal

let () = Tempo_log.init ()
let log_ctx st =
  Tempo_log.context ~instant:st.debug.instant_counter ~step:st.debug.step_counter

type snapshot_phase =
  [ `Before_step
  | `After_step
  | `After_finalize
  | `After_rollover
  ]

type runtime_snapshot = {
    phase : snapshot_phase
  ; instant : int
  ; step : int
  ; current_q : int
  ; blocked_q : int
  ; next_q : int
  ; tracked_signals : int
  ; awaiters : int
  ; guard_waiters : int
  ; kill_watchers : int
  ; live_tasks : int
  ; kill_context_refs : int
  ; kill_context_nodes : int
  ; kill_context_max_depth : int
  ; active_thread_slots : int
  ; total_active_threads : int
  ; total_suspended_threads : int
  ; task_counter : int
  ; thread_counter : int
  ; signal_counter : int
  ; free_task_count : int
  ; gc_minor_words : float
  ; gc_promoted_words : float
  ; gc_major_words : float
  ; gc_minor_collections : int
  ; gc_major_collections : int
  ; gc_heap_words : int
  ; gc_live_words : int
  ; gc_free_words : int
  ; gc_top_heap_words : int
  ; gc_stack_size : int
  ; cum_tasks_created : int
  ; cum_tasks_disposed : int
  ; cum_tasks_enqueued_now : int
  ; cum_tasks_enqueued_next : int
  ; cum_tasks_blocked : int
  ; cum_signals_created : int
  ; cum_signals_tracked : int
  ; cum_signals_untracked : int
  ; cum_awaiters_registered : int
  ; cum_awaiters_resumed : int
  ; cum_awaiters_pruned : int
  ; cum_guard_waiter_registrations : int
  ; cum_guard_waiter_wakeups : int
  ; cum_kill_watchers_registered : int
  ; cum_kill_watchers_fired : int
  ; cum_kill_watchers_pruned : int
}

let fold_signals_stats (signals : any_signal list) =
  List.fold_left
    (fun (awaiters, guards, kills) (Any s) ->
      ( awaiters + List.length s.awaiters
      , guards + List.length s.guard_waiters
      , kills + List.length s.kill_watchers ))
    (0, 0, 0) signals

let fold_threads_stats (threads : thread_table) =
  Array.fold_left
    (fun (slots, active, suspended) state ->
      match state with
      | None -> (slots, active, suspended)
      | Some ts ->
          ( slots + 1
          , active + ts.active
          , suspended + ts.suspended ))
    (0, 0, 0) threads.states

let fold_kill_context_stats (st : scheduler_state) =
  let refs = ref 0 in
  let nodes = ref 0 in
  let max_depth = ref 0 in
  let rec walk depth ctx =
    refs := !refs + 1;
    if depth > !max_depth then max_depth := depth;
    match ctx with
    | KEmpty -> ()
    | KNode node ->
        nodes := !nodes + 1;
        walk (depth + 1) node.parent
  in
  let walk_task (t : task) = walk 0 t.kill_ctx in
  let walk_signal (Any s) =
    List.iter (fun (aw : _ awaiter) -> walk 0 aw.kill_ctx) s.awaiters;
    List.iter (fun (w : kill_watcher) -> walk 0 w.kill_ctx) s.kill_watchers
  in
  Queue.iter walk_task st.current;
  List.iter walk_task st.next_instant;
  List.iter walk_task st.blocked;
  List.iter walk_task st.free_tasks;
  List.iter walk_signal st.signals;
  Array.iter
    (function
      | None -> ()
      | Some ts ->
          List.iter
            (fun (w : join_waiter) -> walk 0 w.kill_ctx)
            ts.waiters)
    st.threads.states;
  (!refs, !nodes, !max_depth)

let make_snapshot (st : scheduler_state) (phase : snapshot_phase) =
  let awaiters, guard_waiters, kill_watchers = fold_signals_stats st.signals in
  let active_thread_slots, total_active_threads, total_suspended_threads =
    fold_threads_stats st.threads
  in
  let kill_context_refs, kill_context_nodes, kill_context_max_depth =
    fold_kill_context_stats st
  in
  let gc = Gc.quick_stat () in
  {
    phase
  ; instant = st.debug.instant_counter
  ; step = st.debug.step_counter
  ; current_q = Queue.length st.current
  ; blocked_q = List.length st.blocked
  ; next_q = List.length st.next_instant
  ; tracked_signals = List.length st.signals
  ; awaiters
  ; guard_waiters
  ; kill_watchers
  ; live_tasks = total_active_threads + total_suspended_threads
  ; kill_context_refs
  ; kill_context_nodes
  ; kill_context_max_depth
  ; active_thread_slots
  ; total_active_threads
  ; total_suspended_threads
  ; task_counter = st.debug.task_counter
  ; thread_counter = st.thread_counter
  ; signal_counter = st.debug.sig_counter
  ; free_task_count = st.free_task_count
  ; gc_minor_words = gc.Gc.minor_words
  ; gc_promoted_words = gc.Gc.promoted_words
  ; gc_major_words = gc.Gc.major_words
  ; gc_minor_collections = gc.Gc.minor_collections
  ; gc_major_collections = gc.Gc.major_collections
  ; gc_heap_words = gc.Gc.heap_words
  ; gc_live_words = gc.Gc.live_words
  ; gc_free_words = gc.Gc.free_words
  ; gc_top_heap_words = gc.Gc.top_heap_words
  ; gc_stack_size = gc.Gc.stack_size
  ; cum_tasks_created = st.metrics.tasks_created
  ; cum_tasks_disposed = st.metrics.tasks_disposed
  ; cum_tasks_enqueued_now = st.metrics.tasks_enqueued_now
  ; cum_tasks_enqueued_next = st.metrics.tasks_enqueued_next
  ; cum_tasks_blocked = st.metrics.tasks_blocked
  ; cum_signals_created = st.metrics.signals_created
  ; cum_signals_tracked = st.metrics.signals_tracked
  ; cum_signals_untracked = st.metrics.signals_untracked
  ; cum_awaiters_registered = st.metrics.awaiters_registered
  ; cum_awaiters_resumed = st.metrics.awaiters_resumed
  ; cum_awaiters_pruned = st.metrics.awaiters_pruned
  ; cum_guard_waiter_registrations = st.metrics.guard_waiter_registrations
  ; cum_guard_waiter_wakeups = st.metrics.guard_waiter_wakeups
  ; cum_kill_watchers_registered = st.metrics.kill_watchers_registered
  ; cum_kill_watchers_fired = st.metrics.kill_watchers_fired
  ; cum_kill_watchers_pruned = st.metrics.kill_watchers_pruned
  }

let emit_snapshot on_snapshot st phase =
  match on_snapshot with
  | None -> ()
  | Some f -> f (make_snapshot st phase)

let dispose_task st t =
  st.metrics.tasks_disposed <- st.metrics.tasks_disposed + 1;
  finish_task st.threads t.thread;
  recycle_task st t

let handle_task : scheduler_state -> task -> unit =
  fun st t ->
    let debug_enabled = Tempo_log.should_log ~level:Logs.Debug () in
    let ctx = log_ctx st in
    let parent_thread = t.thread in
    let parent_guards = task_guards t in
    let parent_kill_ctx = task_kill_ctx t in
    let parent_alive () = kill_context_alive parent_kill_ctx in
    let dlog ?task ?signal scope fmt =
      if debug_enabled then
        Tempo_log.log ?task ?signal (ctx) scope fmt
      else Format.ifprintf Format.std_formatter fmt
    in
    let run_task () =
      match t.run () with
      | () -> dlog ~task:t.t_id "step" "task done | task=#%d thread=#%d" t.t_id t.thread
      | effect (New_signal ()), k ->
          let s = fresh_event_signal st in
          dlog ~task:t.t_id ~signal:s.s_id "step"
            "create event signal | task=#%d | signal=#%d" t.t_id s.s_id;
          continue k s
      | effect (New_signal_agg (initial, combine)), k ->
          let s = fresh_aggregate_signal st ~initial ~combine in
          dlog ~task:t.t_id ~signal:s.s_id "step"
            "create aggregate signal | task=#%d signal=#%d" t.t_id s.s_id;
          continue k s
      | effect (Emit (s, v)), k ->
          dlog ~task:t.t_id ~signal:s.s_id "step"
            "emit | task=#%d signal=#%d" t.t_id s.s_id;
          update_signal st s v;
          continue k ()
      | effect (Await s), k ->
          let resume v =
            let new_task =
              spawn_next ~parent:t st parent_thread parent_guards parent_kill_ctx
                (fun () -> continue k v)
            in
            dlog ~task:new_task.t_id ~signal:s.s_id "step"
              "resume await | task=#%d -> task=#%d signal=#%d"
              t.t_id new_task.t_id s.s_id
          in
          if s.present then
            match s.kind with
            | Event_signal ->
                dlog ~task:t.t_id ~signal:s.s_id "step"
                  "await present | task=#%d signal=#%d -> resume next instant"
                  t.t_id s.s_id;
                resume (Option.get s.value)
            | Aggregate_signal _ ->
                mark_suspended st.threads parent_thread;
                register_awaiter st s
                  { resume; kill_ctx = parent_kill_ctx; thread = parent_thread }
          else begin
            dlog ~task:t.t_id ~signal:s.s_id "step"
              "await absent | task=#%d signal`#%d -> enqueue" t.t_id s.s_id;
            mark_suspended st.threads parent_thread;
            register_awaiter st s
              { resume; kill_ctx = parent_kill_ctx; thread = parent_thread }
          end;
      | effect (Await_immediate s), k ->
          if s.present then
            match s.value with
            | Some v ->
                dlog ~task:t.t_id ~signal:s.s_id "step"
                  "await_immediate present | task=#%d signal`#%d -> resume now"
                  t.t_id s.s_id;
                continue k v;
            | None -> failwith "Error : present but no value"
          else
            let resume v =
              let new_task =
                spawn_now ~parent:t st parent_thread parent_guards parent_kill_ctx
                  (fun () -> continue k v)
              in
              dlog ~task:t.t_id ~signal:s.s_id "step"
                "resume await imm | task=#%d -> task=#%d signal`#%d"
                t.t_id new_task.t_id s.s_id
            in
            dlog ~task:t.t_id ~signal:s.s_id "step"
              "await immediate absent | task=#%d signal`#%d -> save continuation"
              t.t_id s.s_id;
            mark_suspended st.threads parent_thread;
            register_awaiter st s
              { resume; kill_ctx = parent_kill_ctx; thread = parent_thread };
      | effect (Register_kill_watcher (s, kill)), k ->
          ensure_signal_tracked st s;
          register_kill_watcher st s kill parent_kill_ctx;
          continue k ();
      | effect Pause, k ->
          let new_task =
            spawn_next ~parent:t st parent_thread parent_guards parent_kill_ctx
              (fun () -> continue k ())
          in
          dlog ~task:new_task.t_id "step"
            "pause | task=#%d resume next instant as task #%d" t.t_id new_task.t_id;
      | effect (Fork p_child), k ->
          let child_thread = Tempo_thread.new_thread_id st in
          let t' =
            spawn_now ~parent:t st child_thread parent_guards parent_kill_ctx
              p_child
          in
          dlog ~task:t.t_id "step"
            "spawn logical thread=#%d as task=#%d" child_thread t'.t_id;
          continue k child_thread;
      | effect (With_guard (s, body)), k ->
          let guard_task =
            spawn_now ~parent:t st parent_thread (Any s :: parent_guards) parent_kill_ctx
              (fun () ->
                body (); (* body may perform an abort_kill*)
                if parent_alive () then begin
                  let t' =
                    spawn_now ~parent:t st parent_thread parent_guards parent_kill_ctx
                      (fun () -> continue k ())
                  in  dlog ~task:t.t_id "step"
                    "with_guard exit | tasks=#%d -> task=#%d" t.t_id t'.t_id;
                end)
          in
          dlog ~task:guard_task.t_id ~signal:s.s_id "step"
            "with_guard enter | task=#%d signal=#%d -> schedule task=#%d"
            t.t_id s.s_id guard_task.t_id;
      | effect (With_kill (kk, body)), k ->
          let resume_mode : [ `None | `Later | `Now ] ref = ref `None in
          let resumed = ref false in
          let resume_if mode () =
            if (not !resumed) && !resume_mode = mode then begin
              resumed := true;
              continue k ()
            end
          in
          let continue_now () =
            kk.cleanup <- None;
            if parent_alive () then begin
              if !resume_mode <> `Now then begin
                resume_mode := `Now;
                let t' =
                  spawn_now ~parent:t st parent_thread parent_guards parent_kill_ctx
                    (resume_if `Now)
                in
                dlog ~task:t.t_id "step"
                  "with_kill exit | normal termination : task=#%d -> task=#%d"
                  t.t_id t'.t_id;
              end
            end else
              dlog ~task:t.t_id "step"
                "with_kill exit | drop continuation (parent kill dead) task=#%d"
                t.t_id;
          and continue_later () =
            kk.cleanup <- None;
            if parent_alive () then begin
              if !resume_mode = `None then begin
                resume_mode := `Later;
                let t' =
                  spawn_next ~parent:t st parent_thread parent_guards parent_kill_ctx
                    (resume_if `Later)
                in
                dlog ~task:t.t_id "step"
                  "with_kill | cancelation : task=#%d -> task #%d" t.t_id t'.t_id;
              end
            end else
              dlog ~task:t.t_id "step"
                "with_kill | cancelation drop (parent kill dead) task=#%d"
                t.t_id;
          in
          if not !(kk.alive) then begin
            continue_later ()
          end else begin
            kk.cleanup <- Some continue_later;
            let runner () =
              body ();
              continue_now ()
            in
            let new_task =
              spawn_now ~parent:t st parent_thread parent_guards
                (push_kill_context kk parent_kill_ctx) runner
            in
            dlog ~task:new_task.t_id "step"
              "with_kill enter | task=#%d -> task=#%d" t.t_id new_task.t_id
          end;
      | effect (Watch (s, body)), k ->
          if s.present then
            continue k ()
          else
            let kk = Tempo_low_level.new_kill () in
            let resumed = ref false in
            let resume_next () =
              if not !resumed then begin
                resumed := true;
                kk.alive := false;
                kk.cleanup <- None;
                let t' =
                  spawn_next ~parent:t st parent_thread parent_guards parent_kill_ctx
                    (fun () -> continue k ())
                in
                dlog ~task:t.t_id "step"
                  "watch exit (killed) | task=#%d -> task=#%d" t.t_id t'.t_id
              end
            in
            let resume_now () =
              if not !resumed then begin
                resumed := true;
                kk.alive := false;
                kk.cleanup <- None;
                let t' =
                  spawn_now ~parent:t st parent_thread parent_guards parent_kill_ctx
                    (fun () -> continue k ())
                in
                dlog ~task:t.t_id "step"
                  "watch exit (normal) | task=#%d -> task=#%d" t.t_id t'.t_id
              end
            in
            kk.cleanup <- Some resume_next;
            register_kill_watcher st s kk parent_kill_ctx;
            let guarded_body () =
              body ();
              resume_now ()
            in
            let t' =
              spawn_now ~parent:t st parent_thread parent_guards
                (push_kill_context kk parent_kill_ctx) guarded_body
            in
            dlog ~task:t.t_id ~signal:s.s_id "step"
              "watch enter | task=#%d signal=#%d -> task=#%d"
              t.t_id s.s_id t'.t_id;
        | effect (Join thread_id), k ->
          if thread_id = parent_thread then invalid_arg "join: cannot join current thread";
          let resume () =
            let t' =
              spawn_now ~parent:t st parent_thread parent_guards parent_kill_ctx
                (fun () -> continue k ())
            in
            dlog ~task:t.t_id "step.join"
            "join resume | waiter task=#%d on task=#%d as task =#%d" t.t_id thread_id t'.t_id;
          in
          if thread_id < 0 || thread_id >= st.thread_counter then
            invalid_arg (Printf.sprintf "unknown thread %d" thread_id)
          else
            match Tempo_thread.find_opt st.threads thread_id with
            | None ->
                dlog ~task:t.t_id "step.join"
                  "join immediate | waiter task=#%d on completed thread=#%d"
                  t.t_id thread_id;
                continue k ()
            | Some state ->
                if state.completed && state.active = 0 then begin
                  dlog ~task:t.t_id "step.join"
                    "join immediate | waiter task=#%d on completed thread=#%d"
                    t.t_id thread_id;
                  continue k ()
                end
                else begin
                  dlog ~task:t.t_id "step"
                    "joint wait | task=#%d waiting for thread=#%d" t.t_id thread_id;
                  mark_suspended st.threads parent_thread;
                  add_join_waiter st.threads thread_id parent_thread parent_kill_ctx resume
                end
    in
    let cleanup () =
      dispose_task st t
    in
    try Fun.protect ~finally:cleanup run_task with
    | Aborted -> ()

let rec step : scheduler_state -> unit =
  fun st ->
    let debug_enabled = Tempo_log.should_log ~level:Logs.Debug () in
    let ctx = log_ctx st in
    let counter = if debug_enabled then Some (Mtime_clock.counter ()) else None in
    let record_step_metrics () =
      match counter with
      | Some c ->
          let span = Mtime_clock.count c in
          Tempo_log.log_step_summary (ctx) span;
          Tempo_log.record_duration "step" span
      | None -> ()
    in
    if debug_enabled then begin
      Tempo_log.log_banner_step (ctx);
      Tempo_log.log_snapshot (ctx)
        ~current:(Tempo_log.snapshot_queue st.current)
        ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals
    end;
    st.debug.step_counter <- st.debug.step_counter + 1;
      let rec take_next () =
        if Queue.is_empty st.current then 
          begin 
            Tempo_log.log ~level:Logs.Debug (ctx) "step"
              "queue empty | stop current instant";
            record_step_metrics ();
            None
          end
        else
          let t = Queue.take st.current in
          t.queued <- false;
          if task_kills_alive t then Some t
          else (
            dispose_task st t;
            take_next ())
      in
      match take_next () with
      | None ->
          record_step_metrics ()
      | Some t ->
          if debug_enabled then Tempo_log.log_pick (ctx) t;
          let continue =
            if task_has_guards t && not (task_guards_ok t) then begin
          if debug_enabled then begin
                let missing = Tempo_signal.missing_guards (task_guards t) in
                Tempo_log.log_block (ctx) t missing;
                block_on_guards_with_missing st t missing
              end else
                block_on_guards st t;
              fun () -> step st
            end else begin
              handle_task st t;
              fun () -> step st
            end
          in
          if debug_enabled then
            Tempo_log.log_snapshot (ctx)
              ~current:(Tempo_log.snapshot_queue st.current)
              ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals;
          record_step_metrics ();
          continue ()

let rec run_instant : (runtime_snapshot -> unit) option -> (unit -> unit) ->
    (unit -> unit) -> scheduler_state -> int option -> unit =
  fun on_snapshot before_step after_step st remaining ->
    match remaining with
    | Some n when n <= 0 -> ()
    | _ ->
        let debug_enabled = Tempo_log.should_log ~level:Logs.Debug () in
        let ctx = log_ctx st in
        if debug_enabled then begin
          Tempo_log.log_banner_instant (ctx) st.debug.instant_counter;
          Tempo_log.log_snapshot (ctx)
            ~current:(Tempo_log.snapshot_queue st.current)
            ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals
        end;
        let counter = if debug_enabled then Some (Mtime_clock.counter ()) else None in
        st.blocked <- [];
        emit_snapshot on_snapshot st `Before_step;
        before_step ();
        step st;
        after_step ();
        emit_snapshot on_snapshot st `After_step;
        (match counter with
        | Some c ->
            let span = Mtime_clock.count c in
            Tempo_log.log (ctx) "instant" "instant=%a" Tempo_log.pp_span span;
            Tempo_log.record_duration "instant" span
        | None -> ());
        finalize_signals st;
        prune_dead_join_waiters st.threads (current_kill_epoch ());
        List.iter
          ( fun (t : task) ->
              if t.blocked then begin
              t.blocked <- false;
              if task_kills_alive t then enqueue_next st t
            end )
          st.blocked;
          emit_snapshot on_snapshot st `After_finalize;
          st.debug.instant_counter <- st.debug.instant_counter + 1;
          st.debug.step_counter <- 0;
          match st.next_instant with
            | [] ->
                if debug_enabled then
                  Tempo_log.log ~level:Logs.Debug (ctx) "tasks"
                    "no more tasks | for next instant -> stop"
            | ts ->
              let survivors =
                List.filter
                  (fun t ->
                     if task_kills_alive t then true
                    else (
                      dispose_task st t;
                      false))
                  ts
              in
              if debug_enabled then
                Tempo_log.log (ctx) "instant"
                  "rollover tasks | moving %d tasks to next instant"
                  (List.length survivors);
              st.next_instant <- [];
              List.iter (enqueue_now st) survivors;
              emit_snapshot on_snapshot st `After_rollover;
              run_instant on_snapshot before_step after_step st
                (Option.map pred remaining)

let create_scheduler_state () = 
  let metrics =
    Tempo_types.
      {
        tasks_created = 0
      ; tasks_disposed = 0
      ; tasks_enqueued_now = 0
      ; tasks_enqueued_next = 0
      ; tasks_blocked = 0
      ; signals_created = 0
      ; signals_tracked = 0
      ; signals_untracked = 0
      ; awaiters_registered = 0
      ; awaiters_resumed = 0
      ; awaiters_pruned = 0
      ; guard_waiter_registrations = 0
      ; guard_waiter_wakeups = 0
      ; kill_watchers_registered = 0
      ; kill_watchers_fired = 0
      ; kill_watchers_pruned = 0
      }
  in
  { current         = Queue.create ()
    ;next_instant    = []
    ;blocked         = []
    ;signals         = []
    ;thread_counter  = 0
    ;debug           =
      { sig_counter     = 0
      ; task_counter    = 1
      ; step_counter    = 0
      ; instant_counter = 0 }
    ;threads         = Tempo_thread.create ()
    ;free_tasks      = []
    ;free_task_count = 0
    ;metrics         = metrics
  }

let execute ?instants ?(input = fun () -> None) ?(output = fun _ -> ())
    ?on_snapshot initial =
  let st = create_scheduler_state () in
  let input_signal = fresh_event_signal st in
  let output_signal = fresh_event_signal st in
  let before_step () =
    match input () with
    | None -> ()
    | Some payload -> emit_event_from_host st input_signal payload
  in
  let after_step () =
      match output_signal.value with
      | Some value -> output value
      | None -> ()
  in
  let thread = Tempo_thread.new_thread_id st in
  ignore
    (spawn_now st thread [] empty_kill_context
       (fun () -> initial input_signal output_signal));
  Tempo_log.log_banner (log_ctx st) "execute" "runtime start | schedule initial task=#%d";
  run_instant on_snapshot before_step after_step st instants;
  Tempo_log.log_duration_summary ()
