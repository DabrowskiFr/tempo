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

(* Keep a mutable reference to the stats of the step currently being
   executed. This allows resumed continuations (captured in a previous step)
   to still record their spawns in the stats of the step that is actually
   running. *)
let current_step_stats : Tempo_log.step_stats option ref = ref None

let with_stats f =
  match !current_step_stats with
  | Some stats -> f stats
  | None -> ()

let inc_spawns_now () = with_stats (fun stats -> stats.spawns_now <- stats.spawns_now + 1)
let inc_spawns_next () = with_stats (fun stats -> stats.spawns_next <- stats.spawns_next + 1)
let inc_aborted () = with_stats (fun stats -> stats.aborted <- stats.aborted + 1)

(* let update_signal st s v = Tempo_signal.update_signal st s v
let emit_event_from_host st s v = Tempo_signal.emit_event_from_host st s v *)

let prune_waiting st thread =
  st.waiting <- List.filter (fun (_, target) -> target <> thread) st.waiting

let handle_task : scheduler_state -> Tempo_log.step_stats -> task -> unit =
  fun st _stats t ->
    let ctx = log_ctx st in
    let run_task () =
      match t.run () with
      | () -> ()
      | effect (New_signal (_)), k ->
          let s = fresh_event_signal st in
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.signal"
            "create event signal #%d" s.s_id;
          continue k s
      | effect (New_signal_agg (initial, combine)), k ->
          let s = fresh_aggregate_signal st ~initial ~combine in
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.signal"
            "create aggregate signal #%d" s.s_id;
          continue k s
      | effect (Emit (s, v)), k ->
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.emit"
            "emit signal #%d" s.s_id;
          update_signal st s v;
          continue k ()
      | effect (Await s), k ->
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.await"
            "await signal #%d" s.s_id;
          let resume v =
            inc_spawns_next ();
            let new_task =
              spawn_next_with_id st t.thread t.guards t.kills
                (fun () -> continue k v)
            in
            Tempo_log.log ~task:new_task.t_id ~signal:s.s_id ctx "step.await"
              "resume continuation of task #%d as task #%d after signal #%d"
              t.t_id new_task.t_id s.s_id
          in
          if s.present then
            match s.kind with
            | Event_signal -> 
                resume (Option.get s.value)
            | Aggregate_signal _ -> s.awaiters <- resume :: s.awaiters
          else begin
            Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.await"
              "signal #%d absent: enqueue continuation in awaiters" s.s_id;
            s.awaiters <- resume :: s.awaiters
          end;
      | effect (Await_immediate s), k ->
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.await"
            "await_immediate signal #%d" s.s_id;
          if s.present then
            match s.value with
            | Some v ->
                Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.await"
                  "await_immediate satisfied immediately for task #%d signal #%d"
                  t.t_id s.s_id;
                continue k v;
            | None -> failwith "Error : present but no value"
          else
            let resume v =
              inc_spawns_now ();
              Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step.await"
                "signal #%d absent: enqueue await_immediate continuation" s.s_id;
              let new_task =
                spawn_now_with_id st t.thread t.guards t.kills
                  (fun () -> continue k v)
              in
              Tempo_log.log ~task:new_task.t_id ~signal:s.s_id ctx "step.await"
                "resume continuation of task #%d as task #%d after await_immediate on signal #%d"
                t.t_id new_task.t_id s.s_id
            in
            s.awaiters <- resume :: s.awaiters;
      | effect Pause, k ->
          inc_spawns_next ();
          let new_task =
            spawn_next_with_id st t.thread t.guards t.kills (fun () -> continue k ())
          in
          Tempo_log.log ~task:new_task.t_id ctx "step.pause"
            "pause: resumed as task #%d" new_task.t_id;
      | effect (Fork p_child), k ->
          let child_thread = Tempo_thread.new_thread_id st in
          Tempo_log.log ~task:t.t_id ctx "step.fork"
            "spawn logical thread %d" child_thread;
          inc_spawns_now ();
          spawn_now st child_thread t.guards t.kills p_child;
          continue k child_thread;
            
      | effect (With_guard (s, body)), k ->
          inc_spawns_now ();
          let guard_task =
            spawn_now_with_id st t.thread (Any s :: t.guards) t.kills
              (fun () ->
                body (); (* body may perform an abort_kill*)
                if kills_alive t.kills then begin
                  inc_spawns_now ();
                  spawn_now st t.thread t.guards t.kills (fun () -> continue k ())
                end)
          in
          Tempo_log.log ~task:guard_task.t_id ~signal:s.s_id ctx "step.guard"
            "with_guard signal #%d spawned task #%d" s.s_id guard_task.t_id;
      | effect (With_kill (kk, body)), k ->
          let continue_now () =
            kk.cleanup <- None;
            inc_spawns_now ();
            spawn_now st t.thread t.guards t.kills (fun () -> continue k ())
          and continue_later () =
            kk.cleanup <- None;
            inc_spawns_next ();
            spawn_next st t.thread t.guards t.kills (fun () -> continue k ())
          in
          kk.cleanup <- Some continue_later;
          let runner () =
            body ();
            continue_now ()
          in
          inc_spawns_now ();
          let new_task =
            spawn_now_with_id st t.thread t.guards (kk :: t.kills) runner
          in
          Tempo_log.log ~task:new_task.t_id ctx "step.kill"
            "with_kill spawned task #%d" new_task.t_id;
      | effect (Join thread_id), k ->
          if thread_id = t.thread then invalid_arg "join: cannot join current thread";
          let resume () =
            inc_spawns_now ();
            spawn_now st t.thread t.guards t.kills (fun () -> continue k ())
          in
          let state = Tempo_thread.find st.threads thread_id in
          if state.completed && state.active = 0 then resume ()
          else begin
            Tempo_log.log ~task:t.t_id ctx "step.join"
              "wait for logical thread %d" thread_id;
            if not (List.exists (fun (w, tgt) -> w = t.thread && tgt = thread_id) st.waiting)
            then st.waiting <- (t.thread, thread_id) :: st.waiting;
            add_join_waiter st.threads thread_id resume
          end
    in
    let cleanup () =
      finish_task st.threads t.thread;
      prune_waiting st t.thread
    in
    try Fun.protect ~finally:cleanup run_task with
    | Aborted -> inc_aborted ()

(* Scheduler main loop *)
  let rec step : scheduler_state -> unit =
  fun st ->
    let counter = Mtime_clock.counter () in
    let ctx = log_ctx st in
    let stats = Tempo_log.empty_stats () in
    current_step_stats := Some stats;
    Tempo_log.log_banner_step ctx;
    Tempo_log.log_snapshot ctx
      ~current:(Tempo_log.snapshot_queue st.current)
      ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals;
    st.debug.step_counter <- st.debug.step_counter + 1;
    if Queue.is_empty st.current then begin
      Tempo_log.log ~level:Logs.Debug ctx "step" "queue empty, stopping current instant";
      let span = Mtime_clock.count counter in
      Tempo_log.log_step_summary ctx stats span;
      Tempo_log.log_waiting ctx st.waiting;
      Tempo_log.record_duration "step" span;
      current_step_stats := None
    end else begin
      let rec take_next () =
        if Queue.is_empty st.current then None
        else
          let t = Queue.take st.current in
          t.queued <- false;
          if kills_alive t.kills then Some t
          else (
            finish_task st.threads t.thread;
            prune_waiting st t.thread;
            take_next ())
      in
      match take_next () with
      | None ->
          let span = Mtime_clock.count counter in
          Tempo_log.log_step_summary ctx stats span;
          Tempo_log.log_waiting ctx st.waiting;
          Tempo_log.log_signal_waiters ctx st.signals;
          Tempo_log.record_duration "step" span;
          current_step_stats := None
      | Some t ->
          Tempo_log.log_pick ctx t;
          let continue =
            if not (guard_ok t.guards) then begin
              let missing = Tempo_signal.missing_guards t.guards in
              Tempo_log.log_block ctx t missing;
              Tempo_log.log ~task:t.t_id ctx "step.block"
                "guards not satisfied, task blocked";
              stats.blocks <- stats.blocks + 1;
              block_on_guards st t;
              fun () -> step st
            end else begin
              handle_task st stats t;
              fun () -> step st
            end
          in
          let span = Mtime_clock.count counter in
          Tempo_log.log_snapshot ctx
            ~current:(Tempo_log.snapshot_queue st.current)
            ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals;
          Tempo_log.log_step_summary ctx stats span;
          Tempo_log.log_waiting ctx st.waiting;
          Tempo_log.log_signal_waiters ctx st.signals;
          Tempo_log.record_duration "step" span;
          current_step_stats := None;
          continue ()
    end

let rec run_instant :
      ?before_step:(unit -> unit) ->
      ?after_step:(unit -> unit) ->
      scheduler_state -> int option -> unit =
  fun ?(before_step = fun () -> ()) ?(after_step = fun () -> ()) st remaining ->
    (match remaining with
    | Some n when n <= 0 -> Stdlib.exit 0
    | _ -> ());
    let ctx = log_ctx st in
    Tempo_log.log_banner_instant ctx st.debug.instant_counter;
    Tempo_log.log_snapshot ctx
      ~current:(Tempo_log.snapshot_queue st.current)
      ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals;
    let counter = Mtime_clock.counter () in
    st.blocked <- [];
    before_step ();
    step st;
    after_step ();
    let span = Mtime_clock.count counter in
    Tempo_log.log ctx "instant" "instant=%a" Tempo_log.pp_span span;
    Tempo_log.record_duration "instant" span;
    finalize_signals st;
    List.iter
      ( fun (t : task) ->
        if t.blocked then begin
          t.blocked <- false;
          if (kills_alive t.kills) then enqueue_next st t
        end )
      st.blocked;
      st.debug.instant_counter <- st.debug.instant_counter + 1;
      st.debug.step_counter <- 0;
      match st.next_instant with
        | [] ->
            Tempo_log.log ~level:Logs.Debug ctx "tasks" "no more tasks for next instant, terminating execution"
        | ts ->
          let survivors =
            List.filter
              (fun t ->
                 if kills_alive t.kills then true
                else (
                  finish_task st.threads t.thread;
                  prune_waiting st t.thread;
                  false))
              ts
          in
          Tempo_log.log ctx "instant" "➡️  moving %d tasks to next instant"
            (List.length survivors);
          st.next_instant <- [];
          List.iter (enqueue_now st) (List.rev survivors);
          run_instant ~before_step ~after_step st (Option.map pred remaining)

let execute ?instants ?(input = fun () -> None) ?(output = fun _ -> ()) initial =
  let st =
      { current         = Queue.create ();
        next_instant    = [];
        blocked         = [];
        signals         = [];
        thread_counter  = 0;
        debug           =
          { sig_counter     = 0;
            task_counter    = 1;
            step_counter    = 0;
            instant_counter = 0; };
        threads         = Hashtbl.create 16;
        waiting         = [];
      } in
  Tempo_log.log_banner (log_ctx st) "run" "Runtime Start";
  Tempo_log.log (log_ctx st) "run" "push initial tasks...";
  let input_signal = fresh_event_signal st in
  let output_signal = fresh_event_signal st in
  let before_step () =
    match input () with
    | None -> ()
    | Some payload -> emit_event_from_host st input_signal payload
  in
  let after_step () =
    if output_signal.present then
      match output_signal.value with
      | Some value -> output value
      | None -> ()
  in
  let thread = Tempo_thread.new_thread_id st in
  let _ =
    spawn_now st thread [] [] (fun () -> initial input_signal output_signal)
  in
  run_instant ~before_step ~after_step st instants;
  Tempo_log.log_duration_summary ()
