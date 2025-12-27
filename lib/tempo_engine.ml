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

let handle_task : scheduler_state -> task -> unit =
  fun st t ->
    let run_task () =
      match t.run () with
      | () -> ()
      | effect (New_signal (_)), k ->
          let s = fresh_event_signal st in
          ignore (spawn_now st t.thread t.guards t.kills (fun () -> continue k s))
      | effect (New_signal_agg (initial, combine)), k ->
          let s = fresh_aggregate_signal st ~initial ~combine in
          ignore (spawn_now st t.thread t.guards t.kills (fun () -> continue k s))
      | effect (Emit (s, v)), k ->
          update_signal st s v;
          ignore (spawn_now st t.thread t.guards t.kills (fun () -> continue k ()));
          Tempo_log.log (log_ctx st) "signals" "state: %a"
            (Tempo_log.pp_any_signal_list ~brief:true) st.signals
      | effect (Await s), k ->
          Tempo_log.log ~task:t.t_id ~signal:s.s_id (log_ctx st) "tasks.await"
            "waiting for signal";
          let resume v =
            ignore (spawn_next st t.thread t.guards t.kills (fun () -> continue k v))
          in
          if s.present then
            match s.kind with
            | Event_signal -> resume (Option.get s.value)
            | Aggregate_signal _ -> s.awaiters <- resume :: s.awaiters
          else s.awaiters <- resume :: s.awaiters
      | effect (Await_immediate s), k ->
          Tempo_log.log ~task:t.t_id ~signal:s.s_id (log_ctx st)
            "tasks.await_immediate" "waiting for signal";
          if s.present then
            match s.value with
            | Some v ->
                ignore
                  (spawn_now st t.thread t.guards t.kills (fun () ->
                       continue k v))
            | None -> failwith "Error : present but no value"
          else
            let resume v =
              ignore (spawn_now st t.thread t.guards t.kills (fun () -> continue k v))
            in
            s.awaiters <- resume :: s.awaiters
      | effect Pause, k ->
          ignore (spawn_next st t.thread t.guards t.kills (fun () -> continue k ()));
          Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.pause"
            "rescheduled next instant"
      | effect (Fork p_child), k ->
          let child_thread = Tempo_thread.new_thread_id st in
          ignore (spawn_now st child_thread t.guards t.kills p_child);
          ignore
            (spawn_now st t.thread t.guards t.kills (fun () -> continue k child_thread))
      | effect (With_guard (s, body)), k ->
          let t' =
            spawn_now st t.thread (Any s :: t.guards) t.kills
              (fun () ->
                body (); (* body may perform an abort_kill*)
                if kills_alive t.kills then
                  ignore
                    (spawn_now st t.thread t.guards t.kills (fun () -> continue k ())))
          in
          Tempo_log.log ~task:t.t_id ~signal:s.s_id (log_ctx st) "tasks.guard"
            "updated as task %d, now guarded by signal %d" t'.t_id s.s_id;
          Tempo_log.log_guard ~task:t.t_id ~signal:s.s_id (log_ctx st)
            "task %d registered guard on signal %d" t'.t_id s.s_id;
          assert (kills_alive t'.kills);
          ()
      | effect (With_kill (kk, body)), k ->
          let continue_now () =
            kk.cleanup <- None;
            ignore (spawn_now st t.thread t.guards t.kills (fun () -> continue k ()))
          and continue_later () =
            kk.cleanup <- None;
            ignore (spawn_next st t.thread t.guards t.kills (fun () -> continue k ()))
          in
          kk.cleanup <- Some continue_later;
          let runner () =
            body ();
            continue_now ()
          in
          Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.watched"
            "updated as task %d, now watched by signal" t.t_id;
          ignore (spawn_now st t.thread t.guards (kk :: t.kills) runner)
      | effect (Join thread_id), k ->
          if thread_id = t.thread then invalid_arg "join: cannot join current thread";
          let resume () =
            ignore (spawn_now st t.thread t.guards t.kills (fun () -> continue k ()))
          in
          add_join_waiter st.threads thread_id resume
    in
    try Fun.protect ~finally:(fun () -> finish_task st.threads t.thread) run_task with
    | Aborted ->
        Tempo_log.log ~task:t.t_id (log_ctx st) "tasks" "aborted";
        ()

(* Scheduler main loop *)
let rec step : scheduler_state -> unit =
  fun st ->
    let counter = Mtime_clock.counter () in
    Tempo_log.log_banner (log_ctx st) "step"
      (Format.asprintf "Step %03d (Instant %03d)" st.debug.step_counter st.debug.instant_counter);
    st.debug.step_counter <- st.debug.step_counter + 1;
    if Queue.is_empty st.current then begin
      Tempo_log.log ~level:Logs.Debug (log_ctx st) "step" "queue empty, stopping current instant";
      let span = Mtime_clock.count counter in
      Tempo_log.log ~level:Logs.Debug (log_ctx st) "step" "completed in %a" Tempo_log.pp_span span
    end else begin
      let current_snapshot = Tempo_log.snapshot_queue st.current in
      Tempo_log.log_queue_state (log_ctx st) "queues" current_snapshot st.blocked st.next_instant;
      Tempo_log.log (log_ctx st) "signals" "state: %a" Tempo_log.pp_any_signal_list_full st.signals;
      let rec take_next () =
        if Queue.is_empty st.current then None
        else
          let t = Queue.take st.current in
          t.queued <- false;
          if kills_alive t.kills then Some t
          else (finish_task st.threads t.thread; take_next ())
      in
      match take_next () with
      | None ->
          Tempo_log.log ~level:Logs.Debug (log_ctx st) "step" "no runnable tasks";
          let span = Mtime_clock.count counter in
          Tempo_log.log ~level:Logs.Debug (log_ctx st) "step" "completed in %a" Tempo_log.pp_span span;
          Tempo_log.record_duration "step" span
      | Some t ->
          let remaining = Tempo_log.snapshot_queue st.current in
          Tempo_log.log ~task:t.t_id (log_ctx st) "step.select"
            "pick task | current=[%a] blocked=[%a] paused=[%a]"
            Tempo_log.pp_task_id_list_default remaining Tempo_log.pp_task_id_list_default st.blocked
            Tempo_log.pp_task_id_list_default st.next_instant;
          let continue =
            if not (guard_ok t.guards) then begin
              Tempo_log.log ~task:t.t_id (log_ctx st) "step.status" "guard not satisfied; blocking task";
              block_on_guards st t;
              let current_snapshot = Tempo_log.snapshot_queue st.current in
              Tempo_log.log_queue_state (log_ctx st) "queues" current_snapshot st.blocked st.next_instant;
              Tempo_log.log (log_ctx st) "signals" "state: %a" Tempo_log.pp_any_signal_list_full st.signals;
              fun () -> step st
            end else begin
              Tempo_log.log ~task:t.t_id (log_ctx st) "step.status" "guard satisfied; executing";
              Tempo_log.log_guard ~task:t.t_id (log_ctx st) "guards satisfied, running task";
              handle_task st t;
              let current_snapshot = Tempo_log.snapshot_queue st.current in
              Tempo_log.log_queue_state (log_ctx st) "queues" current_snapshot st.blocked st.next_instant;
              fun () -> step st
            end
          in
          let span = Mtime_clock.count counter in
          Tempo_log.log ~level:Logs.Debug (log_ctx st) "step" "completed in %a" Tempo_log.pp_span span;
          Tempo_log.record_duration "step" span;
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
    Tempo_log.log_info (log_ctx st) "instant" (Format.asprintf "Start Instant %d" st.debug.instant_counter);
    let counter = Mtime_clock.counter () in
    st.blocked <- [];
    let l = Tempo_log.snapshot_queue st.current in
    Tempo_log.log_queue_state (log_ctx st) "queues" l st.blocked st.next_instant;
    Tempo_log.log (log_ctx st) "signals" "state: %a" Tempo_log.pp_any_signal_list_full st.signals;
    before_step ();
    step st;
    after_step ();
    let span = Mtime_clock.count counter in
    Tempo_log.log ~level:Logs.Debug (log_ctx st) "instant" "duration %a" Tempo_log.pp_span span;
    Tempo_log.record_duration "instant" span;
    finalize_signals st;
    Tempo_log.log (log_ctx st) "instant" "prepare next instant";
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
            Tempo_log.log ~level:Logs.Debug (log_ctx st) "tasks" "no more tasks for next instant, terminating execution"
        | ts ->
          let survivors =
            List.filter
              (fun t ->
                 if kills_alive t.kills then true
                 else (finish_task st.threads t.thread; false))
              ts
          in
          Tempo_log.log ~level:Logs.Debug (log_ctx st) "instant" "ending instant, moving with %d tasks" (List.length survivors);
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
            task_counter    = 0;
            step_counter    = 0;
            instant_counter = 0; };
        threads         = Hashtbl.create 16;
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
