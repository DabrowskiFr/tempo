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

(* Allow signal helpers to annotate the current step stats when available. *)
let current_step_stats : Tempo_log.step_stats option ref = ref None

let mark_emit () =
  match !current_step_stats with
  | Some stats -> stats.last_event <- Some "emit"
  | None -> ()

let update_signal st s v =
  mark_emit ();
  Tempo_signal.update_signal st s v

let emit_event_from_host st s v =
  mark_emit ();
  Tempo_signal.emit_event_from_host st s v

let handle_task : scheduler_state -> Tempo_log.step_stats -> task -> unit =
  fun st stats t ->
    let set_event e = stats.last_event <- Some e in
    let run_task () =
      let inc_now () = stats.spawns_now <- stats.spawns_now + 1
      and inc_next () = stats.spawns_next <- stats.spawns_next + 1 in
      match t.run () with
      | () ->
          if stats.last_event = None then set_event "complete"
      | effect (New_signal (_)), k ->
          let s = fresh_event_signal st in
          inc_now ();
          spawn_now st t.thread t.guards t.kills (fun () -> continue k s);
          set_event "new_signal"
      | effect (New_signal_agg (initial, combine)), k ->
          let s = fresh_aggregate_signal st ~initial ~combine in
          inc_now ();
          spawn_now st t.thread t.guards t.kills (fun () -> continue k s);
          set_event "new_signal_agg"
      | effect (Emit (s, v)), k ->
          update_signal st s v;
          inc_now ();
          spawn_now st t.thread t.guards t.kills (fun () -> continue k ());
          set_event "emit"
      | effect (Await s), k ->
          let resume v =
            inc_next ();
            spawn_next st t.thread t.guards t.kills (fun () -> continue k v)
          in
          if s.present then
            match s.kind with
            | Event_signal -> resume (Option.get s.value)
            | Aggregate_signal _ -> s.awaiters <- resume :: s.awaiters
          else s.awaiters <- resume :: s.awaiters;
          set_event "await"
      | effect (Await_immediate s), k ->
          if s.present then
            match s.value with
            | Some v ->
                inc_now ();
                spawn_now st t.thread t.guards t.kills (fun () -> continue k v);
                set_event "await_immediate"
            | None -> failwith "Error : present but no value"
          else
            let resume v =
              inc_now ();
              spawn_now st t.thread t.guards t.kills (fun () -> continue k v)
            in
            s.awaiters <- resume :: s.awaiters;
            set_event "await_immediate"
      | effect Pause, k ->
          inc_next ();
          spawn_next st t.thread t.guards t.kills (fun () -> continue k ());
          set_event "pause"
      | effect (Fork p_child), k ->
          let child_thread = Tempo_thread.new_thread_id st in
          inc_now ();
          spawn_now st child_thread t.guards t.kills p_child;
          inc_now ();
          spawn_now st t.thread t.guards t.kills (fun () -> continue k child_thread);
          set_event "fork"
      | effect (With_guard (s, body)), k ->
          inc_now ();
          spawn_now st t.thread (Any s :: t.guards) t.kills
            (fun () ->
              body (); (* body may perform an abort_kill*)
              if kills_alive t.kills then begin
                inc_now ();
                spawn_now st t.thread t.guards t.kills (fun () -> continue k ())
              end);
          set_event "with_guard"
      | effect (With_kill (kk, body)), k ->
          let continue_now () =
            kk.cleanup <- None;
            inc_now ();
            spawn_now st t.thread t.guards t.kills (fun () -> continue k ())
          and continue_later () =
            kk.cleanup <- None;
            inc_next ();
            spawn_next st t.thread t.guards t.kills (fun () -> continue k ())
          in
          kk.cleanup <- Some continue_later;
          let runner () =
            body ();
            continue_now ()
          in
          inc_now ();
          spawn_now st t.thread t.guards (kk :: t.kills) runner;
          set_event "with_kill"
      | effect (Join thread_id), k ->
          if thread_id = t.thread then invalid_arg "join: cannot join current thread";
          let resume () =
            inc_now ();
            spawn_now st t.thread t.guards t.kills (fun () -> continue k ())
          in
          add_join_waiter st.threads thread_id resume;
          set_event "join"
    in
    try Fun.protect ~finally:(fun () -> finish_task st.threads t.thread) run_task with
    | Aborted ->
        stats.aborted <- stats.aborted + 1;
        set_event "aborted"

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
      stats.last_event <- Some "idle";
      Tempo_log.log ~level:Logs.Debug ctx "step" "queue empty, stopping current instant";
      let span = Mtime_clock.count counter in
      Tempo_log.log_step_summary ctx stats span;
      Tempo_log.record_duration "step" span;
      current_step_stats := None
    end else begin
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
          stats.last_event <- Some "idle";
          let span = Mtime_clock.count counter in
          Tempo_log.log_step_summary ctx stats span;
          Tempo_log.record_duration "step" span
      | Some t ->
          Tempo_log.log_pick ctx t;
          let continue =
            if not (guard_ok t.guards) then begin
              let missing = Tempo_signal.missing_guards t.guards in
              Tempo_log.log_block ctx t missing;
              stats.blocks <- stats.blocks + 1;
              stats.last_event <- Some "block";
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
                else (finish_task st.threads t.thread; false))
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
