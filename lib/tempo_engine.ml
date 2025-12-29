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
    let ctx = log_ctx st in
    let run_task () =
      match t.run () with
      | () -> Tempo_log.log ~task:t.t_id  ctx "step" "task done | task=#%d thread=#%d" t.t_id t.thread 
      | effect (New_signal ()), k ->
          let s = fresh_event_signal st in
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
            "create event signal | task=#%d | signal=#%d" t.t_id s.s_id;
          continue k s
      | effect (New_signal_agg (initial, combine)), k ->
          let s = fresh_aggregate_signal st ~initial ~combine in
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
            "create aggregate signal | task=#%d signal=#%d" t.t_id s.s_id;
          continue k s
      | effect (Emit (s, v)), k ->
          Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
            "emit | task=#%d signal=#%d" t.t_id s.s_id;
          update_signal st s v;
          continue k ()
      | effect (Await s), k ->
          let resume v =
            let new_task =
              spawn_next st t.thread t.guards t.kills
                (fun () -> continue k v)
            in
            Tempo_log.log ~task:new_task.t_id ~signal:s.s_id ctx "step"
              "resume await | task=#%d -> task=#%d signal=#%d"
              t.t_id new_task.t_id s.s_id
          in
          if s.present then
            match s.kind with
            | Event_signal -> 
                  Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
                  "await present | task=#%d signal=#%d -> resume next instant" t.t_id s.s_id;
                resume (Option.get s.value)
            | Aggregate_signal _ -> s.awaiters <- resume :: s.awaiters
          else begin
            Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
              "await absent | task=#%d signal`#%d -> enqueue" t.t_id s.s_id;
            s.awaiters <- resume :: s.awaiters
          end;
      | effect (Await_immediate s), k ->
          if s.present then
            match s.value with
            | Some v ->
                Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
                  "await_immediate present | task=#%d signal`#%d -> resume now"
                  t.t_id s.s_id;
                continue k v;
            | None -> failwith "Error : present but no value"
          else
            let resume v =
              let new_task =
                spawn_now st t.thread t.guards t.kills
                  (fun () -> continue k v)
              in
              Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
                "resume await imm | task=#%d -> task=#%d signal`#%d" t.t_id new_task.t_id s.s_id
            in
            Tempo_log.log ~task:t.t_id ~signal:s.s_id ctx "step"
                "await immediate absent | task=#%d signal`#%d -> save continuation" t.t_id s.s_id;
                s.awaiters <- resume :: s.awaiters;
      | effect Pause, k ->
          let new_task =
            spawn_next st t.thread t.guards t.kills (fun () -> continue k ())
          in
          Tempo_log.log ~task:new_task.t_id ctx "step"
            "pause | task=#%d resume next instant as task #%d" t.t_id new_task.t_id;
      | effect (Fork p_child), k ->
          let child_thread = Tempo_thread.new_thread_id st in
          let t' = spawn_now st child_thread t.guards t.kills p_child in
          Tempo_log.log ~task:t.t_id ctx "step"
            "spawn logical thread=#%d as task=#%d" child_thread t'.t_id;
          continue k child_thread;
      | effect (With_guard (s, body)), k ->
          let guard_task =
            spawn_now st t.thread (Any s :: t.guards) t.kills
              (fun () ->
                body (); (* body may perform an abort_kill*)
                if kills_alive t.kills then begin
                  let t' = spawn_now st t.thread t.guards t.kills (fun () -> continue k ())
                  in  Tempo_log.log ~task:t.t_id ctx "step"
                    "with_guard exit | tasks=#%d -> task=#%d" t.t_id t'.t_id;
                end)
          in
          Tempo_log.log ~task:guard_task.t_id ~signal:s.s_id ctx "step"
            "with_guard enter | task=#%d signal=#%d -> schedule task=#%d" t.t_id s.s_id guard_task.t_id;
      | effect (With_kill (kk, body)), k ->
          let continue_now () =
            kk.cleanup <- None;
            let t' = spawn_now st t.thread t.guards t.kills (fun () -> continue k ()) in 
            Tempo_log.log ~task:t.t_id ctx "step"
            "with_kill exit | normal termination : task=#%d -> task=#%d" t.t_id t'.t_id;
          and continue_later () =
            kk.cleanup <- None;
            let t' = 
              spawn_next st t.thread t.guards t.kills (fun () -> continue k ()) in 
            Tempo_log.log ~task:t.t_id ctx "step"
            "with_kill | cancelation : task=#%d -> task #%d" t.t_id t'.t_id;
          in
          kk.cleanup <- Some continue_later;
          let runner () =
            body ();
            continue_now ()
          in
          let new_task =
            spawn_now st t.thread t.guards (kk :: t.kills) runner
          in
          Tempo_log.log ~task:new_task.t_id ctx "step"
            "with_kill enter | task=#%d -> task=#%d" t.t_id new_task.t_id;
      | effect (Join thread_id), k ->
          if thread_id = t.thread then invalid_arg "join: cannot join current thread";
          let resume () =
            let t' = 
              spawn_now st t.thread t.guards t.kills (fun () -> continue k ()) in
            Tempo_log.log ~task:t.t_id ctx "step.join"
            "join resume | waiter task=#%d on task=#%d as task =#%d" t.t_id thread_id t'.t_id;
          in
          let state = Tempo_thread.find st.threads thread_id in
          if state.completed && state.active = 0 then resume ()
          else begin
            Tempo_log.log ~task:t.t_id ctx "step"
              "joint wait | task=#%d waiting for thread=#%d" t.t_id thread_id;
            if not (List.exists (fun (w, tgt) -> w = t.thread && tgt = thread_id) st.waiting)
            then st.waiting <- (t.thread, thread_id) :: st.waiting;
            add_join_waiter st.threads thread_id resume
          end
    in
    let cleanup () =
      finish_task st.threads t.thread
    in
    try Fun.protect ~finally:cleanup run_task with
    | Aborted -> ()

  let rec step : scheduler_state -> unit =
  fun st ->
    let counter = Mtime_clock.counter () in
    let ctx = log_ctx st in
    Tempo_log.log_banner_step ctx;
    Tempo_log.log_snapshot ctx
      ~current:(Tempo_log.snapshot_queue st.current)
      ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals;
    st.debug.step_counter <- st.debug.step_counter + 1;
      let rec take_next () =
        if Queue.is_empty st.current then 
          begin 
            Tempo_log.log ~level:Logs.Debug ctx "step" "queue empty | stop current instant";
            let span = Mtime_clock.count counter in
            Tempo_log.log_step_summary ctx span;
            Tempo_log.record_duration "step" span;
            None
          end
        else
          let t = Queue.take st.current in
          t.queued <- false;
          if kills_alive t.kills then Some t
          else (
            finish_task st.threads t.thread;
            take_next ())
      in
      match take_next () with
      | None ->
          let span = Mtime_clock.count counter in
          Tempo_log.log_step_summary ctx span;
          Tempo_log.record_duration "step" span;
      | Some t ->
          Tempo_log.log_pick ctx t;
          let continue =
            if not (guard_ok t.guards) then begin
              let missing = Tempo_signal.missing_guards t.guards in
              Tempo_log.log_block ctx t missing;
              block_on_guards st t;
              fun () -> step st
            end else begin
              handle_task st t;
              fun () -> step st
            end
          in
          let span = Mtime_clock.count counter in
          Tempo_log.log_snapshot ctx
            ~current:(Tempo_log.snapshot_queue st.current)
            ~blocked:st.blocked ~next:st.next_instant ~signals:st.signals;
          Tempo_log.log_step_summary ctx span;
          Tempo_log.record_duration "step" span;
          continue ()

let rec run_instant : (unit -> unit) -> (unit -> unit) -> 
    scheduler_state -> int option -> unit =
  fun before_step after_step  st remaining ->
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
            Tempo_log.log ~level:Logs.Debug ctx "tasks" "no more tasks | for next instant -> stop"
        | ts ->
          let survivors =
            List.filter
              (fun t ->
                 if kills_alive t.kills then true
                else (
                  finish_task st.threads t.thread;
                  false))
              ts
          in
          Tempo_log.log ctx "instant" "rollover tasks | moving %d tasks to next instant"
            (List.length survivors);
          st.next_instant <- [];
          List.iter (enqueue_now st) survivors;
          run_instant before_step after_step st (Option.map pred remaining)

let create_scheduler_state () = 
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
    ;threads         = Hashtbl.create 16
    ;waiting         = []
  }

let execute ?instants ?(input = fun () -> None) ?(output = fun _ -> ()) initial =
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
  ignore (spawn_now st thread [] [] (fun () -> initial input_signal output_signal));
  Tempo_log.log_banner (log_ctx st) "execute" "runtime start | schedule initial task=#%d";
  run_instant before_step after_step st instants;
  Tempo_log.log_duration_summary ()
