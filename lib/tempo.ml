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

open Effect
open Effect.Deep
open Tempo_types

(* -------------------------------------------------------------------------- *)
(* Types and aliases                                                          *)
(* -------------------------------------------------------------------------- *)

let () = Tempo_log.init ()

type event = Tempo_types.event

type aggregate = Tempo_types.aggregate

type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_types.signal_core

type 'a signal = ('a, 'a, event) signal_core

type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core

type kill = Tempo_types.kill

type thread = Tempo_types.thread

type thread_state = Tempo_types.thread_state

(* -------------------------------------------------------------------------- *)
(* Scheduler state                                                            *)
(* -------------------------------------------------------------------------- *)

(* structure for debug purpose *)

type debug_info =
  { mutable sig_counter     : int
  ; mutable task_counter    : int
  ; mutable step_counter    : int
  ; mutable instant_counter : int
  }

 (* structure represents the scheduler’s state 
  - current : task scheduled for the ongoing instant
  - next_instant : tasks scheduled to start in the next instant
  - blocked : tasks blocked on guards
  - signals : registry of all signals
  - thread_counter : allocator for thread id
  - threads : per-thread state
 *) 
type scheduler_state =
  { current                : task Queue.t
  ; mutable next_instant   : task list
  ; mutable blocked        : task list 
  ; mutable signals        : any_signal list
  ; mutable thread_counter : int
  ; mutable thread_dense   : thread_state option array
  ; threads                : (thread, thread_state) Hashtbl.t
  ; debug                  : debug_info
  }

let log_ctx st =
  Tempo_log.context ~instant:st.debug.instant_counter ~step:st.debug.step_counter


module Thread_store = struct
  let ensure_dense_capacity st thread =
    let len = Array.length st.thread_dense in
    if thread >= len then (
      let new_len = ref len in
      while thread >= !new_len do
        new_len := max 1 (!new_len * 2)
      done;
      let grown = Array.make !new_len None in
      Array.blit st.thread_dense 0 grown 0 len;
      st.thread_dense <- grown)

  let ensure st thread =
    ensure_dense_capacity st thread;
    match st.thread_dense.(thread) with
    | Some ts -> ts
    | None ->
        let ts = { active = 0; completed = false; waiters = [] } in
        st.thread_dense.(thread) <- Some ts;
        Hashtbl.replace st.threads thread ts;
        ts

  let find st thread =
    if thread < Array.length st.thread_dense then
      match st.thread_dense.(thread) with
      | Some ts -> ts
      | None -> invalid_arg (Printf.sprintf "unknown thread %d" thread)
    else
      invalid_arg (Printf.sprintf "unknown thread %d" thread)

  let fresh_id st =
    let id = st.thread_counter in
    st.thread_counter <- id + 1;
    ensure_dense_capacity st id;
    if Option.is_none st.thread_dense.(id) then (
      let ts = { active = 0; completed = false; waiters = [] } in
      st.thread_dense.(id) <- Some ts;
      Hashtbl.replace st.threads id ts);
    id
end

let ensure_thread_state = Thread_store.ensure
let find_thread_state = Thread_store.find
let new_thread_id = Thread_store.fresh_id


(* -------------------------------------------------------------------------- *)
(* Thread bookkeeping                                                         *)
(* -------------------------------------------------------------------------- *)

(* records a new task for a thread *)
let register_task_state state =
  if state.completed && state.active = 0 then state.completed <- false;
  state.active <- state.active + 1

let finish_task_state state =
  let rec resume_waiters waiters =
    match waiters with
    | [] -> ()
    | f :: rest ->
        f ();
        resume_waiters rest
  in
  state.active <- state.active - 1;
  if state.active = 0 then begin
    state.completed <- true;
    let waiters = List.rev state.waiters in
    state.waiters <- [];
    resume_waiters waiters
  end

let add_join_waiter st thread waiter =
  let state = find_thread_state st thread in
  if state.completed then waiter ()
  else state.waiters <- waiter :: state.waiters

module Join_many = struct
  let wait_for_threads st ~(current_thread : thread) ~(resume_parent : unit -> unit)
      (thread_ids : thread list) : unit =
    match thread_ids with
    | [] ->
        resume_parent ()
    | [ tid ] ->
        if tid = current_thread then invalid_arg "join_many: cannot join current thread";
        let state = find_thread_state st tid in
        if state.completed then resume_parent ()
        else state.waiters <- resume_parent :: state.waiters
    | _ ->
        let remaining = ref 0 in
        let resumed = ref false in
        let try_resume () =
          if not !resumed then (
            decr remaining;
            if !remaining = 0 then (
              resumed := true;
              resume_parent ()))
        in
        let rec register = function
          | [] -> ()
          | tid :: rest ->
              if tid = current_thread then
                invalid_arg "join_many: cannot join current thread";
              let state = find_thread_state st tid in
              if not state.completed then (
                incr remaining;
                state.waiters <- try_resume :: state.waiters);
              register rest
        in
        register thread_ids;
        if !remaining = 0 then resume_parent ()
end

(**********************************************************)
(* Scheduler functions                                    *)
(**********************************************************)

(* -------------------------------------------------------------------------- *)
(* Signal registry                                                            *)
(* -------------------------------------------------------------------------- *)

(** Reset all signals for a new instant.
    The status becomes absent and the value is undefined.
    Awaiters are left unchanged because a process can wait 
    for a signal for many instants Guarded awaiters are 
    cleared *)


let register_signal : scheduler_state -> ('emit, 'agg, 'mode) signal_core -> unit =
  fun st s -> 
    st.signals <- (Any s) :: st.signals

let create_event_signal st =
  let id = st.debug.sig_counter in
  st.debug.sig_counter <- id + 1;
  let s =
    { s_id = id
    ; present = false
    ; value = None
    ; awaiters = Stack.create ()
    ; guard_waiters = Stack.create ()
    ; kind = Event_signal
    }
  in
  register_signal st s;
  Tempo_log.log ~signal:s.s_id (log_ctx st) "signals"
    "created IO signal %d" s.s_id;
  s

let finalize_signals : scheduler_state -> unit =
  fun st ->
    List.iter
      (fun (Any s) ->
         (match s.kind with
          | Aggregate_signal _ when s.present ->
              let delivered =
                match s.value with
                | Some value -> value
                | None -> failwith "aggregate signal flagged present but no value"
              in
              while not (Stack.is_empty s.awaiters) do
                let resume = Stack.pop s.awaiters in
                resume delivered
              done
          | _ -> ());
         s.present <- false;
         s.value <- None;
         Stack.clear s.guard_waiters)
      st.signals




(* -------------------------------------------------------------------------- *)
(* Guard helpers                                                              *)
(* -------------------------------------------------------------------------- *)

module Task_fastpath = struct
  let kills_alive kills =
    List.for_all (fun k -> !(k.alive)) kills

  let task_kills_alive (t : task) : bool =
    if t.has_kills then kills_alive t.kills else true

  let guard_ok : any_signal list -> bool = function
    | guards -> List.for_all (fun (Any s) -> s.present) guards

  let task_guard_ok (t : task) : bool =
    match t.guard_single with
    | Some (Any s) -> s.present
    | None -> guard_ok t.guards
end

let task_kills_alive = Task_fastpath.task_kills_alive
let task_guard_ok = Task_fastpath.task_guard_ok

(** [missing_guards signals] : returns the signals in [signals] that are not present. 
    @param guards a list of signals
    @return the sublist of signals that are absent
  *)
module Task_builder = struct
  let guard_single (guards : any_signal list) : any_signal option =
    match guards with
    | [ g ] -> Some g
    | _ -> None

  (* Create a new task while registering ownership in thread state. *)
  let mk_task_with_state :
      scheduler_state ->
      thread ->
      thread_state ->
      any_signal list ->
      kill list ->
      (unit -> unit) ->
      task =
   fun st thread thread_state guards kills run ->
    let t_id = st.debug.task_counter in
    st.debug.task_counter <- st.debug.task_counter + 1;
    register_task_state thread_state;
    { t_id
    ; thread
    ; thread_state
    ; guards
    ; guard_single = guard_single guards
    ; kills
    ; has_kills = (kills <> [])
    ; run
    ; queued = false
    ; blocked = false
    }

  let mk_task :
      scheduler_state -> thread -> any_signal list -> kill list -> (unit -> unit) -> task =
   fun st thread guards kills run ->
    let thread_state = ensure_thread_state st thread in
    mk_task_with_state st thread thread_state guards kills run
end

let mk_task_with_state = Task_builder.mk_task_with_state
let mk_task = Task_builder.mk_task

(* Enqueue a task to be scheduled in the current instant *)
let enqueue_now : scheduler_state -> task -> unit =
  fun st t ->
    if not t.queued then
        begin 
          if Tempo_log.level_enabled Logs.Debug then
            Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.current"
              "push task %a" (Tempo_log.pp_task ~brief:false) t;
          t.queued <- true; 
          t.blocked <- false; 
          Queue.add t st.current 
        end

(* Enqueue a task to be scheduled in the next instant *)
let enqueue_next : scheduler_state -> task -> unit = 
  fun st t -> 
        if task_kills_alive t then begin
          let debug_enabled = Tempo_log.level_enabled Logs.Debug in
          if debug_enabled then
            Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.next"
              "schedule task for next instant";
          st.next_instant <- t::st.next_instant;
          if debug_enabled then
            Tempo_log.log (log_ctx st) "tasks.next" "pending next instant: %a"
              Tempo_log.pp_task_list_brief st.next_instant
        end else
          finish_task_state t.thread_state

module Instant_rollover = struct
  let schedule_unblocked_for_next (st : scheduler_state) : unit =
    List.iter
      (fun (t : task) ->
        if t.blocked then (
          t.blocked <- false;
          enqueue_next st t))
      st.blocked

  let drain_survivors_for_next_instant (st : scheduler_state) : task list =
    match st.next_instant with
    | [] -> []
    | ts ->
        st.next_instant <- [];
        List.rev
          (List.filter
            (fun t ->
              if task_kills_alive t then true
              else (
                finish_task_state t.thread_state;
                false))
            ts)
end

module Guard_waiters = struct
  let register_missing_guards (t : task) (guards : any_signal list) : any_signal list =
    let rec loop acc = function
      | [] -> acc
      | ((Any s) as g) :: rest ->
          if s.present then loop acc rest
          else (
            Stack.push t s.guard_waiters;
            loop (g :: acc) rest)
    in
    loop [] guards

  (* Block a task on its unsatified guards *)
  let block_on_guards : scheduler_state -> task -> unit =
   fun st t ->
    if task_kills_alive t then begin
      if t.blocked then (
        (* The task is already registered on the missing guards of this instant.
           Signals only move from absent to present, so missing guards can only shrink. *)
        ())
      else begin
        t.blocked <- true;
        st.blocked <- t :: st.blocked;
        let miss_rev = register_missing_guards t t.guards in
        let need_detail_logs =
          Tempo_log.trace_guards || Tempo_log.level_enabled Logs.Debug
        in
        if need_detail_logs then (
          let ctx = log_ctx st in
          let miss = List.rev miss_rev in
          Tempo_log.log ~task:t.t_id ctx "tasks.block"
            "blocked on guards %a" (Tempo_log.pp_any_guard_list ~brief:false) miss;
          Tempo_log.log_guard ~task:t.t_id ctx
            "waiting for guards %a" (Tempo_log.pp_any_guard_list ~brief:false) miss)
      end
    end else
      finish_task_state t.thread_state

  (* Given a signal s, wake up all tasks waiting on the guard s
     if the guard is satisfied, otherwise block the task on its missing guards *)
  let wake_guard_waiters :
      scheduler_state -> ('emit, 'agg, 'mode) signal_core -> unit =
   fun st s ->
    let ws = s.guard_waiters in
    let ws_len = Stack.length ws in
    if Tempo_log.trace_guards && ws_len <> 0 then
      Tempo_log.log_guard ~signal:s.s_id (log_ctx st)
        "signal %d waking %d guard waiters" s.s_id ws_len;
    while not (Stack.is_empty ws) do
      let t = Stack.pop ws in
      if task_guard_ok t then enqueue_now st t else block_on_guards st t
    done
end

let block_on_guards = Guard_waiters.block_on_guards
let wake_guard_waiters = Guard_waiters.wake_guard_waiters

(* -------------------------------------------------------------------------- *)
(* Effect handler                                                             *)
(* -------------------------------------------------------------------------- *)

let emit_event_from_host :
    type a. scheduler_state -> (a, a, event) signal_core -> a -> unit =
 fun st s value ->
  if s.present then invalid_arg "Emit : multiple emission";
  s.present <- true;
  s.value <- Some value;
  while not (Stack.is_empty s.awaiters) do
    let resume = Stack.pop s.awaiters in
    resume value
  done;
  wake_guard_waiters st s

(* Handle a task by performing its effect *)

module Effects_handler = struct
let run : scheduler_state -> task -> unit =
  fun st t ->
   let debug_enabled = Tempo_log.level_enabled Logs.Debug in
   let guard_trace = Tempo_log.trace_guards in
   let ctx = lazy (log_ctx st) in
   let debug_log ?task ?signal scope fmt =
     if debug_enabled then
       Tempo_log.log ?task ?signal (Lazy.force ctx) scope fmt
     else
       Format.ifprintf Format.std_formatter fmt
   in
   let guard_log ?task ?signal fmt =
     if guard_trace then
       Tempo_log.log_guard ?task ?signal (Lazy.force ctx) fmt
     else
       Format.ifprintf Format.std_formatter fmt
   in
   let run_task () =
     match t.run () with 
        | () -> ()
        | effect (New_signal (_)), k -> 
            let id = st.debug.sig_counter in 
              st.debug.sig_counter <- id + 1;
              let s = 
                { s_id = id;
                  present = false; value = None;
                  awaiters = Stack.create (); guard_waiters = Stack.create ();
                  kind = Event_signal }
              in 
                register_signal st s;
                debug_log ~signal:s.s_id "signals" "created new signal %a"
                  (Tempo_log.pp_signal ~brief:false) s;
                enqueue_now st
                  (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                     (fun () -> continue k s))
        | effect (New_signal_agg (initial, combine)), k ->
            let id = st.debug.sig_counter in
            st.debug.sig_counter <- id + 1;
            let s =
              { s_id = id
              ; present = false
              ; value = None
              ; awaiters = Stack.create ()
              ; guard_waiters = Stack.create ()
              ; kind = Aggregate_signal { combine; initial }
              }
            in
            register_signal st s;
            debug_log ~signal:s.s_id "signals"
              "created new aggregate signal %a" (Tempo_log.pp_signal ~brief:false) s;
            enqueue_now st
              (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                 (fun () -> continue k s))
        | effect (Emit (s, v)), k  ->
            begin match s.kind with
            | Event_signal ->
                if s.present then invalid_arg "Emit : multiple emission";
                s.present <- true;
                s.value <- Some v;
                while not (Stack.is_empty s.awaiters) do
                  let resume = Stack.pop s.awaiters in
                  resume v
                done
            | Aggregate_signal { combine; initial } ->
                let acc =
                  match s.value with
                  | None -> combine initial v
                  | Some agg -> combine agg v
                in
                s.present <- true;
                s.value <- Some acc
            end;
            wake_guard_waiters st s;
            enqueue_now st
            (mk_task_with_state st t.thread t.thread_state t.guards t.kills
               (fun () -> continue k ()))
        | effect (Await s), k ->
            let enqueue_resume v =
              let t' =
                mk_task_with_state st t.thread t.thread_state t.guards t.kills
                  (fun () -> continue k v)
              in
              debug_log ~task:t'.t_id ~signal:s.s_id "tasks.await"
                "signal already present, resume next instant as task %d" t'.t_id;
              enqueue_next st t'
            in
            let register_waiter () =
              let resume v =
                let t' =
                  mk_task_with_state st t.thread t.thread_state t.guards t.kills
                    (fun () -> continue k v)
                in
                debug_log ~task:t'.t_id ~signal:s.s_id "tasks.await"
                  "resume task next instant (task %d)" t'.t_id;
                enqueue_next st t'
              in
              Stack.push resume s.awaiters
            in
            if s.present then
              match s.kind with
              | Event_signal ->
                  (match s.value with
                   | Some v -> enqueue_resume v
                   | None -> failwith "Error : present but no value")
              | Aggregate_signal _ ->
                  register_waiter ()
            else
              register_waiter ()
        | effect (Await_immediate s), k ->
            if s.present then
              match s.value with
              | Some v ->
                  let t' =
                    mk_task_with_state st t.thread t.thread_state t.guards t.kills
                      (fun () -> continue k v)
                  in
                  enqueue_now st t'
              | None -> failwith "Error : present but no value"
            else
              let resume v =
                let t =
                  mk_task_with_state st t.thread t.thread_state t.guards t.kills
                    (fun () -> continue k v)
                in
                enqueue_now st t
              in
              Stack.push resume s.awaiters
        | effect Pause, k -> 
            let t' =
              mk_task_with_state st t.thread t.thread_state t.guards t.kills
                (fun () -> continue k ())
            in
            debug_log ~task:t.t_id "tasks.pause"
              "rescheduled as task %d next instant" t'.t_id;
            enqueue_next st t'
        | effect (Fork p_child), k -> 
              let child_thread = new_thread_id st in
              enqueue_now st (mk_task st child_thread t.guards t.kills p_child);
              enqueue_now st
                (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                   (fun () -> continue k child_thread))
        | effect (With_guard (s, body)), k -> 
           let t' = 
            mk_task_with_state st t.thread t.thread_state (Any s :: t.guards) t.kills
            (
              fun () -> 
                body ();
                enqueue_now st
                  (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                     (fun () -> continue k ()))
              )
            in 
              debug_log ~task:t.t_id ~signal:s.s_id "tasks.guard"
                "updated as task %d, now guarded by signal %d" t'.t_id s.s_id;
              guard_log ~task:t.t_id ~signal:s.s_id
                "task %d registered guard on signal %d" t'.t_id s.s_id;
              enqueue_now st t'
        | effect (With_kill (kk, body)), k ->
            let continue_now () =
              kk.cleanup <- None;
              enqueue_now st
                (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                   (fun () -> continue k ()))
            and continue_later () =
              kk.cleanup <- None;
              enqueue_next st
                (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                   (fun () -> continue k ()))
            in
            kk.cleanup <- Some continue_later;
            let runner () =
              body ();
              continue_now ()
            in
              debug_log ~task:t.t_id "tasks.watched"
              "updated as task %d, now watched by signal" t.t_id;
            let t' =
              mk_task_with_state st t.thread t.thread_state t.guards (kk :: t.kills)
                runner
            in
            enqueue_now st t'
        | effect (Join thread_id), k ->
            if thread_id = t.thread then invalid_arg "join: cannot join current thread";
            let resume () =
              enqueue_now st
                (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                   (fun () -> continue k ()))
            in
            add_join_waiter st thread_id resume
        | effect (Join_many thread_ids), k ->
            let resume_parent () =
              enqueue_now st
                (mk_task_with_state st t.thread t.thread_state t.guards t.kills
                   (fun () -> continue k ()))
            in
            Join_many.wait_for_threads st ~current_thread:t.thread ~resume_parent
              thread_ids
   in
   try Fun.protect ~finally:(fun () -> finish_task_state t.thread_state) run_task
   with
   | Aborted ->
       debug_log ~task:t.t_id "tasks" "aborted";
       ()
end

let handle_task = Effects_handler.run

(* -------------------------------------------------------------------------- *)
(* Scheduler loop                                                             *)
(* -------------------------------------------------------------------------- *)

(* Scheduler main loop *)
module Scheduler_step = struct
let rec step : scheduler_state -> unit =
  fun st ->
    let measure = Tempo_log.metrics_enabled in
    let counter = if measure then Some (Mtime_clock.counter ()) else None in
    let ctx = log_ctx st in
    let debug_enabled = Tempo_log.level_enabled Logs.Debug in
    let info_enabled = Tempo_log.level_enabled Logs.Info in
    if info_enabled then
      Tempo_log.log ~level:Logs.Info ctx "step" "Step %03d (Instant %03d)"
        st.debug.step_counter st.debug.instant_counter;
    st.debug.step_counter <- st.debug.step_counter + 1;
    if Queue.is_empty st.current then begin
      Tempo_log.log ~level:Logs.Debug ctx "step" "queue empty, stopping current instant";
      (match counter with
       | None -> ()
       | Some c ->
           let span = Mtime_clock.count c in
           Tempo_log.log ~level:Logs.Debug ctx "step" "completed in %a" Tempo_log.pp_span span)
    end else begin
      if debug_enabled then (
        let current_snapshot = Tempo_log.snapshot_queue st.current in
        Tempo_log.log_queue_state ctx "queues" current_snapshot st.blocked st.next_instant;
        Tempo_log.log ctx "signals" "state: %a" Tempo_log.pp_any_signal_list_full st.signals);
      let rec take_next () =
        if Queue.is_empty st.current then None
        else
          let t = Queue.take st.current in
          t.queued <- false;
          if task_kills_alive t then Some t
          else (finish_task_state t.thread_state; take_next ())
      in
      match take_next () with
      | None ->
          Tempo_log.log ~level:Logs.Debug ctx "step" "no runnable tasks";
          (match counter with
           | None -> ()
           | Some c ->
               let span = Mtime_clock.count c in
               Tempo_log.log ~level:Logs.Debug ctx "step" "completed in %a" Tempo_log.pp_span span;
               Tempo_log.record_duration "step" span)
      | Some t ->
          if debug_enabled then (
            let remaining = Tempo_log.snapshot_queue st.current in
            Tempo_log.log ~task:t.t_id ctx "step.select"
              "pick task | current=[%a] blocked=[%a] paused=[%a]"
              Tempo_log.pp_task_id_list_default remaining Tempo_log.pp_task_id_list_default st.blocked
              Tempo_log.pp_task_id_list_default st.next_instant);
          let continue =
            if not (task_guard_ok t) then begin
              Tempo_log.log ~task:t.t_id ctx "step.status" "guard not satisfied; blocking task";
              block_on_guards st t;
              if debug_enabled then (
                let current_snapshot = Tempo_log.snapshot_queue st.current in
                Tempo_log.log_queue_state ctx "queues" current_snapshot st.blocked st.next_instant;
                Tempo_log.log ctx "signals" "state: %a" Tempo_log.pp_any_signal_list_full st.signals);
              fun () -> step st
            end else begin
              Tempo_log.log ~task:t.t_id ctx "step.status" "guard satisfied; executing";
              Tempo_log.log_guard ~task:t.t_id ctx "guards satisfied, running task";
              handle_task st t;
              if debug_enabled then (
                let current_snapshot = Tempo_log.snapshot_queue st.current in
                Tempo_log.log_queue_state ctx "queues" current_snapshot st.blocked st.next_instant);
              fun () -> step st
            end
          in
          (match counter with
           | None -> ()
           | Some c ->
               let span = Mtime_clock.count c in
               Tempo_log.log ~level:Logs.Debug ctx "step" "completed in %a" Tempo_log.pp_span span;
               Tempo_log.record_duration "step" span);
          continue ()
    end

let rec run_instant :
      ?before_step:(unit -> unit) ->
      ?after_step:(unit -> unit) ->
      ?on_instant_end:(scheduler_state -> unit) ->
      scheduler_state -> int option -> unit =
  fun ?(before_step = fun () -> ()) ?(after_step = fun () -> ())
      ?(on_instant_end = fun _ -> ()) st remaining ->
    match remaining with
    | Some n when n <= 0 -> ()
    | _ ->
        let ctx = log_ctx st in
        let debug_enabled = Tempo_log.level_enabled Logs.Debug in
        let measure = Tempo_log.metrics_enabled in
        Tempo_log.log ~level:Logs.Info ctx "instant" "Start Instant %d"
          st.debug.instant_counter;
        let counter = if measure then Some (Mtime_clock.counter ()) else None in
        st.blocked <- [];
        if debug_enabled then (
          let l = Tempo_log.snapshot_queue st.current in
          Tempo_log.log_queue_state ctx "queues" l st.blocked st.next_instant;
          Tempo_log.log ctx "signals" "state: %a"
            Tempo_log.pp_any_signal_list_full st.signals);
        before_step ();
        step st;
        after_step ();
        (match counter with
         | None -> ()
         | Some c ->
             let span = Mtime_clock.count c in
             Tempo_log.log ~level:Logs.Debug ctx "instant" "duration %a"
               Tempo_log.pp_span span;
             Tempo_log.record_duration "instant" span);
        finalize_signals st;
        on_instant_end st;
        Tempo_log.log ctx "instant" "prepare next instant";
        Instant_rollover.schedule_unblocked_for_next st;
        st.debug.instant_counter <- st.debug.instant_counter + 1;
        st.debug.step_counter <- 0;
        let survivors = Instant_rollover.drain_survivors_for_next_instant st in
        match survivors with
        | [] ->
            Tempo_log.log ~level:Logs.Debug ctx "tasks"
              "no more tasks for next instant, terminating execution"
        | _ ->
            Tempo_log.log ~level:Logs.Debug ctx "instant"
              "ending instant, moving with %d tasks" (List.length survivors);
            List.iter (enqueue_now st) survivors;
            run_instant ~before_step ~after_step ~on_instant_end st
              (Option.map pred remaining)
end

let run_instant = Scheduler_step.run_instant

(* -------------------------------------------------------------------------- *)
(* Public API entry point                                                     *)
(* -------------------------------------------------------------------------- *)

module Runtime_bootstrap = struct
  let create_state () : scheduler_state =
    {
      current = Queue.create ();
      next_instant = [];
      blocked = [];
      signals = [];
      thread_counter = 0;
      thread_dense = [||];
      debug =
        { sig_counter = 0; task_counter = 0; step_counter = 0; instant_counter = 0 };
      threads = Hashtbl.create 16;
    }

  let create_io_hooks st ~(input : unit -> 'i option) ~(output : 'o -> unit) :
      ((unit -> unit) * (unit -> unit) * ('i, 'i, event) signal_core * ('o, 'o, event) signal_core) =
    let input_signal = create_event_signal st in
    let output_signal = create_event_signal st in
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
    (before_step, after_step, input_signal, output_signal)

  let enqueue_root st main input_signal output_signal =
    let thread = new_thread_id st in
    let root = mk_task st thread [] [] (fun () -> main input_signal output_signal) in
    enqueue_now st root
end

let execute ?instants ?(input = fun () -> None) ?(output = fun _ -> ()) initial =
  let st = Runtime_bootstrap.create_state () in
  Tempo_log.log_banner (log_ctx st) "run" "Runtime Start";
  Tempo_log.log (log_ctx st) "run" "push initial tasks...";
  let before_step, after_step, input_signal, output_signal =
    Runtime_bootstrap.create_io_hooks st ~input ~output
  in
  Runtime_bootstrap.enqueue_root st initial input_signal output_signal;
  run_instant ~before_step ~after_step st instants;
  Tempo_log.log_duration_summary ()

type inspector_snapshot = {
  instant : int;
  current_tasks : int;
  blocked_tasks : int;
  next_tasks : int;
  signal_count : int;
}

let execute_inspect ?instants ?(input = fun () -> None) ?(output = fun _ -> ())
    ~on_instant initial =
  let st = Runtime_bootstrap.create_state () in
  let before_step, after_step, input_signal, output_signal =
    Runtime_bootstrap.create_io_hooks st ~input ~output
  in
  let on_instant_end st =
    on_instant
      {
        instant = st.debug.instant_counter;
        current_tasks = Queue.length st.current;
        blocked_tasks = List.length st.blocked;
        next_tasks = List.length st.next_instant;
        signal_count = List.length st.signals;
      }
  in
  Runtime_bootstrap.enqueue_root st initial input_signal output_signal;
  run_instant ~before_step ~after_step ~on_instant_end st instants

module Execution_helpers = struct
  let resolve_instants ~instants ~inputs =
    match instants with Some n -> n | None -> List.length inputs

  let make_input_cursor inputs =
    let remaining_inputs = ref inputs in
    fun () ->
      match !remaining_inputs with
      | [] -> None
      | x :: xs ->
          remaining_inputs := xs;
          x
end

let execute_trace ?instants ~inputs main =
  let outputs_rev = ref [] in
  let input = Execution_helpers.make_input_cursor inputs in
  let output v = outputs_rev := v :: !outputs_rev in
  let instants = Execution_helpers.resolve_instants ~instants ~inputs in
  execute ~instants ~input ~output main;
  List.rev !outputs_rev

type ('input, 'output) timeline_instant = {
  instant : int;
  input : 'input option;
  output : 'output option;
}

type ('input, 'output) timeline_instant_mut = {
  instant : int;
  input : 'input option;
  mutable output : 'output option;
}

let execute_timeline ?instants ~inputs main :
    ('input, 'output) timeline_instant list =
  let next_input = Execution_helpers.make_input_cursor inputs in
  let instant_counter = ref 0 in
  let timeline_rev : ('input, 'output) timeline_instant_mut list ref = ref [] in
  let input () =
    let input_value = next_input () in
    let rec_mut = { instant = !instant_counter; input = input_value; output = None } in
    incr instant_counter;
    timeline_rev := rec_mut :: !timeline_rev;
    input_value
  in
  let output value =
    match !timeline_rev with
    | current :: _ -> current.output <- Some value
    | [] -> ()
  in
  let instants = Execution_helpers.resolve_instants ~instants ~inputs in
  execute ~instants ~input ~output main;
  List.rev_map
    (fun (t : ('input, 'output) timeline_instant_mut) ->
      ({ instant = t.instant; input = t.input; output = t.output } :
        ('input, 'output) timeline_instant))
    !timeline_rev

(* helper functions *)

let new_signal : unit -> 'a signal =
  fun () -> perform (New_signal ())

let new_signal_agg :
    initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal =
 fun ~initial ~combine -> perform (New_signal_agg (initial, combine))

let emit : type emit agg mode. (emit, agg, mode) signal_core -> emit -> unit =
 fun s v -> perform (Emit (s, v))

let await : type emit agg mode. (emit, agg, mode) signal_core -> agg =
 fun s -> perform (Await s)

let await_immediate : 'a signal -> 'a =
  fun s -> perform (Await_immediate s)

let is_present :
    type emit agg mode. (emit, agg, mode) signal_core -> bool =
  fun s -> s.present

let peek : 'a signal -> 'a option =
  fun s -> if s.present then s.value else None

let pause : unit -> unit = 
  fun () -> perform Pause

let fork (proc : unit -> unit) : thread =
  perform (Fork proc)

let join (thread_id : thread) : unit =
  perform (Join thread_id)

let join_many (thread_ids : thread list) : unit =
  perform (Join_many thread_ids)

let parallel (procs : (unit -> unit) list) : unit =
  match procs with
  | [] -> ()
  | [ proc ] -> proc ()
  | _ ->
      let rec fork_all_rev acc = function
        | [] -> acc
        | p :: rest ->
            let tid = fork p in
            fork_all_rev (tid :: acc) rest
      in
      let threads = fork_all_rev [] procs in
      join_many threads

(* let (||) f g  = fun () -> parallel [f; g] *)

let when_ :
    type emit agg mode. (emit, agg, mode) signal_core -> (unit -> unit) -> unit =
 fun g body -> perform (With_guard (g, body))

let new_kill () = { alive = ref true; cleanup = None }

let abort_kill k =
  if !(k.alive) then begin
    k.alive := false;
    match k.cleanup with
    | Some f ->
        k.cleanup <- None;
        f ()
    | None -> ()
  end

let with_kill k body = perform (With_kill (k, body))

 let watch :
    type emit agg mode. (emit, agg, mode) signal_core -> (unit -> unit) -> unit =
  fun s body ->
    let shared_k = new_kill () in
    let guardian_k = new_kill () in
    let guardian () =
      with_kill shared_k (fun () ->
          with_kill guardian_k (fun () ->
              when_ s (fun () -> abort_kill shared_k)))
    in
    let _ = fork guardian in
    with_kill shared_k (fun () ->
        body ();
        abort_kill guardian_k)

(* let trap (body : trap -> unit) : unit =
  let module Owner_exit = struct exception Exit end in
  let handle =
    { trap_kill = new_kill ()
    ; trap_completion = new_signal ()
    ; trap_owner = current_task_id ()
    ; trap_finished = false
    ; raise_owner = (fun () -> raise Owner_exit.Exit)
    }
  in
  let finish () =
    if not handle.trap_finished then begin
      handle.trap_finished <- true;
      emit handle.trap_completion ()
    end
  in
  let wrapped () =
    try body handle with Owner_exit.Exit -> ()
  in
  with_kill handle.trap_kill (fun () ->
      wrapped ();
      finish ());
  let _ = await handle.trap_completion in
  () *)

(* let exit handle =
  if not handle.trap_finished then begin
    handle.trap_finished <- true;
    abort_kill handle.trap_kill;
    emit handle.trap_completion ();
    match (handle.trap_owner, current_task_id ()) with
    | (Some owner, Some current) when owner = current ->
        handle.raise_owner ()
    | _ ->
        raise Aborted
  end *)

let present_then_else :
    ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> (unit -> unit) -> unit =
 fun s body_then body_else ->
  let decision : [ `Then | `Else ] signal = new_signal () in
  let then_kill = new_kill () in
  let else_kill = new_kill () in
  let run_then () =
    with_kill then_kill (fun () ->
        when_ s (fun () ->
            abort_kill else_kill;
            body_then ();
            emit decision `Then))
  in
  let run_else () =
    with_kill else_kill (fun () ->
        pause ();
        abort_kill then_kill;
        body_else ();
        emit decision `Else)
  in
  let _ = fork run_then in
  let _ = fork run_else in
  let _ = await decision in
  ()

module Core = struct
  type nonrec event = event
  type nonrec aggregate = aggregate
  type nonrec ('emit, 'agg, 'mode) signal_core = ('emit, 'agg, 'mode) signal_core
  type nonrec 'a signal = 'a signal
  type nonrec ('emit, 'agg) agg_signal = ('emit, 'agg) agg_signal

  let new_signal = new_signal
  let new_signal_agg = new_signal_agg
  let emit = emit
  let await = await
  let await_immediate = await_immediate
  let pause = pause
  let when_ = when_
  let watch = watch
  let parallel = parallel
  let execute = execute
end

module Low_level = struct
  let new_kill = new_kill
  let with_kill = with_kill
  let abort_kill = abort_kill

  let fork = fork
  let join = join
end

module Tick_tags = struct
  type t = { mutable marks_rev : string list }
  let create () = { marks_rev = [] }
  let mark t tag = t.marks_rev <- tag :: t.marks_rev
  let all t = List.rev t.marks_rev
  let clear t = t.marks_rev <- []
end

type packed_state = Pack : {
  id : int;
  dump : unit -> string;
  restore : string -> unit;
} -> packed_state

let state_id_counter = ref 0
let state_registry : packed_state list ref = ref []

type 'a state = {
  mutable value : 'a;
  updates : ('a, 'a option) agg_signal;
}

let new_state initial =
  let id = !state_id_counter in
  incr state_id_counter;
  let st =
    {
      value = initial;
      updates = new_signal_agg ~initial:None ~combine:(fun _ v -> Some v);
    }
  in
  state_registry :=
    Pack
      {
        id;
        dump = (fun () -> Marshal.to_string st.value []);
        restore =
          (fun blob ->
            let v = Marshal.from_string blob 0 in
            st.value <- v);
      }
    :: !state_registry;
  st

let get_state st = st.value

let set_state st v =
  st.value <- v;
  emit st.updates v

let modify_state st f =
  let next = f st.value in
  set_state st next

let rec await_state st =
  match await st.updates with
  | Some v -> v
  | None -> await_state st

module State = struct
  type 'a t = 'a state

  let create = new_state
  let get = get_state
  let set = set_state
  let modify = modify_state
  let await = await_state

  let update_and_get st f =
    modify st f;
    get st
end

module Runtime_snapshot = struct
  type t = {
    frame : int;
    states : (int * string) list;
    tags : string list;
  }

  let capture ?tags ~frame () =
    let states = List.rev_map (fun (Pack p) -> (p.id, p.dump ())) !state_registry in
    let tags =
      match tags with
      | None -> []
      | Some t -> Tick_tags.all t
    in
    { frame; states; tags }

  let restore snapshot =
    List.iter
      (fun (id, blob) ->
        match List.find_opt (fun (Pack p) -> p.id = id) !state_registry with
        | None -> ()
        | Some (Pack p) -> p.restore blob)
      snapshot.states

  let frame s = s.frame
  let tags s = s.tags
end

module Dynamic = struct
  type handle = {
    kill : kill;
    thread : thread;
  }

  let spawn proc =
    let k = new_kill () in
    let t = fork (fun () -> with_kill k proc) in
    { kill = k; thread = t }

  let stop h = abort_kill h.kill
  let join h = join h.thread
  let spawn_many ps = List.map spawn ps
end

module Game = struct
  let after_n n body =
    if n < 0 then invalid_arg "Tempo.Game.after_n: n must be >= 0";
    let rec wait k =
      if k <= 0 then body ()
      else (
        pause ();
        wait (k - 1))
    in
    wait n

  let every_n n body =
    if n <= 0 then invalid_arg "Tempo.Game.every_n: n must be > 0";
    let rec loop k =
      if k <= 0 then (
        body ();
        pause ();
        loop (n - 1))
      else (
        pause ();
        loop (k - 1))
    in
    loop (n - 1)

  let timeout n ~on_timeout body =
    if n < 0 then invalid_arg "Tempo.Game.timeout: n must be >= 0";
    let timed_out = new_signal () in
    let completed = new_signal () in
    parallel
      [
        (fun () ->
          watch completed (fun () ->
              after_n n (fun () -> emit timed_out ())));
        (fun () ->
          watch timed_out (fun () ->
              body ();
              emit completed ()));
      ];
    when_ timed_out on_timeout

  let cooldown n s handler =
    if n < 0 then invalid_arg "Tempo.Game.cooldown: n must be >= 0";
    let rec sleep k =
      if k <= 0 then ()
      else (
        pause ();
        sleep (k - 1))
    in
    let rec loop () =
      let v = await s in
      handler v;
      sleep n;
      loop ()
    in
    loop ()
end

module Reactive = struct
  let rising_edge level input edge =
    let prev = ref false in
    let rec loop () =
      let v = await input in
      let now = level v in
      if now && not !prev then emit edge ();
      prev := now;
      loop ()
    in
    loop ()

  let falling_edge level input edge =
    let prev = ref false in
    let rec loop () =
      let v = await input in
      let now = level v in
      if (not now) && !prev then emit edge ();
      prev := now;
      loop ()
    in
    loop ()

  let edge_by pred input edge =
    let prev = ref None in
    let rec loop () =
      let curr = await input in
      (match !prev with Some p when pred p curr -> emit edge () | _ -> ());
      prev := Some curr;
      loop ()
    in
    loop ()

  let hold_last initial s =
    let st = new_state initial in
    ignore
      (fork (fun () ->
           let rec loop () =
             let v = await s in
             set_state st v;
             loop ()
           in
           loop ()));
    st

  let sample_on st trigger =
    let out = new_signal () in
    ignore
      (fork (fun () ->
           let rec loop () =
             let trig = await trigger in
             emit out (get_state st, trig);
             loop ()
           in
           loop ()));
    out

  let toggle_on ?(initial = false) trigger =
    let st = new_state initial in
    ignore
      (fork (fun () ->
           let rec loop () =
             let _ = await trigger in
             modify_state st not;
             loop ()
           in
           loop ()));
    st

  let pulse_n n =
    let out = new_signal () in
    ignore (fork (fun () -> Game.every_n n (fun () -> emit out ())));
    out

  let supervise_until stop procs = watch stop (fun () -> parallel procs)
end

module App = struct
  let tempo_emit = emit

  type 'msg dispatch = 'msg -> unit
  type 'msg command = dispatch:'msg dispatch -> unit

  let none ~dispatch:_ = ()
  let emit msg ~dispatch = dispatch msg

  let after_n n msg ~dispatch =
    if n < 0 then invalid_arg "Tempo.App.after_n: n must be >= 0";
    ignore (fork (fun () -> Game.after_n n (fun () -> dispatch msg)))

  let every_n n msg ~dispatch =
    if n <= 0 then invalid_arg "Tempo.App.every_n: n must be > 0";
    ignore (fork (fun () -> Game.every_n n (fun () -> dispatch msg)))

  let tick_every n ~tick = every_n n tick
  let tick_if enabled n ~tick = if enabled then tick_every n ~tick else none
  let command_if cond cmd = if cond then cmd else none
  let command_when cond ~then_ ~else_ = if cond then then_ else else_

  let batch cmds ~dispatch = List.iter (fun cmd -> cmd ~dispatch) cmds

  let boot_once_input ~boot input =
    let boot_sent = ref false in
    fun () ->
      if not !boot_sent then (
        boot_sent := true;
        Some boot)
      else input ()

  let input_union inputs () =
    let rec pick = function
      | [] -> None
      | f :: fs -> (match f () with Some _ as m -> m | None -> pick fs)
    in
    pick inputs

  let with_boot_and_tick ~boot ~tick ~tick_every:n ~input =
    if n <= 0 then invalid_arg "Tempo.App.with_boot_and_tick: tick_every must be > 0";
    (boot_once_input ~boot input, tick_every n ~tick)

  type ('model, 'msg) program = {
    init : 'model;
    update : 'model -> 'msg -> 'model * 'msg command;
  }

  let run ?instants ?(input = fun () -> None) ?(on_model = fun _ -> ())
      (program : ('model, 'msg) program) =
    execute ?instants ~input (fun input_signal _ ->
        let model = ref program.init in
        let pending : 'msg Queue.t = Queue.create () in
        let dispatch msg = Queue.push msg pending in
        let rec drain () =
          if not (Queue.is_empty pending) then (
            let msg = Queue.pop pending in
            let m', cmd = program.update !model msg in
            model := m';
            cmd ~dispatch;
            drain ())
        in
        let rec loop () =
          (match peek input_signal with Some msg -> dispatch msg | None -> ());
          drain ();
          on_model !model;
          pause ();
          loop ()
        in
        loop ())

  let run_with_view ?instants ?(input = fun () -> None) ?(equal_view = ( = ))
      ~view ?(output = fun _ -> ()) (program : ('model, 'msg) program) =
    execute ?instants ~input ~output (fun input_signal output_signal ->
        let model = ref program.init in
        let pending : 'msg Queue.t = Queue.create () in
        let last_view : 'view option ref = ref None in
        let dispatch msg = Queue.push msg pending in
        let rec drain () =
          if not (Queue.is_empty pending) then (
            let msg = Queue.pop pending in
            let m', cmd = program.update !model msg in
            model := m';
            cmd ~dispatch;
            drain ())
        in
        let maybe_emit_view () =
          let v = view !model in
          match !last_view with
          | Some prev when equal_view prev v -> ()
          | _ ->
              last_view := Some v;
              tempo_emit output_signal v
        in
        let rec loop () =
          (match peek input_signal with Some msg -> dispatch msg | None -> ());
          drain ();
          maybe_emit_view ();
          pause ();
          loop ()
        in
        loop ())
end

module Loop = struct
  type ('input, 'output, 'state) config = {
    init : 'state;
    input : unit -> 'input option;
    step : 'state -> 'input option -> 'state * 'output option;
    output : 'output -> unit;
  }

  let run ?instants cfg =
    let state = ref cfg.init in
    let main input_signal output_signal =
      let rec loop () =
        let seen = ref None in
        when_ input_signal (fun () -> seen := Some (await_immediate input_signal));
        let next_state, out = cfg.step !state !seen in
        state := next_state;
        (match out with Some v -> emit output_signal v | None -> ());
        pause ();
        loop ()
      in
      loop ()
    in
    execute ?instants ~input:cfg.input ~output:cfg.output main
end

module Scene = struct
  type 'id t = {
    request_sig : 'id signal;
    mutable current_id : 'id option;
    equal : 'id -> 'id -> bool;
    on_enter : 'id -> unit;
    on_exit : 'id -> unit;
  }

  let create ?(equal = ( = )) ~on_enter ~on_exit () =
    { request_sig = new_signal (); current_id = None; equal; on_enter; on_exit }

  let request t id = emit t.request_sig id
  let current t = t.current_id

  let process t =
    let rec loop () =
      let next = await t.request_sig in
      (match t.current_id with
      | Some prev when t.equal prev next -> ()
      | Some prev ->
          t.on_exit prev;
          t.current_id <- Some next;
          t.on_enter next
      | None ->
          t.current_id <- Some next;
          t.on_enter next);
      loop ()
    in
    loop ()
end

module Resource = struct
  type t = {
    acquire_fn : unit -> unit;
    release_fn : unit -> unit;
    mutable acquired : bool;
  }

  let create ~acquire ~release = { acquire_fn = acquire; release_fn = release; acquired = false }

  let acquire r =
    if not r.acquired then (
      r.acquire_fn ();
      r.acquired <- true)

  let release r =
    if r.acquired then (
      r.release_fn ();
      r.acquired <- false)

  let with_resource r f =
    acquire r;
    Fun.protect ~finally:(fun () -> release r) f

  type manager = { mutable resources : t list }
  let create_manager () = { resources = [] }
  let register m r = m.resources <- r :: m.resources
  let release_all m = List.iter release m.resources
end

module Input_map = struct
  type ('raw, 'action) t = {
    table : ('raw, 'action) Hashtbl.t;
    default : 'action option;
  }

  let create ?default bindings =
    let table = Hashtbl.create (max 8 (List.length bindings)) in
    List.iter (fun (k, v) -> Hashtbl.replace table k v) bindings;
    { table; default }

  let bind t raw action = Hashtbl.replace t.table raw action
  let resolve t raw = match Hashtbl.find_opt t.table raw with Some v -> Some v | None -> t.default
end

module Event_bus = struct
  type 'a channel = ('a, 'a list) agg_signal
  let channel () = new_signal_agg ~initial:[] ~combine:(fun acc v -> v :: acc)
  let publish ch v = emit ch v
  let await_batch ch = List.rev (await ch)
end

module Fixed_step = struct
  type accumulator = { leftover : float }
  let empty = { leftover = 0.0 }

  let step ~fixed_dt ~delta acc =
    if fixed_dt <= 0.0 then invalid_arg "Tempo.Fixed_step.step: fixed_dt must be > 0";
    let total = acc.leftover +. delta in
    let steps = int_of_float (floor (total /. fixed_dt)) in
    let leftover = total -. (float_of_int steps *. fixed_dt) in
    (steps, { leftover })

  let alpha ~fixed_dt acc =
    if fixed_dt <= 0.0 then invalid_arg "Tempo.Fixed_step.alpha: fixed_dt must be > 0";
    let a = acc.leftover /. fixed_dt in
    if a < 0.0 then 0.0 else if a > 1.0 then 1.0 else a
end

module Rng = struct
  type t = Random.State.t
  let create seed = Random.State.make [| seed |]
  let int t bound = Random.State.int t bound
  let float t bound = Random.State.float t bound
  let bool t = Random.State.bool t
  let split t = create (Random.State.int t Int.max_int)
end

module Netcode = struct
  type 's snapshot = {
    frame : int;
    state : 's;
  }

  let snapshot ~frame state = { frame; state }
  let rollback st snap = set_state st snap.state
end

module Profiler = struct
  type sample = {
    name : string;
    duration_ns : int64;
  }

  type t = { mutable samples_rev : sample list }

  let create () = { samples_rev = [] }
  let clear t = t.samples_rev <- []

  let measure t ~name f =
    let c = Mtime_clock.counter () in
    let result = f () in
    let span = Mtime_clock.count c in
    let ns = Int64.of_float (Mtime.Span.to_float_ns span) in
    t.samples_rev <- { name; duration_ns = ns } :: t.samples_rev;
    result

  let snapshot t = List.rev t.samples_rev
end

module Entity_set = struct
  type ('id, 'entity) t = {
    items : ('id, ('entity * Dynamic.handle)) Hashtbl.t;
  }

  let create () = { items = Hashtbl.create 16 }

  let spawn t ~id ~entity ~process =
    (match Hashtbl.find_opt t.items id with
    | Some (_, h) ->
        Dynamic.stop h;
        Dynamic.join h
    | None -> ());
    let h = Dynamic.spawn (fun () -> process entity) in
    Hashtbl.replace t.items id (entity, h)

  let despawn t id =
    match Hashtbl.find_opt t.items id with
    | None -> ()
    | Some (_, h) ->
        Dynamic.stop h;
        Dynamic.join h;
        Hashtbl.remove t.items id

  let broadcast t f = Hashtbl.iter (fun _ (entity, _) -> f entity) t.items
  let ids t = Hashtbl.fold (fun id _ acc -> id :: acc) t.items []

  let despawn_all t =
    Hashtbl.iter
      (fun _ (_, h) ->
        Dynamic.stop h;
        Dynamic.join h)
      t.items;
    Hashtbl.reset t.items
end

module Timeline_json = struct
  type 'a serializer = 'a -> string

  let escape_json s =
    let b = Buffer.create (String.length s + 16) in
    String.iter
      (function
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | '\r' -> Buffer.add_string b "\\r"
        | '\t' -> Buffer.add_string b "\\t"
        | c -> Buffer.add_char b c)
      s;
    Buffer.contents b

  let json_string_or_null to_string = function
    | None -> "null"
    | Some v -> "\"" ^ escape_json (to_string v) ^ "\""

  let of_timeline_with
      ~(input_to_string : 'a serializer)
      ~(output_to_string : 'b serializer)
      (timeline : ('a, 'b) timeline_instant list) =
    let item_to_json (i : ('a, 'b) timeline_instant) =
      let input = json_string_or_null input_to_string i.input in
      let output = json_string_or_null output_to_string i.output in
      Printf.sprintf
        "{\"instant\":%d,\"input\":%s,\"output\":%s}"
        i.instant input output
    in
    "[" ^ String.concat "," (List.map item_to_json timeline) ^ "]"

  let of_timeline input_to_string output_to_string timeline =
    of_timeline_with ~input_to_string ~output_to_string timeline
end

module Error_bus = struct
  type error = {
    instant : int option;
    exn : exn;
    backtrace : string;
  }

  let signal () = new_signal ()

  let safe errors proc =
    try proc () with
    | exn ->
        let bt = Printexc.get_backtrace () in
        emit errors { instant = None; exn; backtrace = bt }

  let execute_safe ?instants ?input ?output ~errors initial =
    execute ?instants ?input ?output (fun i o -> safe errors (fun () -> initial i o))
end

let version_string = "0.2.0-game"
let api_level = 2

let require_api_level n =
  if n > api_level then
    invalid_arg
      (Printf.sprintf "Tempo API level %d required, current level is %d" n api_level)

module Dev_hud = struct
  let to_lines (snap : inspector_snapshot) =
    [
      Printf.sprintf "instant: %d" snap.instant;
      Printf.sprintf "tasks(current/blocked/next): %d/%d/%d" snap.current_tasks
        snap.blocked_tasks snap.next_tasks;
      Printf.sprintf "signals: %d" snap.signal_count;
    ]

  let to_string snap = String.concat "\n" (to_lines snap)
end

module Layer2 = struct
  module Dynamic = Dynamic
  module Game = Game
  module Reactive = Reactive
  module App = App
  module Loop = Loop
  module Scene = Scene
  module Resource = Resource
  module Input_map = Input_map
  module Event_bus = Event_bus
  module Fixed_step = Fixed_step
  module Rng = Rng
  module Netcode = Netcode
  module Profiler = Profiler
  module Entity_set = Entity_set
  module State = State
  module Timeline_json = Timeline_json
  module Tick_tags = Tick_tags
  module Runtime_snapshot = Runtime_snapshot
  module Error_bus = Error_bus
  module Dev_hud = Dev_hud
end

(* let rec do_every (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) :
    unit =
  let completion = new_signal () in
  watch s (fun () ->
      body ();
      emit completion ());
  present_then_else completion
    (fun () ->
      pause ();
      do_every s body)
    (fun () -> do_every s body) *)

(* let every_do (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) : unit =
  let rec loop () =
    let _ = await s in
    pause ();
    let completion = new_signal () in
    watch s (fun () ->
        body ();
        emit completion ());
    present_then_else completion
      (fun () ->
        pause ();
        loop ())
      (fun () -> loop ())
  in
  loop () *)

(*****************)

(* let watch_ (g : 'a signal) (body : kill -> unit) : proc =
  fun () ->
    let k = new_kill () in
    let guardian () =
      let _ = await g in
      abort k
    in
    fork guardian;
    try
      with_kill k (fun () -> body k)
    with Aborted -> ()

  let every_ (g : 'a signal) (body : kill -> unit) : proc =
    fun () ->
      let current = ref (new_kill ()) in
      let start_instance (k : kill) =
        fork (fun () ->
          try with_kill k (fun () -> body k) with Aborted -> ())
      in
      (* démarre une première instance *)
      start_instance !current;

      let rec controller () =
        (* strong restart: si g est déjà présent, await revient immédiatement *)
        let _ = await g in
          abort !current;
        let k' = new_kill () in
          current := k';
        start_instance k';
        (* empêche de redéclencher en boucle dans le même instant *)
        pause ();
        controller ()
    in
    controller () *)

(** Kill *)
(* let new_kill () = {alive = ref true}
let abort k = k.alive := false
let check k = if not !(k.alive) then raise Aborted 
let with_kill k body = perform (With_kill (k, body)) *)
(* let kills_ok (ks : kill list) = 
  List.for_all (fun k -> !(k.alive)) ks *)

  (* | effect (With_kill (kk, body)), k ->
          enqueue_now st  
            (mk_task st
              t.guards
              (kk::t.kills)
              (fun () -> 
                  body ();
                  enqueue_now st (
                    mk_task st
                      t.guards 
                      t.kills
                      (fun () -> continue k ())
                  ) 
                  )) *)
