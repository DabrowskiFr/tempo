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

(* structure to keep track of logical threads states              
  - active : number of tasks currently running under that thread 
  - completed : whether the thread has completed all its tasks
  - waiters : continuations to call when the thread completes    *)

type thread_state =
  { mutable active : int
  ; mutable completed : bool
  ; mutable waiters : (unit -> unit) list
  }

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
  ; threads                : (thread, thread_state) Hashtbl.t
  ; debug                  : debug_info
  }

let log_ctx st =
  Tempo_log.context ~instant:st.debug.instant_counter ~step:st.debug.step_counter

let ensure_thread_state st thread =
  match Hashtbl.find_opt st.threads thread with
  | Some ts -> ts
  | None ->
      let ts = { active = 0; completed = false; waiters = [] } in
      Hashtbl.add st.threads thread ts;
      ts

let find_thread_state st thread =
  match Hashtbl.find_opt st.threads thread with
  | Some ts -> ts
  | None -> invalid_arg (Printf.sprintf "unknown thread %d" thread)

let new_thread_id st =
  let id = st.thread_counter in
  st.thread_counter <- id + 1;
  let _ = ensure_thread_state st id in
  id


let add_join_waiter st thread waiter =
  let state = find_thread_state st thread in
  if state.completed then waiter ()
  else state.waiters <- waiter :: state.waiters

(* -------------------------------------------------------------------------- *)
(* Guard helpers                                                              *)
(* -------------------------------------------------------------------------- *)

(* Checks that every kill token in the list is still alive (i.e. not cancelled). *)
let kills_alive kills = List.for_all (fun k -> !(k.alive)) kills

(* Guards *)
(** [guard_ok signals] checks whether all signals in [signals] are present.
    @param signals a list of signals
    @return [true] if all signals are present, [false] otherwise. *)
let guard_ok : any_signal list -> bool = 
  List.for_all (fun (Any s) -> s.present)

(** [missing_guards signals] : returns the signals in [signals] that are not present. 
    @param guards a list of signals
    @return the sublist of signals that are absent
  *)
let missing_guards : any_signal list -> any_signal list = 
  List.filter (fun (Any s) -> not s.present)


(******************************************************************)
(* Task scheduling                                                *)
(******************************************************************)

(* Enqueue a task to be scheduled in the current instant. 
   pre : the task is alive *)
let enqueue_now : scheduler_state -> task -> unit =
  fun st t ->
    assert (kills_alive t.kills);
    if not t.queued then
        begin 
          Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.current" "push task %a" (Tempo_log.pp_task ~brief:false) t;
          t.queued <- true; 
          t.blocked <- false; 
          Queue.add t st.current 
        end

(* Enqueue a task to be scheduled in the next instant 
   pre : the task is alive *)
let enqueue_next : scheduler_state -> task -> unit = 
  fun st t -> 
    assert (kills_alive t.kills);
    Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.next" "schedule task for next instant";
    st.next_instant <- t::st.next_instant;
    Tempo_log.log (log_ctx st) "tasks.next" "pending next instant: %a" Tempo_log.pp_task_list_brief st.next_instant

(*  Block a task on its unsatified guards
    pre : the task is alive *)
let block_on_guards : scheduler_state -> task -> unit =  
  fun st t -> 
    assert (kills_alive t.kills);
    if not t.blocked then begin
        t.blocked <- true;
        st.blocked <- t :: st.blocked
      end;
    let miss = missing_guards t.guards in
    Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.block"
      "blocked on guards %a" (Tempo_log.pp_any_guard_list ~brief:false) miss;
    Tempo_log.log_guard ~task:t.t_id (log_ctx st)
      "waiting for guards %a" (Tempo_log.pp_any_guard_list ~brief:false) miss;
    List.iter (fun (Any s) -> s.guard_waiters <- t :: s.guard_waiters) miss


(* Given a signal s, wake up all tasks waiting on the guard s 
   if the guard is satisfied, otherwise block the task on its missing guards *)

let wake_guard_waiters : 
    scheduler_state -> ('emit, 'agg, 'mode) signal_core -> unit =
    fun st s ->
      List.iter 
        (fun t -> if guard_ok t.guards && kills_alive t.kills then enqueue_now st t) 
        s.guard_waiters;
      s. guard_waiters <- []


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
    ; awaiters = []
    ; guard_waiters = []
    ; kind = Event_signal
    }
  in
  register_signal st s;
  Tempo_log.log ~signal:s.s_id (log_ctx st) "signals"
    "created IO signal %d" s.s_id;
  s

(* on ne reinitialise pas awaiter pour un signal aggregate,
  la valeur ne sera connue qu'à la fin de l'instant et 
  finalize leur transmettra la valeur *)

let update_signal : 
  type emit agg mode. scheduler_state -> (emit, agg, mode) signal_core -> emit -> unit = 
  fun st s v ->
  begin 
    match s.kind with
    | Event_signal -> 
          if s.present then invalid_arg "Emit : multiple emission";
          let resumes = s.awaiters in
          s.present <- true;
          s.value <- Some v;
          s.awaiters <- [];
          List.iter (fun resume -> resume v) resumes
    | Aggregate_signal { combine; initial } -> 
        s.present <- true;
        let acc =
          match s.value with
          | None -> combine initial v
          | Some agg -> combine agg v
        in s.value <- Some acc
    end;
    wake_guard_waiters st s
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
              let resumes = s.awaiters in
              s.awaiters <- [];
              List.iter (fun resume -> resume delivered) resumes
          | _ -> ());
         s.present <- false;
         s.value <- None;
         s.guard_waiters <- [])
      st.signals

(******************************************************************)
(* Tasks management                                               *)
(******************************************************************)

let register_task st thread =
  let state = ensure_thread_state st thread in
  if state.completed && state.active = 0 then state.completed <- false;
  state.active <- state.active + 1

let mk_task : scheduler_state -> thread -> any_signal list -> kill list -> (unit -> unit)-> task =
  fun st thread guards kills run ->
    let t_id = st.debug.task_counter in 
    st.debug.task_counter <- st.debug.task_counter + 1;
    register_task st thread;
    { t_id; thread; guards; kills; run; queued = false; blocked = false }

let finish_task st thread =
  let rec resume_waiters waiters =
    match waiters with
    | [] -> ()
    | f :: rest ->
        f ();
        resume_waiters rest
  in
  let state = find_thread_state st thread in
  state.active <- state.active - 1;
  if state.active = 0 then begin
    state.completed <- true;
    let waiters = List.rev state.waiters in
    state.waiters <- [];
    resume_waiters waiters
  end

(*****************************************************************************)
(* Host input *)
(*****************************************************************************)

(* Emits an event signal initiated by the host (outside the effect handler), 
   waking awaiters immediately. *)
let emit_event_from_host :
    type a. scheduler_state -> (a, a, event) signal_core -> a -> unit =
 fun st s value ->
  Tempo_log.log ~signal:s.s_id (log_ctx st) "signals" "io emit signal";
  if s.present then invalid_arg "Emit : multiple emission";
  s.present <- true;
  s.value <- Some value;
  Tempo_log.log ~signal:s.s_id (log_ctx st) "tasks.await"
    "resuming %d queued continuations" (List.length s.awaiters);
  let resumes = s.awaiters in
  s.awaiters <- [];
  List.iter (fun resume -> resume value) resumes;
  wake_guard_waiters st s

(* -------------------------------------------------------------------------- *)
(* Effect handler                                                             *)
(* -------------------------------------------------------------------------- *)

(* Handle a task by performing its effect *)

let fresh_signal_id st = 
  let id = st.debug.sig_counter in 
    st.debug.sig_counter <- id + 1; 
    id 

let fresh_event_signal st : 'a signal =
  let s = 
  { s_id = fresh_signal_id st;
    present = false; value = None;
    awaiters = []; guard_waiters = [];
    kind = Event_signal }
  in register_signal st s; s

let fresh_aggregate_signal : 
  scheduler_state -> initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit,'agg) agg_signal = 
    fun st ~initial ~combine ->
    let s =
      { s_id = fresh_signal_id st
      ; present = false
      ; value = None
      ; awaiters = []
      ; guard_waiters = []
      ; kind = Aggregate_signal { combine; initial }
      }
    in register_signal st s; s

let handle_task : scheduler_state -> task -> unit = 
  fun st t ->
   let run_task () =
     match t.run () with 
        | () -> ()
        | effect (New_signal (_)), k -> 
          let s = fresh_event_signal st in
          let t' = mk_task st t.thread t.guards t.kills (fun () -> continue k s) in
            enqueue_now st t'
        | effect (New_signal_agg (initial, combine)), k ->
            let s = fresh_aggregate_signal st ~initial ~combine in
            let t' = mk_task st t.thread t.guards t.kills (fun () -> continue k s) in
              enqueue_now st t'
        | effect (Emit (s, v)), k  ->
            update_signal st s v;
            let t' = mk_task st t.thread t.guards t.kills (fun () -> continue k ()) in
              enqueue_now st t';
              Tempo_log.log (log_ctx st) "signals" "state: %a" (Tempo_log.pp_any_signal_list ~brief:true) st.signals    
        | effect (Await s), k ->
              Tempo_log.log ~task:t.t_id ~signal:s.s_id (log_ctx st) "tasks.await" "waiting for signal";
              let resume v =
                let t' = mk_task st t.thread t.guards t.kills (fun () -> continue k v) in
                  enqueue_next st t'
              in
                if s.present then
                  match s.kind with
                  | Event_signal -> resume (Option.get s.value)
                  | Aggregate_signal _ -> s.awaiters <- resume :: s.awaiters
                else s.awaiters <- resume :: s.awaiters
        | effect (Await_immediate s), k ->
            Tempo_log.log ~task:t.t_id ~signal:s.s_id (log_ctx st) "tasks.await_immediate" "waiting for signal";
            if s.present then
              match s.value with
              | Some v ->
                  let t' = mk_task st t.thread t.guards t.kills (fun () -> continue k v) in
                  Tempo_log.log ~task:t'.t_id ~signal:s.s_id (log_ctx st) "tasks.await_immediate"
                    "signal already present, resume in current instant as task %d" t'.t_id;
                  enqueue_now st t'
              | None -> failwith "Error : present but no value"
            else
              let resume v =
                let t = mk_task st t.thread t.guards t.kills (fun () -> continue k v) in
                Tempo_log.log ~task:t.t_id ~signal:s.s_id (log_ctx st) "tasks.await_immediate"
                  "resume task in current instant (task %d)" t.t_id;
                assert (kills_alive t.kills);
                  enqueue_now st t
              in
              s.awaiters <- resume :: s.awaiters
        | effect Pause, k -> 
            let t' = mk_task st t.thread t.guards t.kills (fun() -> continue k ()) in
            Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.pause"
              "rescheduled as task %d next instant" t'.t_id;
            enqueue_next st t'
        | effect (Fork p_child), k -> 
              let child_thread = new_thread_id st in
              enqueue_now st (mk_task st child_thread t.guards t.kills p_child);
              enqueue_now st (mk_task st t.thread t.guards t.kills (fun () -> continue k child_thread))
        | effect (With_guard (s, body)), k -> 
           let t' = 
            mk_task st t.thread (Any s :: t.guards) t.kills 
            (
              fun () -> 
                body (); (* body may perform an abort_kill*)
                if (kills_alive t.kills) then begin
                  let t'' = (mk_task st t.thread t.guards t.kills (fun () -> continue k ())) in
                  assert (kills_alive t''.kills);
                  enqueue_now st t''
                end
              )
            in 
              Tempo_log.log ~task:t.t_id ~signal:s.s_id (log_ctx st) "tasks.guard"
                "updated as task %d, now guarded by signal %d" t'.t_id s.s_id;
              Tempo_log.log_guard ~task:t.t_id ~signal:s.s_id (log_ctx st)
                "task %d registered guard on signal %d" t'.t_id s.s_id;
                                assert (kills_alive t'.kills);
              enqueue_now st t'
        | effect (With_kill (kk, body)), k ->
            let continue_now () =
              kk.cleanup <- None;
              let t' = (mk_task st t.thread t.guards t.kills (fun () -> continue k ())) in
              enqueue_now st t'
            and continue_later () =
              kk.cleanup <- None;
              enqueue_next st (mk_task st t.thread t.guards t.kills (fun () -> continue k ()))
            in
            kk.cleanup <- Some continue_later;
            let runner () =
              body ();
              continue_now ()
            in
              Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.watched"
              "updated as task %d, now watched by signal" t.t_id;
            let t' = mk_task st t.thread t.guards (kk :: t.kills) runner in
            enqueue_now st t'
        | effect (Join thread_id), k ->
            if thread_id = t.thread then invalid_arg "join: cannot join current thread";
            let resume () =
              let t' = (mk_task st t.thread t.guards t.kills (fun () -> continue k ())) in
              enqueue_now st t'
            in
            add_join_waiter st thread_id resume
   in
   try Fun.protect ~finally:(fun () -> finish_task st t.thread) run_task
   with
   | Aborted ->
       Tempo_log.log ~task:t.t_id (log_ctx st) "tasks" "aborted";
       ()
  
(* -------------------------------------------------------------------------- *)
(* Scheduler loop                                                             *)
(* -------------------------------------------------------------------------- *)

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
          else (finish_task st t.thread; take_next ())
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
    (* Tempo_log.log_banner (log_ctx st) "instant" (Format.asprintf "Instant %03d START" st.debug.instant_counter); *)
    st.blocked <- [];
    let l = Tempo_log.snapshot_queue st.current in
    Tempo_log.log_queue_state (log_ctx st) "queues" l st.blocked st.next_instant;
    Tempo_log.log (log_ctx st) "signals" "state: %a" Tempo_log.pp_any_signal_list_full st.signals;
    before_step ();
    step st;
    after_step ();
    (* Tempo_log.log_banner (log_ctx st) "instant" (Format.asprintf "Instant %03d END" st.debug.instant_counter); *)
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
                 else (finish_task st t.thread; false))
              ts
          in
          Tempo_log.log ~level:Logs.Debug (log_ctx st) "instant" "ending instant, moving with %d tasks" (List.length survivors);
          st.next_instant <- [];
          List.iter (enqueue_now st) (List.rev survivors);
          run_instant ~before_step ~after_step st (Option.map pred remaining)

(* -------------------------------------------------------------------------- *)
(* Public API entry point                                                     *)
(* -------------------------------------------------------------------------- *)

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
  let thread = new_thread_id st in
  let root =
    mk_task st thread [] [] (fun () -> initial input_signal output_signal)
  in
  enqueue_now st root;
  run_instant ~before_step ~after_step st instants;
  Tempo_log.log_duration_summary ()

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

let pause : unit -> unit = 
  fun () -> perform Pause

let fork (proc : unit -> unit) : thread =
  perform (Fork proc)

let join (thread_id : thread) : unit =
  perform (Join thread_id)

let parallel (procs : (unit -> unit) list) : unit =
  let threads = List.map fork procs in
  List.iter join threads

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

module Low_level = struct
  let new_kill = new_kill
  let with_kill = with_kill
  let abort_kill = abort_kill

  let fork = fork
  let join = join
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


                  (* let block_on_guards : scheduler_state -> task -> unit =  
  fun st t -> 
    if kills_alive t.kills then begin
      if not t.blocked then begin
        t.blocked <- true;
        st.blocked <- t :: st.blocked
      end;
      let miss = missing_guards t.guards in
      Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.block"
        "blocked on guards %a" (Tempo_log.pp_any_guard_list ~brief:false) miss;
      Tempo_log.log_guard ~task:t.t_id (log_ctx st)
        "waiting for guards %a" (Tempo_log.pp_any_guard_list ~brief:false) miss;
      List.iter (fun (Any s) -> s.guard_waiters <- t :: s.guard_waiters) miss
    end else
      finish_task st t.thread *)


    (* let wake_guard_waiters :
    scheduler_state -> ('emit, 'agg, 'mode) signal_core -> unit =
    fun st s ->
      let ws = s.guard_waiters in
      s.guard_waiters <- [];
      if Tempo_log.trace_guards && ws <> [] then
        Tempo_log.log_guard ~signal:s.s_id (log_ctx st)
          "signal %d waking %d guard waiters" s.s_id (List.length ws);
    List.iter
        (fun t ->
            if guard_ok t.guards then enqueue_now st t else block_on_guards st t)
        ws *)

        (* let enqueue_next : scheduler_state -> task -> unit = 
  fun st t -> 
        if kills_alive t.kills then begin
          Tempo_log.log ~task:t.t_id (log_ctx st) "tasks.next" "schedule task for next instant";
          st.next_instant <- t::st.next_instant;
          Tempo_log.log (log_ctx st) "tasks.next" "pending next instant: %a" Tempo_log.pp_task_list_brief st.next_instant
        end else
          finish_task st t.thread *)
