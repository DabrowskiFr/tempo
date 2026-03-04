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

let register_signal (st : Tempo_types.scheduler_state) s =
  st.signals <- Tempo_types.Any s :: st.signals

let fresh_event_signal (st : Tempo_types.scheduler_state) :
    'a Tempo_types.signal =
  let s =
    Tempo_types.
      {
        s_id = fresh_signal_id st
      ; present = false
      ; value = None
      ; awaiters = []
      ; guard_waiters = []
      ; kind = Tempo_types.Event_signal
      }
  in
  register_signal st s;
  s

let fresh_aggregate_signal (st : Tempo_types.scheduler_state) ~initial ~combine
    =
  let s =
    Tempo_types.
      {
        s_id = fresh_signal_id st
      ; present = false
      ; value = None
      ; awaiters = []
      ; guard_waiters = []
      ; kind = Tempo_types.Aggregate_signal { combine; initial }
      }
  in
  register_signal st s;
  s

let guard_ok guards = List.for_all (fun (Tempo_types.Any s) -> s.present) guards

let missing_guards guards =
  List.filter (fun (Tempo_types.Any s) -> not s.present) guards

let update_signal : type emit agg mode.
    Tempo_types.scheduler_state -> (emit, agg, mode) signal_core -> emit -> unit
    =
 fun st s v ->
  (match s.kind with
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
      in
      s.value <- Some acc);
  Tempo_task.wake_guard_waiters st s

(* Emits an event signal initiated by the host (outside the effect handler), 
   waking awaiters immediately. *)
let emit_event_from_host : type a.
    Tempo_types.scheduler_state -> (a, a, event) signal_core -> a -> unit =
 fun st s value ->
  if s.present then invalid_arg "Emit : multiple emission";
  s.present <- true;
  s.value <- Some value;
  let resumes = s.awaiters in
  s.awaiters <- [];
  List.iter (fun resume -> resume value) resumes;
  Tempo_task.wake_guard_waiters st s

let finalize_signals (st : Tempo_types.scheduler_state) =
  List.iter
    (fun (Tempo_types.Any s) ->
      (match s.kind with
      | Tempo_types.Aggregate_signal _ when s.present ->
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
