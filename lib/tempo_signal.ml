let fresh_signal_id (st : Tempo_types.scheduler_state) =
  let id = st.debug.sig_counter in
  st.debug.sig_counter <- id + 1;
  id

let register_signal (st : Tempo_types.scheduler_state) s =
  st.signals <- Tempo_types.Any s :: st.signals

let fresh_event_signal
    (st : Tempo_types.scheduler_state) : 'a Tempo_types.signal =
  let s =
    Tempo_types.
      { s_id = fresh_signal_id st
      ; present = false
      ; value = None
      ; awaiters = []
      ; guard_waiters = []
      ; kind = Tempo_types.Event_signal
      }
  in
  register_signal st s;
  s

let fresh_aggregate_signal
    (st : Tempo_types.scheduler_state)
    ~initial ~combine =
  let s =
    Tempo_types.
      { s_id = fresh_signal_id st
      ; present = false
      ; value = None
      ; awaiters = []
      ; guard_waiters = []
      ; kind = Tempo_types.Aggregate_signal { combine; initial }
      }
  in
  register_signal st s;
  s

let guard_ok guards =
  List.for_all (fun (Tempo_types.Any s) -> s.present) guards

let missing_guards guards =
  List.filter (fun (Tempo_types.Any s) -> not s.present) guards

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
