type thread_state = Tempo_types.thread_state

let ensure (threads : (Tempo_types.thread, thread_state) Hashtbl.t)
    (thread : Tempo_types.thread) =
  match Hashtbl.find_opt threads thread with
  | Some ts -> ts
  | None ->
      let ts =
        Tempo_types.{ active = 0; completed = false; waiters = [] }
      in
      Hashtbl.add threads thread ts;
      ts

let find (threads : (Tempo_types.thread, thread_state) Hashtbl.t)
    (thread : Tempo_types.thread) =
  match Hashtbl.find_opt threads thread with
  | Some ts -> ts
  | None -> invalid_arg (Printf.sprintf "unknown thread %d" thread)

let add_join_waiter threads (thread : Tempo_types.thread) waiter =
  let state = find threads thread in
  if state.completed then waiter ()
  else state.waiters <- waiter :: state.waiters

let finish_task threads (thread : Tempo_types.thread) =
  let rec resume_waiters waiters =
    match waiters with
    | [] -> ()
    | f :: rest ->
        f ();
        resume_waiters rest
  in
  let state = find threads thread in
  state.active <- state.active - 1;
  if state.active = 0 then begin
    state.completed <- true;
    let waiters = List.rev state.waiters in
    state.waiters <- [];
    resume_waiters waiters
  end
