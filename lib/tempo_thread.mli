type thread_state = Tempo_types.thread_state

val ensure :
  (Tempo_types.thread, thread_state) Hashtbl.t ->
  Tempo_types.thread ->
  thread_state

val find :
  (Tempo_types.thread, thread_state) Hashtbl.t ->
  Tempo_types.thread ->
  thread_state

val add_join_waiter :
  (Tempo_types.thread, thread_state) Hashtbl.t ->
  Tempo_types.thread ->
  (unit -> unit) ->
  unit

val finish_task :
  (Tempo_types.thread, thread_state) Hashtbl.t ->
  Tempo_types.thread ->
  unit
