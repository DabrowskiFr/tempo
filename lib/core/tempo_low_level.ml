open Effect

type kill = Tempo_types.kill
type thread = Tempo_types.thread

let new_kill () = { Tempo_types.alive = ref true; cleanup = None }

let abort_kill (k : kill) =
  if !(k.alive) then k.alive := false;
  match k.cleanup with
  | None -> ()
  | Some cleanup ->
      k.cleanup <- None;
      cleanup ()

let with_kill (k : kill) (f : unit -> unit) = perform (Tempo_types.With_kill (k, f))
let with_guard s body = perform (Tempo_types.With_guard (s, body))
let fork (proc : unit -> unit) : thread = perform (Tempo_types.Fork proc)
let join (thread_id : thread) : unit = perform (Tempo_types.Join thread_id)
let peek s = s.Tempo_types.value
let is_present s = s.Tempo_types.present
