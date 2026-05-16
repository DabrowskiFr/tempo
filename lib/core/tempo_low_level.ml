open Effect
open Tempo_types

type kill = Tempo_types.kill

let new_kill () = { alive = ref true; cleanup = None }

let abort_kill_core ~bump_epoch (k : kill) =
  if !(k.alive) then begin
    k.alive := false;
    (* Bump global kill epoch only when a continuation cleanup may wake
       contexts that rely on epoch-based cache invalidation. *)
    if bump_epoch && Option.is_some k.cleanup then Tempo_task.bump_kill_epoch ()
  end;
  match k.cleanup with
  | None -> ()
  | Some cleanup ->
      k.cleanup <- None;
      cleanup ()

let abort_kill (k : kill) = abort_kill_core ~bump_epoch:true k

let abort_kill_batched (k : kill) = abort_kill_core ~bump_epoch:false k

let with_kill (k : kill) (f : unit -> unit) = perform (With_kill (k, f))
