open Effect
open Tempo_types

type kill = Tempo_types.kill

let new_kill () = { alive = ref true; cleanup = None }

let abort_kill (k : kill) =
  if !(k.alive) then begin
    k.alive := false;
    (* Bump global kill epoch only when a continuation cleanup may wake
       contexts that rely on epoch-based cache invalidation. *)
    if Option.is_some k.cleanup then Tempo_task.bump_kill_epoch ()
  end;
  match k.cleanup with
  | None -> ()
  | Some cleanup ->
      k.cleanup <- None;
      cleanup ()

let with_kill (k : kill) (f : unit -> unit) = perform (With_kill (k, f))
