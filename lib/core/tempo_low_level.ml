open Effect
open Tempo_types

type kill = Tempo_types.kill

let new_kill () = { alive = ref true; cleanup = None }

let abort_kill (k : kill) =
  if !(k.alive) then begin
    k.alive := false;
    Tempo_task.bump_kill_epoch ()
  end;
  match k.cleanup with
  | None -> ()
  | Some cleanup ->
      k.cleanup <- None;
      cleanup ()

let with_kill (k : kill) (f : unit -> unit) = perform (With_kill (k, f))
