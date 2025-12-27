open Tempo

let log name message = Format.printf "[%s] %s@.%!" name message

let rec loop b () : unit =
  b ();
  pause ();
  loop b ()

let tick_tock tick tock =
  loop (fun () ->
      log "tick_tock" (Format.asprintf "tick@.%!");
      emit tick ();
      pause ();
      log "tick_tock" (Format.asprintf "tock@.%!");
      emit tock ())

let when_tick s () =
  when_ s (fun () ->
      loop (fun () -> log "when_tick" (Format.asprintf "tick received@.%!")) ())

let scenario () =
  let tick = new_signal () in
  let tock = new_signal () in
  parallel [ tick_tock tick tock; when_tick tick ]

let _ = execute ~instants:10 (fun _ _ -> scenario ())
