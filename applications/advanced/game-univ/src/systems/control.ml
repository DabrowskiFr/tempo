open Types

let difficulty_name = function
  | 0 -> "Facile"
  | 1 -> "Normal"
  | _ -> "Difficile"

let difficulty_factor = function
  | 0 -> 0.75
  | 1 -> 1.0
  | _ -> 1.3

let process (bus : Bus.t) (world : world) =
  let rec loop () =
    let input = Tempo.await bus.input in
    if input.restart then Question.reset_world world;
    if input.start && (not world.started) && not world.game_over then (
      world.started <- true;
      world.paused <- false;
      world.message <- Printf.sprintf "Manche %d/%d" (world.round_index + 1) world.rounds_total);
    if input.pause_toggle && world.started && not world.game_over then (
      world.paused <- not world.paused;
      world.message <- if world.paused then "Pause" else "Reprise");
    (match input.difficulty_set with
    | Some d ->
        let d = Time_helpers.clamp (float_of_int d) ~min:0.0 ~max:2.0 |> int_of_float in
        if d <> world.difficulty then (
          world.difficulty <- d;
          world.cheat_window_factor <- difficulty_factor d;
          world.message <- Printf.sprintf "Difficulte: %s" (difficulty_name d))
    | None -> ());
    loop ()
  in
  loop ()
