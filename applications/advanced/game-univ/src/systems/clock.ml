open Types

let zone_label = function
  | 0 -> "Gauche"
  | 1 -> "Centre"
  | _ -> "Droite"

let process (bus : Bus.t) (world : world) =
  let rec loop () =
    let _ = Tempo.await bus.input in
    if world.started && (not world.paused) && not world.game_over then (
      world.hot_zone_left <- max 0 (world.hot_zone_left - 1);
      if world.hot_zone_left = 0 then (
        world.hot_zone <- (world.hot_zone + 1) mod 3;
        world.hot_zone_left <- world.hot_zone_period;
        world.message <- Printf.sprintf "Zone d'attention: %s" (zone_label world.hot_zone));
      if world.combo_window_left > 0 then (
        world.combo_window_left <- world.combo_window_left - 1;
        if world.combo_window_left = 0 then world.combo <- 0);
      world.round_left <- max 0 (world.round_left - 1);
      if world.round_left = 0 then
        if world.round_index + 1 < world.rounds_total then (
          world.round_index <- world.round_index + 1;
          world.round_left <- world.round_frames;
          world.message <- Printf.sprintf "Manche %d/%d" (world.round_index + 1) world.rounds_total)
        else (
          world.game_over <- true;
          world.message <- "Partie terminee - appuyez sur R pour rejouer"));
    loop ()
  in
  loop ()
