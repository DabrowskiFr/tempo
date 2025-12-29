open Types

let frame_process (bus : Bus.t) (world : world) =
  let rec loop () =
    let _ = Tempo.await bus.input in
    if world.started && (not world.paused) && not world.game_over then (
      world.round_left <- max 0 (world.round_left - 1);
      if world.round_left = 0 then
        if world.round_index + 1 < world.rounds_total then (
          world.round_index <- world.round_index + 1;
          world.round_left <- world.round_frames;
          world.message <- Printf.sprintf "Manche %d/%d" (world.round_index + 1) world.rounds_total)
        else (
          world.game_over <- true;
          world.message <- "Partie terminee - appuyez sur R pour rejouer"));
    Tempo.emit bus.draw
      [
        Draw_room;
        Draw_hud
          {
            score = world.score;
            flagged = world.flagged;
            message = world.message;
            round_index = world.round_index + 1;
            rounds_total = world.rounds_total;
            round_left = world.round_left;
            started = world.started;
            paused = world.paused;
            game_over = world.game_over;
            energy = world.energy;
            focus_left = world.focus_left;
            cheat_window_factor = world.cheat_window_factor;
            hot_zone_label = "N/A";
            hot_zone_left = 0;
            catches = world.catches;
            false_positives = world.false_positives;
            empty_checks = world.empty_checks;
            combo = world.combo;
            combo_best = world.combo_best;
            combo_window_left = world.combo_window_left;
          };
      ];
    if world.game_over then
      Tempo.emit bus.draw
        [ Draw_results (world.score, world.catches, world.false_positives, world.empty_checks, world.combo_best) ];
    loop ()
  in
  loop ()

let flush (bus : Bus.t) (world : world) =
  let rec loop () =
    let cmds = Tempo.await bus.draw in
    let frame =
      {
        draw = List.rev cmds;
        audio = [];
        score = world.score;
        flagged = world.flagged;
        message = world.message;
      }
    in
    Tempo.emit bus.output frame;
    loop ()
  in
  loop ()
