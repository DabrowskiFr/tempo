open Types

let zone_label = function
  | 0 -> "Gauche"
  | 1 -> "Centre"
  | _ -> "Droite"

let frame_process (bus : Bus.t) (world : world) =
  let rec loop () =
    let _ = Tempo.await bus.input in
    Tempo.emit bus.draw
      [
        Draw_room;
        Draw_hot_zone world.hot_zone;
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
            hot_zone_label = zone_label world.hot_zone;
            hot_zone_left = world.hot_zone_left;
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
  let pending_audio = ref [] in
  let rec audio_collector () =
    let cmds = Tempo.await bus.audio in
    pending_audio := List.rev_append cmds !pending_audio;
    audio_collector ()
  in
  let rec draw_loop () =
    let cmds = Tempo.await bus.draw in
    let audio = List.rev !pending_audio in
    pending_audio := [];
    let frame =
      {
        draw = List.rev cmds;
        audio;
        score = world.score;
        flagged = world.flagged;
        message = world.message;
      }
    in
    Tempo.emit bus.output frame;
    draw_loop ()
  in
  Tempo.parallel [ audio_collector; draw_loop ]
