open Types

let interaction_radius_sq = 55.0 *. 55.0

let difficulty_factor = function
  | 0 -> 0.75
  | 1 -> 1.0
  | _ -> 1.3

let reset_world (world : world) =
  world.professor.pos.x <- 76.0;
  world.professor.pos.y <- 170.0;
  world.detection_radius <- 110.0;
  world.energy <- 100.0;
  world.focus_left <- 0;
  world.coffee_active <- Some 0;
  world.coffee_ttl <- 14 * 60;
  world.coffee_respawn <- 0;
  world.coffee_cursor <- 1;
  world.drink_progress <- 0.0;
  world.started <- false;
  world.paused <- false;
  world.difficulty <- 1;
  world.cheat_window_factor <- 1.0;
  world.hot_zone <- 0;
  world.hot_zone_left <- world.hot_zone_period;
  world.round_index <- 0;
  world.round_left <- world.round_frames;
  world.score <- 0;
  world.flagged <- 0;
  world.catches <- 0;
  world.false_positives <- 0;
  world.empty_checks <- 0;
  world.combo <- 0;
  world.combo_best <- 0;
  world.combo_window_left <- 0;
  world.game_over <- false;
  world.message <- "Appuyez sur Entree ou Espace pour demarrer";
  Array.iter
    (fun s ->
      s.cheating <- false;
      s.cheat_hold <- 0;
      s.caught_cooldown <- 0;
      s.tell <- 0.0)
    world.students

let nearest_student (world : world) =
  let best_id = ref None in
  let best_d2 = ref max_float in
  Array.iter
    (fun s ->
      let d2 =
        Time_helpers.dist_sq (world.professor.pos.x, world.professor.pos.y) (s.pos.x, s.pos.y)
      in
      if d2 < !best_d2 then (
        best_d2 := d2;
        best_id := Some s.id))
    world.students;
  (!best_id, !best_d2)

let process (bus : Bus.t) (world : world) =
  let rec loop () =
    let input = Tempo.await bus.input in
    (match input.difficulty_set with
    | Some d ->
        let d = Time_helpers.clamp (float_of_int d) ~min:0.0 ~max:2.0 |> int_of_float in
        world.difficulty <- d;
        world.cheat_window_factor <- difficulty_factor d
    | None -> ());
    if input.ask && world.started && (not world.paused) && not world.game_over then (
      let closest, d2 = nearest_student world in
      match closest with
      | Some id when d2 <= interaction_radius_sq ->
        let s = world.students.(id) in
        if s.cheating then (
          world.score <- world.score + 10;
          world.energy <- Time_helpers.clamp (world.energy -. 1.2) ~min:0.0 ~max:100.0;
          world.catches <- world.catches + 1;
          world.message <- Printf.sprintf "Etudiant %d pris en flagrant delit" id;
            Tempo.emit bus.evt
              [ Ask_success id; Ask_feedback ({ x = s.pos.x; y = s.pos.y }, true, "+10 Flagrant delit") ])
        else (
          world.score <- world.score - 4;
          world.energy <- Time_helpers.clamp (world.energy -. 1.8) ~min:0.0 ~max:100.0;
          world.false_positives <- world.false_positives + 1;
            world.message <- Printf.sprintf "Faux positif sur l'etudiant %d" id;
            Tempo.emit bus.evt
              [ Ask_miss (Some id); Ask_feedback ({ x = s.pos.x; y = s.pos.y }, false, "-4 Faux positif") ])
      | _ ->
          world.score <- world.score - 1;
          world.energy <- Time_helpers.clamp (world.energy -. 0.8) ~min:0.0 ~max:100.0;
          world.empty_checks <- world.empty_checks + 1;
          world.message <- "Personne a interroger a proximite";
          Tempo.emit bus.evt
            [
              Ask_miss None;
              Ask_feedback
                ({ x = world.professor.pos.x; y = world.professor.pos.y -. 30.0 }, false, "-1 Personne proche");
            ]);
    loop ()
  in
  loop ()
