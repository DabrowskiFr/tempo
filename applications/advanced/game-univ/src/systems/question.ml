open Types

let interaction_radius_sq = 55.0 *. 55.0
let combo_window_frames = 5 * 60

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
  world.pending_audio <- [];
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

let zone_of_x x =
  if x < 320.0 then 0
  else if x < 480.0 then 1
  else 2

let zone_label = function
  | 0 -> "Gauche"
  | 1 -> "Centre"
  | _ -> "Droite"

let profile_label = function
  | Prudent -> "prudent"
  | Opportunist -> "opportuniste"
  | Chaotic -> "chaotique"

let profile_capture_bonus = function
  | Prudent -> 4
  | Opportunist -> 2
  | Chaotic -> 1

let profile_energy_cost = function
  | Prudent -> 1.0
  | Opportunist -> 1.2
  | Chaotic -> 1.4

let caught_cooldown_frames profile difficulty =
  match (difficulty, profile) with
  | 0, Prudent -> 10 * 60
  | 0, Opportunist -> 8 * 60
  | 0, Chaotic -> 7 * 60
  | 1, Prudent -> 7 * 60
  | 1, Opportunist -> 6 * 60
  | 1, Chaotic -> 5 * 60
  | _, Prudent -> 5 * 60
  | _, Opportunist -> 4 * 60
  | _, Chaotic -> 3 * 60

let reset_combo world =
  world.combo <- 0;
  world.combo_window_left <- 0

let register_combo world =
  if world.combo_window_left > 0 then world.combo <- world.combo + 1 else world.combo <- 1;
  world.combo_window_left <- combo_window_frames;
  if world.combo > world.combo_best then world.combo_best <- world.combo

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
          register_combo world;
          let combo_bonus = min 12 ((world.combo - 1) * 2) in
          let zone_bonus = if zone_of_x s.pos.x = world.hot_zone then 2 else 0 in
          let points = 8 + profile_capture_bonus s.profile + combo_bonus + zone_bonus in
          world.score <- world.score + points;
          world.energy <-
            Time_helpers.clamp
              (world.energy -. profile_energy_cost s.profile)
              ~min:0.0 ~max:100.0;
          world.catches <- world.catches + 1;
          s.cheating <- false;
          s.cheat_hold <- 0;
          s.caught_cooldown <- caught_cooldown_frames s.profile world.difficulty;
          s.tell <- 0.0;
          world.message <-
            Printf.sprintf
              "Flagrant delit S%d (%s) : +%d | combo x%d | zone %s"
              id (profile_label s.profile) points world.combo
              (zone_label world.hot_zone);
          let combo_suffix =
            if world.combo > 1 then Printf.sprintf " Combo x%d" world.combo else ""
          in
            Tempo.emit bus.evt
              [
                Ask_success id;
                Ask_feedback
                  ( { x = s.pos.x; y = s.pos.y },
                    true,
                    Printf.sprintf "+%d Flagrant%s" points combo_suffix );
              ])
        else (
          let penalty = 4 + min 3 (world.combo / 2) in
          world.score <- world.score - penalty;
          world.energy <- Time_helpers.clamp (world.energy -. 1.8) ~min:0.0 ~max:100.0;
          world.false_positives <- world.false_positives + 1;
          let had_combo = world.combo > 0 in
          reset_combo world;
          world.message <-
            if had_combo then
              Printf.sprintf "Faux positif sur S%d: combo casse" id
            else Printf.sprintf "Faux positif sur l'etudiant %d" id;
            Tempo.emit bus.evt
              [
                Ask_miss (Some id);
                Ask_feedback
                  ( { x = s.pos.x; y = s.pos.y },
                    false,
                    Printf.sprintf "-%d Faux positif" penalty );
              ])
      | _ ->
          world.score <- world.score - 1;
          world.energy <- Time_helpers.clamp (world.energy -. 0.8) ~min:0.0 ~max:100.0;
          world.empty_checks <- world.empty_checks + 1;
          let had_combo = world.combo > 0 in
          reset_combo world;
          world.message <-
            if had_combo then "Verification vide: combo casse"
            else "Personne a interroger a proximite";
          Tempo.emit bus.evt
            [
              Ask_miss None;
              Ask_feedback
                ({ x = world.professor.pos.x; y = world.professor.pos.y -. 30.0 }, false, "-1 Personne proche");
            ]);
    loop ()
  in
  loop ()
