open Types

let combo_window_frames = 5 * 60

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

let reset_combo world =
  world.combo <- 0;
  world.combo_window_left <- 0

let register_combo world =
  if world.combo_window_left > 0 then world.combo <- world.combo + 1 else world.combo <- 1;
  world.combo_window_left <- combo_window_frames;
  if world.combo > world.combo_best then world.combo_best <- world.combo

let apply_success world id =
  let s = world.students.(id) in
  register_combo world;
  let combo_bonus = min 12 ((world.combo - 1) * 2) in
  let zone_bonus = if zone_of_x s.pos.x = world.hot_zone then 2 else 0 in
  let points = 8 + profile_capture_bonus s.profile + combo_bonus + zone_bonus in
  world.score <- world.score + points;
  world.catches <- world.catches + 1;
  ( points,
    -.profile_energy_cost s.profile,
    Printf.sprintf
      "Flagrant delit S%d (%s) : +%d | combo x%d | zone %s"
      id (profile_label s.profile) points world.combo
      (zone_label world.hot_zone) )

let apply_false_positive world id =
  let penalty = 4 + min 3 (world.combo / 2) in
  let had_combo = world.combo > 0 in
  world.score <- world.score - penalty;
  world.false_positives <- world.false_positives + 1;
  reset_combo world;
  ( -.1.8,
    if had_combo then
      Printf.sprintf "Faux positif sur S%d: combo casse" id
    else Printf.sprintf "Faux positif sur l'etudiant %d" id )

let apply_empty_check world =
  let had_combo = world.combo > 0 in
  world.score <- world.score - 1;
  world.empty_checks <- world.empty_checks + 1;
  reset_combo world;
  ( -.0.8,
    if had_combo then "Verification vide: combo casse"
    else "Personne a interroger a proximite" )

let process (bus : Bus.t) (world : world) =
  let rec combo_window_process () =
    let _ = Tempo.await bus.input in
    if world.started && (not world.paused) && not world.game_over && world.combo_window_left > 0 then (
      world.combo_window_left <- world.combo_window_left - 1;
      if world.combo_window_left = 0 then world.combo <- 0);
    combo_window_process ()
  in
  let rec events_process () =
    let events = Tempo.await bus.evt in
    List.iter
      (function
        | Ask_success id ->
            let _, delta, msg = apply_success world id in
            Tempo.emit bus.energy [ delta ];
            Tempo.emit bus.status [ msg ]
        | Ask_miss (Some id) ->
            let delta, msg = apply_false_positive world id in
            Tempo.emit bus.energy [ delta ];
            Tempo.emit bus.status [ msg ]
        | Ask_miss None ->
            let delta, msg = apply_empty_check world in
            Tempo.emit bus.energy [ delta ];
            Tempo.emit bus.status [ msg ]
        | _ -> ())
      events;
    events_process ()
  in
  let rec restart_process () =
    let () = Tempo.await bus.restart in
    world.score <- 0;
    world.catches <- 0;
    world.false_positives <- 0;
    world.empty_checks <- 0;
    world.combo <- 0;
    world.combo_best <- 0;
    world.combo_window_left <- 0;
    restart_process ()
  in
  Tempo.parallel [ combo_window_process; events_process; restart_process ]
