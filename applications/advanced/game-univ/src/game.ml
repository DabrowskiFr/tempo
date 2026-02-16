open Types

type music_mode =
  | Patrol
  | Tension
  | Results

type music_phase =
  | Morning
  | Afternoon
  | Evening

let initial_world () =
  let professor = { pos = { x = 76.0; y = 170.0 }; speed = 3.2; radius = 16.0 } in
  let min_detection_radius = professor.radius *. 2.0 in
  let max_detection_radius = ((min 760.0 460.0) /. 2.0) -. 25.0 in
  let rounds_total = 3 in
  let round_frames = 45 * 60 in
  let hot_zone_period = 9 * 60 in
  {
    professor;
    students =
      [|
        { id = 0; pos = { x = 224.0; y = 168.0 }; profile = Prudent; cheating = false; cheat_hold = 0; caught_cooldown = 0; tell = 0.0 };
        { id = 1; pos = { x = 384.0; y = 168.0 }; profile = Opportunist; cheating = false; cheat_hold = 0; caught_cooldown = 0; tell = 0.0 };
        { id = 2; pos = { x = 544.0; y = 168.0 }; profile = Chaotic; cheating = false; cheat_hold = 0; caught_cooldown = 0; tell = 0.0 };
        { id = 3; pos = { x = 224.0; y = 388.0 }; profile = Opportunist; cheating = false; cheat_hold = 0; caught_cooldown = 0; tell = 0.0 };
        { id = 4; pos = { x = 384.0; y = 388.0 }; profile = Chaotic; cheating = false; cheat_hold = 0; caught_cooldown = 0; tell = 0.0 };
        { id = 5; pos = { x = 544.0; y = 388.0 }; profile = Prudent; cheating = false; cheat_hold = 0; caught_cooldown = 0; tell = 0.0 };
      |];
    detection_radius = 110.0;
    min_detection_radius;
    max_detection_radius;
    energy = 100.0;
    focus_left = 0;
    coffee_spots =
      [|
        { x = 114.0; y = 530.0 };
        { x = 740.0; y = 520.0 };
        { x = 132.0; y = 120.0 };
        { x = 724.0; y = 122.0 };
      |];
    coffee_active = Some 0;
    coffee_ttl = 14 * 60;
    coffee_respawn = 0;
    coffee_cursor = 1;
    drink_progress = 0.0;
    started = false;
    paused = false;
    difficulty = 1;
    cheat_window_factor = 1.0;
    hot_zone = 0;
    hot_zone_left = hot_zone_period;
    hot_zone_period;
    rounds_total;
    round_frames;
    round_index = 0;
    round_left = round_frames;
    score = 0;
    flagged = 0;
    catches = 0;
    false_positives = 0;
    empty_checks = 0;
    combo = 0;
    combo_best = 0;
    combo_window_left = 0;
    game_over = false;
    message = "Appuyez sur Entree ou Espace pour demarrer";
  }

let process (bus : Bus.t) (world : world) =
  let suspicion = Array.make (Array.length world.students) 0.0 in
  let fx : (vec2 * action_kind * string * int) option ref = ref None in
  let audio_out : audio_cmd list ref = ref [] in
  let stop = Tempo.new_signal () in
  let coffee_pickup_radius_sq = 28.0 *. 28.0 in
  let drink_frames = 72.0 in
  let combo_window_frames = 5 * 60 in
  let suspicion_decay = 0.12 in
  let suspicion_gain = 0.75 in
  let interaction_radius_sq = 55.0 *. 55.0 in
  let music_mode = ref Patrol in
  let music_step = ref 0 in
  let jingle_step = ref (-1) in
  let jingle_score = ref 0 in
  let last_warn_second = ref (-1) in
  let push_cue cue =
    audio_out := Play_cue cue :: !audio_out
  in
  let push_music_note freq duration volume =
    push_cue (Cue_music { freq_hz = freq; duration_s = duration; volume })
  in
  let reset_music_state () =
    music_mode := Patrol;
    music_step := 0;
    jingle_step := -1;
    jingle_score := 0;
    last_warn_second := -1
  in
  let reset_runtime_state () =
    Question.reset_world world;
    Array.fill suspicion 0 (Array.length suspicion) 0.0;
    fx := None;
    audio_out := [];
    reset_music_state ()
  in
  let profile_base profile frame id factor =
    let sf v = int_of_float (float_of_int v *. factor) in
    match profile with
    | Prudent -> ((frame + (id * 13)) mod 280) < sf 72
    | Opportunist -> ((frame + (id * 17)) mod 220) < sf 106
    | Chaotic ->
        let wave = sin ((float_of_int (frame + (id * 37))) *. 0.12) in
        wave > (-0.04 -. ((factor -. 1.0) *. 0.3))
  in
  let zone_of_x x =
    if x < 320.0 then 0
    else if x < 480.0 then 1
    else 2
  in
  let zone_label = function
    | 0 -> "Gauche"
    | 1 -> "Centre"
    | _ -> "Droite"
  in
  let difficulty_name d =
    match d with
    | 0 -> "Facile"
    | 1 -> "Normal"
    | _ -> "Difficile"
  in
  let difficulty_factor d =
    match d with
    | 0 -> 0.75
    | 1 -> 1.0
    | _ -> 1.3
  in
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
  in
  let profile_label = function
    | Prudent -> "prudent"
    | Opportunist -> "opportuniste"
    | Chaotic -> "chaotique"
  in
  let profile_capture_bonus = function
    | Prudent -> 4
    | Opportunist -> 2
    | Chaotic -> 1
  in
  let profile_energy_cost = function
    | Prudent -> 1.0
    | Opportunist -> 1.2
    | Chaotic -> 1.4
  in
  let reset_combo () =
    world.combo <- 0;
    world.combo_window_left <- 0
  in
  let register_combo () =
    if world.combo_window_left > 0 then world.combo <- world.combo + 1 else world.combo <- 1;
    world.combo_window_left <- combo_window_frames;
    if world.combo > world.combo_best then world.combo_best <- world.combo
  in
  let nearest_student () =
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
  in
  let recompute_detection_radius () =
    let t = Time_helpers.clamp (world.energy /. 100.0) ~min:0.0 ~max:1.0 in
    let base =
      world.min_detection_radius
      +. (t *. (world.max_detection_radius -. world.min_detection_radius))
    in
    if world.focus_left > 0 then
      Time_helpers.clamp (base *. 1.15)
        ~min:world.min_detection_radius
        ~max:world.max_detection_radius
    else base
  in
  let activate_next_coffee () =
    if Array.length world.coffee_spots > 0 then (
      world.coffee_active <- Some world.coffee_cursor;
      world.coffee_cursor <- (world.coffee_cursor + 1) mod Array.length world.coffee_spots;
      world.coffee_ttl <- 14 * 60;
      world.drink_progress <- 0.0;
      world.message <- "Un cafe vient d'apparaitre")
  in
  let phase_of_round round_index =
    match (max 1 round_index - 1) mod 3 with
    | 0 -> Morning
    | 1 -> Afternoon
    | _ -> Evening
  in
  let tension_count () =
    let radius_sq = world.detection_radius *. world.detection_radius in
    Array.fold_left
      (fun acc s ->
        if not s.cheating then acc
        else
          let d2 =
            Time_helpers.dist_sq
              (world.professor.pos.x, world.professor.pos.y)
              (s.pos.x, s.pos.y)
          in
          if d2 <= radius_sq then acc + 1 else acc)
      0
      world.students
  in
  let music_tick frame =
    if not world.started || world.paused then ()
    else (
      let phase = phase_of_round (world.round_index + 1) in
      let tension = tension_count () in
      let desired_mode =
        if world.game_over then Results else if tension > 0 then Tension else Patrol
      in
      if desired_mode <> !music_mode then (
        music_mode := desired_mode;
        music_step := 0;
        if desired_mode = Results then (
          jingle_score := world.score;
          jingle_step := 0));
      (match !music_mode with
      | Patrol ->
          let notes, tick, dur, vol =
            match phase with
            | Morning ->
                ([| 261.63; 293.66; 329.63; 349.23; 392.0; 440.0 |], 30, 0.14, 0.14)
            | Afternoon ->
                ([| 293.66; 329.63; 369.99; 392.0; 440.0; 493.88 |], 24, 0.13, 0.16)
            | Evening ->
                ([| 220.0; 246.94; 261.63; 293.66; 329.63; 349.23 |], 34, 0.16, 0.13)
          in
          let pattern =
            match phase with
            | Morning -> [| 0; 2; 3; 2; 0; 2; 4; 2 |]
            | Afternoon -> [| 0; 2; 4; 3; 1; 3; 5; 4 |]
            | Evening -> [| 0; 1; 2; 3; 2; 1; 0; 4 |]
          in
          if frame mod tick = 0 then (
            let note = pattern.(!music_step mod Array.length pattern) in
            push_music_note notes.(note) dur vol;
            incr music_step)
      | Tension ->
          let notes, tick, dur, vol =
            match phase with
            | Morning -> ([| 146.83; 174.61; 196.0; 220.0 |], 16, 0.12, 0.10)
            | Afternoon -> ([| 174.61; 196.0; 220.0; 246.94 |], 14, 0.11, 0.12)
            | Evening -> ([| 130.81; 146.83; 164.81; 185.0 |], 18, 0.13, 0.09)
          in
          let pattern =
            match phase with
            | Morning -> [| 0; 2; 1; 3; 0; 2; 1; 3 |]
            | Afternoon -> [| 0; 3; 1; 2; 0; 3; 2; 1 |]
            | Evening -> [| 0; 1; 0; 2; 1; 3; 1; 2 |]
          in
          if frame mod tick = 0 then (
            let note = pattern.(!music_step mod Array.length pattern) in
            push_music_note notes.(note) dur vol;
            incr music_step)
      | Results ->
          if !jingle_step >= 0 && frame mod 18 = 0 then (
            let src =
              if !jingle_score >= 0 then [| 392.0; 493.88; 587.33; 783.99 |]
              else [| 329.63; 293.66; 246.94; 196.0 |]
            in
            if !jingle_step < Array.length src then
              push_music_note src.(!jingle_step) (if !jingle_score >= 0 then 0.18 else 0.20)
                (if !jingle_score >= 0 then 0.20 else 0.18);
            incr jingle_step;
            if !jingle_step >= Array.length src then jingle_step := -1));
      if (not world.game_over) && world.round_left <= (10 * 60) then (
        if world.round_left mod 60 = 0 then
          let sec = world.round_left / 60 in
          if sec <> !last_warn_second then (
            push_cue Cue_warning;
            last_warn_second := sec))
      else last_warn_second := -1)
  in
  let update_students frame =
    Array.iteri
      (fun i s ->
        let radius_sq = world.detection_radius *. world.detection_radius in
        let d2 =
          Time_helpers.dist_sq
            (world.professor.pos.x, world.professor.pos.y)
            (s.pos.x, s.pos.y)
        in
        let in_range = d2 <= radius_sq in
        let factor = difficulty_factor world.difficulty in
        world.cheat_window_factor <- factor;
        let in_hot_zone = zone_of_x s.pos.x = world.hot_zone in
        let local_factor = factor *. (if in_hot_zone then 1.25 else 0.78) in
        let base = profile_base s.profile frame s.id local_factor in
        let hold_frames =
          let factor_bonus = int_of_float (local_factor *. 20.0) in
          let base_hold =
            match s.profile with
            | Prudent -> 60
            | Opportunist -> 48
            | Chaotic -> 40
          in
          let zone_bonus = if in_hot_zone then 10 else -4 in
          max 24 (base_hold + factor_bonus + zone_bonus)
        in
        let trigger_cheat =
          if world.game_over || not world.started || world.paused then false
          else if s.caught_cooldown > 0 then false
          else
            match s.profile with
            | Prudent -> base && not in_range
            | Opportunist ->
                let window = int_of_float (9.0 *. local_factor) in
                base && (not in_range || ((frame + s.id) mod 44 < window))
            | Chaotic -> base
        in
        if world.game_over || not world.started || world.paused then
          s.cheat_hold <- 0
        else if s.caught_cooldown > 0 then (
          s.caught_cooldown <- s.caught_cooldown - 1;
          s.cheat_hold <- 0)
        else if trigger_cheat then
          s.cheat_hold <- max s.cheat_hold hold_frames
        else if s.cheat_hold > 0 then
          s.cheat_hold <- s.cheat_hold - 1;
        let cheating_now = s.cheat_hold > 0 in
        let tell_up =
          if cheating_now then (if in_hot_zone then 0.08 else 0.05)
          else
            match s.profile with
            | Prudent -> if base then (if in_hot_zone then 0.25 else 0.18) else -0.10
            | Opportunist -> if base then (if in_hot_zone then 0.21 else 0.14) else -0.08
            | Chaotic -> if base then (if in_hot_zone then 0.16 else 0.10) else -0.05
        in
        s.tell <- Time_helpers.clamp (s.tell +. tell_up) ~min:0.0 ~max:1.0;
        s.cheating <- cheating_now;
        let delta =
          if world.game_over || not world.started || world.paused then 0.0
          else if s.cheating && d2 < radius_sq then suspicion_gain
          else -.suspicion_decay
        in
        let prev = suspicion.(i) in
        let next = Time_helpers.clamp (prev +. delta) ~min:0.0 ~max:100.0 in
        if prev < 100.0 && next >= 100.0 then world.flagged <- world.flagged + 1;
        suspicion.(i) <- next)
      world.students
  in
  let ask_action () =
    if world.started && (not world.paused) && not world.game_over then (
      let closest, d2 = nearest_student () in
      match closest with
      | Some id when d2 <= interaction_radius_sq ->
          let s = world.students.(id) in
          if s.cheating then (
            register_combo ();
            let combo_bonus = min 12 ((world.combo - 1) * 2) in
            let zone_bonus = if zone_of_x s.pos.x = world.hot_zone then 2 else 0 in
            let points = 8 + profile_capture_bonus s.profile + combo_bonus + zone_bonus in
            world.score <- world.score + points;
            let energy_cost = profile_energy_cost s.profile in
            world.energy <- Time_helpers.clamp (world.energy -. energy_cost) ~min:0.0 ~max:100.0;
            world.catches <- world.catches + 1;
            s.cheating <- false;
            s.cheat_hold <- 0;
            s.caught_cooldown <- caught_cooldown_frames s.profile world.difficulty;
            s.tell <- 0.0;
            world.message <-
              Printf.sprintf
                "Flagrant delit S%d (%s) : +%d | combo x%d | zone %s"
                id
                (profile_label s.profile)
                points
                world.combo
                (zone_label world.hot_zone);
            let combo_suffix =
              if world.combo > 1 then Printf.sprintf " Combo x%d" world.combo else ""
            in
            fx :=
              Some
                ( { x = s.pos.x; y = s.pos.y },
                  Flagrant,
                  Printf.sprintf "+%d Flagrant%s" points combo_suffix,
                  32 );
            push_cue Cue_success)
          else (
            let penalty = 4 + min 3 (world.combo / 2) in
            world.score <- world.score - penalty;
            world.energy <- Time_helpers.clamp (world.energy -. 1.8) ~min:0.0 ~max:100.0;
            world.false_positives <- world.false_positives + 1;
            let had_combo = world.combo > 0 in
            reset_combo ();
            world.message <-
              if had_combo then
                Printf.sprintf "Faux positif sur S%d: combo casse" id
              else Printf.sprintf "Faux positif sur l'etudiant %d" id;
            fx :=
              Some
                ( { x = s.pos.x; y = s.pos.y },
                  False_positive,
                  Printf.sprintf "-%d Faux positif" penalty,
                  32 );
            push_cue Cue_false_positive)
      | _ ->
          world.score <- world.score - 1;
          world.energy <- Time_helpers.clamp (world.energy -. 0.8) ~min:0.0 ~max:100.0;
          world.empty_checks <- world.empty_checks + 1;
          let had_combo = world.combo > 0 in
          reset_combo ();
          world.message <-
            if had_combo then "Verification vide: combo casse"
            else "Personne a interroger a proximite";
          fx :=
            Some
              ( { x = world.professor.pos.x; y = world.professor.pos.y -. 30.0 },
                Empty_check,
                "-1 Personne proche",
                32 );
          push_cue Cue_empty_check)
  in
  let tick_world frame input =
    if input.restart then reset_runtime_state ();
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
          world.cheat_window_factor <- difficulty_factor world.difficulty;
          world.message <- Printf.sprintf "Difficulte: %s" (difficulty_name world.difficulty))
    | None -> ());
    if input.ask then ask_action ();
    if world.started && (not world.paused) && not world.game_over then (
      world.hot_zone_left <- max 0 (world.hot_zone_left - 1);
      if world.hot_zone_left = 0 then (
        world.hot_zone <- (world.hot_zone + 1) mod 3;
        world.hot_zone_left <- world.hot_zone_period;
        world.message <- Printf.sprintf "Zone d'attention: %s" (zone_label world.hot_zone));
      let moving = input.up || input.down || input.left || input.right in
      Movement.apply_input ~bounds_w:960.0 ~bounds_h:640.0 world.professor input;
      let decay =
        0.018 +. (if moving then 0.035 else 0.0) +. (if world.focus_left > 0 then 0.012 else 0.0)
      in
      world.energy <- Time_helpers.clamp (world.energy -. decay) ~min:0.0 ~max:100.0;
      if world.focus_left > 0 then world.focus_left <- world.focus_left - 1;
      (match world.coffee_active with
      | Some idx ->
          world.coffee_ttl <- max 0 (world.coffee_ttl - 1);
          let coffee = world.coffee_spots.(idx) in
          let d2 =
            Time_helpers.dist_sq
              (world.professor.pos.x, world.professor.pos.y)
              (coffee.x, coffee.y)
          in
          let near = d2 <= coffee_pickup_radius_sq in
          if input.drink && near && not moving then (
            world.drink_progress <-
              Time_helpers.clamp (world.drink_progress +. (1.0 /. drink_frames))
                ~min:0.0 ~max:1.0;
            if world.drink_progress >= 1.0 then (
              world.energy <- Time_helpers.clamp (world.energy +. 55.0) ~min:0.0 ~max:100.0;
              world.focus_left <- 6 * 60;
              world.coffee_active <- None;
              world.coffee_respawn <- 8 * 60;
              world.drink_progress <- 0.0;
              world.message <- "Cafe avale: energie rechargee et focus actif"))
          else world.drink_progress <- max 0.0 (world.drink_progress -. 0.05);
          if world.coffee_ttl = 0 then (
            world.coffee_active <- None;
            world.coffee_respawn <- 6 * 60;
            world.drink_progress <- 0.0;
            world.message <- "Le cafe a refroidi...")
      | None ->
          world.coffee_respawn <- max 0 (world.coffee_respawn - 1);
          if world.coffee_respawn = 0 then activate_next_coffee ());
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
    world.detection_radius <- recompute_detection_radius ();
    update_students frame
  in
  let emit_frame frame =
    music_tick frame;
    let draw = ref [ Draw_room; Draw_hot_zone world.hot_zone ] in
    draw := Draw_professor (world.professor.pos, world.professor.radius, world.detection_radius) :: !draw;
    (match world.coffee_active with
    | Some idx -> draw := Draw_coffee (world.coffee_spots.(idx), world.drink_progress) :: !draw
    | None -> ());
    Array.iteri
      (fun i s ->
        draw :=
          Draw_student (s.id, s.pos, 14.0, s.cheating, suspicion.(i), s.profile, s.tell)
          :: !draw)
      world.students;
    draw :=
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
        }
      :: !draw;
    (match !fx with
    | Some (p, kind, label, ttl) -> draw := Draw_action_fx (p, kind, label, ttl) :: !draw
    | None -> ());
    if world.game_over then
      draw :=
        Draw_results
          (world.score, world.catches, world.false_positives, world.empty_checks, world.combo_best)
        :: !draw;
    Tempo.emit bus.output
      {
        draw = List.rev !draw;
        audio = List.rev !audio_out;
        score = world.score;
        flagged = world.flagged;
        message = world.message;
      };
    (match !fx with
    | Some (p, kind, label, ttl) when ttl > 1 -> fx := Some (p, kind, label, ttl - 1)
    | _ -> fx := None)
  in
  let rec logic_loop frame =
    let inp = Tempo.await bus.input in
    if inp.quit then Tempo.emit stop ()
    else (
      audio_out := [];
      tick_world frame inp;
      emit_frame frame;
      logic_loop (frame + 1))
  in
  Tempo.watch stop (fun () -> logic_loop 0)
