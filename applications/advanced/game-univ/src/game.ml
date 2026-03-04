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
  let stop = Tempo.new_signal () in
  let quit_watcher () =
    let rec loop () =
      let input = Tempo.await bus.input in
      if input.quit then Tempo.emit stop () else loop ()
    in
    loop ()
  in
  let student_processes =
    Array.to_list (Array.map (fun student () -> Student.process bus world student) world.students)
  in
  let runtime_processes =
    [
      (fun () -> Control.process bus world);
      (fun () -> Clock.process bus world);
      (fun () -> Professor.process bus world);
      (fun () -> Suspicion.process bus world);
      (fun () -> Flag_counter.process bus world);
      (fun () -> Question.process bus world);
      (fun () -> Action_feedback.process bus);
      (fun () -> Audio_logic.process bus world);
      (fun () -> Render.frame_process bus world);
      (fun () -> Render.flush bus world);
    ]
    @ student_processes
  in
  Tempo.parallel
    [
      (fun () ->
        Tempo.Constructs.supervise_until stop (fun () -> Tempo.parallel runtime_processes));
      quit_watcher;
    ]
