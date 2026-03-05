open Types

type music_mode =
  | Patrol
  | Tension
  | Results

type music_phase =
  | Morning
  | Afternoon
  | Evening

let phase_of_round round_index =
  match (max 1 round_index - 1) mod 3 with
  | 0 -> Morning
  | 1 -> Afternoon
  | _ -> Evening

let tension_count (world : world) =
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

let process (bus : Bus.t) (world : world) =
  let music_mode = ref Patrol in
  let patrol_steps = Array.make 3 0 in
  let tension_steps = Array.make 3 0 in
  let jingle_step = ref 0 in
  let jingle_score = ref 0 in
  let last_warn_second = ref (-1) in
  let emit_cue cue = Tempo.emit bus.audio [ Play_cue cue ] in
  let emit_music freq duration volume =
    emit_cue (Cue_music { freq_hz = freq; duration_s = duration; volume })
  in
  let is_running () = world.started && not world.paused && not world.game_over in
  let current_phase () = phase_of_round (world.round_index + 1) in
  let phase_index = function Morning -> 0 | Afternoon -> 1 | Evening -> 2 in
  let reset_all_steps () =
    Array.fill patrol_steps 0 (Array.length patrol_steps) 0;
    Array.fill tension_steps 0 (Array.length tension_steps) 0;
    jingle_step := 0
  in
  let run_pattern_if ~mode ~phase ~steps ~notes ~pattern ~duration_s ~volume () =
    if is_running () && !music_mode = mode && current_phase () = phase then (
      let idx = phase_index phase in
      let note = pattern.(steps.(idx) mod Array.length pattern) in
      emit_music notes.(note) duration_s volume;
      steps.(idx) <- steps.(idx) + 1)
    else
      steps.(phase_index phase) <- 0
  in
  let rec event_loop () =
    let events = Tempo.await bus.evt in
    List.iter
      (function
        | Ask_success _ -> emit_cue Cue_success
        | Ask_miss (Some _) -> emit_cue Cue_false_positive
        | Ask_miss None -> emit_cue Cue_empty_check
        | _ -> ())
      events;
    event_loop ()
  in
  let rec state_loop () =
    let _ = Tempo.await bus.input in
    let desired_mode =
      if world.game_over then Results
      else if tension_count world > 0 then Tension
      else Patrol
    in
    if desired_mode <> !music_mode then (
      music_mode := desired_mode;
      reset_all_steps ();
      if desired_mode = Results then jingle_score := world.score);
    if world.game_over || world.round_left > (10 * 60) then last_warn_second := -1;
    state_loop ()
  in
  let warning_loop () =
    Tempo.Constructs.every_n 60 (fun () ->
        if (not world.game_over) && is_running () && world.round_left <= (10 * 60) then
          let sec = world.round_left / 60 in
          if sec <> !last_warn_second then (
            emit_cue Cue_warning;
            last_warn_second := sec))
  in
  let patrol_loop () =
    Tempo.parallel
      [
        (fun () ->
          Tempo.Constructs.every_n 30
            (run_pattern_if
               ~mode:Patrol
               ~phase:Morning
               ~steps:patrol_steps
               ~notes:[| 261.63; 293.66; 329.63; 349.23; 392.0; 440.0 |]
               ~pattern:[| 0; 2; 3; 2; 0; 2; 4; 2 |]
               ~duration_s:0.14
               ~volume:0.14));
        (fun () ->
          Tempo.Constructs.every_n 24
            (run_pattern_if
               ~mode:Patrol
               ~phase:Afternoon
               ~steps:patrol_steps
               ~notes:[| 293.66; 329.63; 369.99; 392.0; 440.0; 493.88 |]
               ~pattern:[| 0; 2; 4; 3; 1; 3; 5; 4 |]
               ~duration_s:0.13
               ~volume:0.16));
        (fun () ->
          Tempo.Constructs.every_n 34
            (run_pattern_if
               ~mode:Patrol
               ~phase:Evening
               ~steps:patrol_steps
               ~notes:[| 220.0; 246.94; 261.63; 293.66; 329.63; 349.23 |]
               ~pattern:[| 0; 1; 2; 3; 2; 1; 0; 4 |]
               ~duration_s:0.16
               ~volume:0.13));
      ]
  in
  let tension_loop () =
    Tempo.parallel
      [
        (fun () ->
          Tempo.Constructs.every_n 16
            (run_pattern_if
               ~mode:Tension
               ~phase:Morning
               ~steps:tension_steps
               ~notes:[| 146.83; 174.61; 196.0; 220.0 |]
               ~pattern:[| 0; 2; 1; 3; 0; 2; 1; 3 |]
               ~duration_s:0.12
               ~volume:0.10));
        (fun () ->
          Tempo.Constructs.every_n 14
            (run_pattern_if
               ~mode:Tension
               ~phase:Afternoon
               ~steps:tension_steps
               ~notes:[| 174.61; 196.0; 220.0; 246.94 |]
               ~pattern:[| 0; 3; 1; 2; 0; 3; 2; 1 |]
               ~duration_s:0.11
               ~volume:0.12));
        (fun () ->
          Tempo.Constructs.every_n 18
            (run_pattern_if
               ~mode:Tension
               ~phase:Evening
               ~steps:tension_steps
               ~notes:[| 130.81; 146.83; 164.81; 185.0 |]
               ~pattern:[| 0; 1; 0; 2; 1; 3; 1; 2 |]
               ~duration_s:0.13
               ~volume:0.09));
      ]
  in
  let results_loop () =
    Tempo.Constructs.every_n 18 (fun () ->
        if !music_mode = Results then (
          let src =
            if !jingle_score >= 0 then [| 392.0; 493.88; 587.33; 783.99 |]
            else [| 329.63; 293.66; 246.94; 196.0 |]
          in
          if !jingle_step < Array.length src then
            emit_music src.(!jingle_step)
              (if !jingle_score >= 0 then 0.18 else 0.20)
              (if !jingle_score >= 0 then 0.20 else 0.18);
          jingle_step := (!jingle_step + 1) mod Array.length src)
        else jingle_step := 0)
  in
  Tempo.parallel [ event_loop; state_loop; warning_loop; patrol_loop; tension_loop; results_loop ]
