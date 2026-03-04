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
  let music_step = ref 0 in
  let jingle_step = ref (-1) in
  let jingle_score = ref 0 in
  let last_warn_second = ref (-1) in
  let emit_cue cue = world.pending_audio <- Play_cue cue :: world.pending_audio in
  let emit_music freq duration volume =
    emit_cue (Cue_music { freq_hz = freq; duration_s = duration; volume })
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
  let rec music_loop frame =
    let _ = Tempo.await bus.input in
    if world.started && not world.paused then (
      let phase = phase_of_round (world.round_index + 1) in
      let tension = tension_count world in
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
            | Morning -> ([| 261.63; 293.66; 329.63; 349.23; 392.0; 440.0 |], 30, 0.14, 0.14)
            | Afternoon -> ([| 293.66; 329.63; 369.99; 392.0; 440.0; 493.88 |], 24, 0.13, 0.16)
            | Evening -> ([| 220.0; 246.94; 261.63; 293.66; 329.63; 349.23 |], 34, 0.16, 0.13)
          in
          let pattern =
            match phase with
            | Morning -> [| 0; 2; 3; 2; 0; 2; 4; 2 |]
            | Afternoon -> [| 0; 2; 4; 3; 1; 3; 5; 4 |]
            | Evening -> [| 0; 1; 2; 3; 2; 1; 0; 4 |]
          in
          if frame mod tick = 0 then (
            let note = pattern.(!music_step mod Array.length pattern) in
            emit_music notes.(note) dur vol;
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
            emit_music notes.(note) dur vol;
            incr music_step)
      | Results ->
          if !jingle_step >= 0 && frame mod 18 = 0 then (
            let src =
              if !jingle_score >= 0 then [| 392.0; 493.88; 587.33; 783.99 |]
              else [| 329.63; 293.66; 246.94; 196.0 |]
            in
            if !jingle_step < Array.length src then
              emit_music src.(!jingle_step)
                (if !jingle_score >= 0 then 0.18 else 0.20)
                (if !jingle_score >= 0 then 0.20 else 0.18);
            incr jingle_step;
            if !jingle_step >= Array.length src then jingle_step := -1));
      if (not world.game_over) && world.round_left <= (10 * 60) then (
        if world.round_left mod 60 = 0 then
          let sec = world.round_left / 60 in
          if sec <> !last_warn_second then (
            emit_cue Cue_warning;
            last_warn_second := sec))
      else last_warn_second := -1);
    music_loop (frame + 1)
  in
  Tempo.parallel [ event_loop; (fun () -> music_loop 0) ]
