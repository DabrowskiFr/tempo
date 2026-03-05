open Types

let emit_window signal duration =
  let rec loop remaining =
    if remaining <= 0 then ()
    else (
      Tempo.emit signal ();
      Tempo.pause ();
      loop (remaining - 1))
  in
  loop duration

let schedule_window signal ~initial_delay ~period ~duration =
  let rec cycle delay =
    Tempo.Constructs.after_n delay (fun () ->
        Tempo.parallel
          [
            (fun () -> emit_window signal (max 1 (duration ())));
            (fun () -> cycle period);
          ])
  in
  cycle initial_delay

let process (bus : Bus.t) (world : world) (student : student) =
  let base_signal = Tempo.new_signal () in
  let opportunist_phase = ref 0 in
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
  let base_window_process () =
    match student.profile with
    | Prudent ->
        schedule_window base_signal
          ~initial_delay:((student.id * 13) mod 280)
          ~period:280
          ~duration:(fun () ->
            int_of_float (72.0 *. world.cheat_window_factor))
    | Opportunist ->
        schedule_window base_signal
          ~initial_delay:((student.id * 17) mod 220)
          ~period:220
          ~duration:(fun () ->
            int_of_float (106.0 *. world.cheat_window_factor))
    | Chaotic ->
        schedule_window base_signal
          ~initial_delay:((student.id * 37) mod 96)
          ~period:96
          ~duration:(fun () ->
            int_of_float (max 18.0 (30.0 +. ((world.cheat_window_factor -. 1.0) *. 16.0))))
  in
  let rec event_process () =
    let events = Tempo.await bus.evt in
    List.iter
      (function
        | Student_caught id when id = student.id ->
            student.cheating <- false;
            student.cheat_hold <- 0;
            student.caught_cooldown <- caught_cooldown_frames student.profile world.difficulty;
            student.tell <- 0.0
        | _ -> ())
      events;
    event_process ()
  in
  let rec restart_process () =
    let () = Tempo.await bus.restart in
    student.cheating <- false;
    student.cheat_hold <- 0;
    student.caught_cooldown <- 0;
    student.tell <- 0.0;
    restart_process ()
  in
  let rec behavior_process () =
    let _ = Tempo.await bus.input in
    let radius_sq = world.detection_radius *. world.detection_radius in
    let d2 =
      Time_helpers.dist_sq
        (world.professor.pos.x, world.professor.pos.y)
        (student.pos.x, student.pos.y)
    in
    let in_range = d2 <= radius_sq in
    let base = Tempo.Low_level.is_present base_signal in
    let cheating_now =
      if world.game_over || not world.started || world.paused then false
      else if student.caught_cooldown > 0 then false
      else
        match student.profile with
        | Prudent -> base && not in_range
        | Opportunist ->
            let opportunistic_window =
              int_of_float (9.0 *. world.cheat_window_factor)
            in
            base && (not in_range || (!opportunist_phase mod 44) < opportunistic_window)
        | Chaotic -> base
    in
    let tell_up = if base && not cheating_now then 0.18 else -0.08 in
    student.tell <- Time_helpers.clamp (student.tell +. tell_up) ~min:0.0 ~max:1.0;
    if cheating_now <> student.cheating then
      Tempo.emit bus.evt
        [ if cheating_now then Cheating_start student.id else Cheating_stop student.id ];
    student.cheating <- cheating_now;
    opportunist_phase := !opportunist_phase + 1;
    if student.caught_cooldown > 0 then student.caught_cooldown <- student.caught_cooldown - 1;
    behavior_process ()
  in
  Tempo.parallel [ base_window_process; event_process; restart_process; behavior_process ]
