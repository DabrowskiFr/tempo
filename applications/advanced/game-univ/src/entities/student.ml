open Types

let profile_base profile frame id factor =
  let sf v = int_of_float (float_of_int v *. factor) in
  match profile with
  | Prudent -> ((frame + (id * 13)) mod 280) < sf 72
  | Opportunist -> ((frame + (id * 17)) mod 220) < sf 106
  | Chaotic ->
      let wave = sin ((float_of_int (frame + (id * 37))) *. 0.12) in
      wave > (-0.04 -. ((factor -. 1.0) *. 0.3))

let process (bus : Bus.t) (world : world) (student : student) =
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
  let rec loop frame =
    let _ = Tempo.await bus.input in
    let radius_sq = world.detection_radius *. world.detection_radius in
    let d2 =
      Time_helpers.dist_sq
        (world.professor.pos.x, world.professor.pos.y)
        (student.pos.x, student.pos.y)
    in
    let in_range = d2 <= radius_sq in
    let base =
      profile_base student.profile frame student.id world.cheat_window_factor
    in
    let cheating_now =
      if world.game_over || not world.started || world.paused then false
      else if student.caught_cooldown > 0 then false
      else
        match student.profile with
        | Prudent -> base && not in_range
        | Opportunist ->
            let window =
              int_of_float
                (9.0 *. world.cheat_window_factor)
            in
            base && (not in_range || ((frame + student.id) mod 44 < window))
        | Chaotic -> base
    in
    let tell_up = if base && not cheating_now then 0.18 else -0.08 in
    student.tell <- Time_helpers.clamp (student.tell +. tell_up) ~min:0.0 ~max:1.0;
    if cheating_now <> student.cheating then
      Tempo.emit bus.evt
        [ if cheating_now then Cheating_start student.id else Cheating_stop student.id ];
    student.cheating <- cheating_now;
    if student.caught_cooldown > 0 then student.caught_cooldown <- student.caught_cooldown - 1;
    if cheating_now && in_range && base then
      student.cheat_hold <- caught_cooldown_frames student.profile world.difficulty;
    loop (frame + 1)
  in
  loop 0
