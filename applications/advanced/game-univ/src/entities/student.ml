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
    loop (frame + 1)
  in
  loop 0
