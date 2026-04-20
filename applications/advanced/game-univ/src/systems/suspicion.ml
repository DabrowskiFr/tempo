open Types

let decay = 0.12
let gain = 0.75

let student_process (bus : Bus.t) (world : world) (s : student) =
  let rec loop suspicion =
    let _ = Tempo.await bus.input in
    let radius_sq = world.detection_radius *. world.detection_radius in
    let d2 = Time_helpers.dist_sq (world.professor.pos.x, world.professor.pos.y) (s.pos.x, s.pos.y) in
    let delta =
      if world.game_over || not world.started || world.paused then 0.0
      else if s.cheating && d2 < radius_sq then gain
      else -.decay
    in
    let next = Time_helpers.clamp (suspicion +. delta) ~min:0.0 ~max:100.0 in
    if suspicion < 100.0 && next >= 100.0 then Tempo.emit bus.evt [ Student_flagged s.id ];
    Tempo.emit bus.draw
      [ Draw_student (s.id, s.pos, 14.0, s.cheating, next, s.profile, s.tell) ];
    loop next
  in
  loop 0.0

let process (bus : Bus.t) (world : world) =
  let branches = Array.to_list (Array.map (fun s () -> student_process bus world s) world.students) in
  Tempo.parallel branches
