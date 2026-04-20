open Types

let interaction_radius_sq = 55.0 *. 55.0

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

let process (bus : Bus.t) (world : world) =
  let rec loop () =
    let input = Tempo.await bus.input in
    if input.ask && world.started && (not world.paused) && not world.game_over then (
      let closest, d2 = nearest_student world in
      match closest with
      | Some id when d2 <= interaction_radius_sq ->
        let s = world.students.(id) in
        if s.cheating then (
          Tempo.emit bus.evt
            [
              Student_caught id;
              Ask_success id;
              Ask_feedback ({ x = s.pos.x; y = s.pos.y }, true, "Flagrant");
            ])
        else (
          Tempo.emit bus.evt
            [
              Ask_miss (Some id);
              Ask_feedback ({ x = s.pos.x; y = s.pos.y }, false, "Faux positif");
            ])
      | _ ->
          Tempo.emit bus.evt
            [
              Ask_miss None;
              Ask_feedback ({ x = world.professor.pos.x; y = world.professor.pos.y -. 30.0 }, false, "Personne proche");
            ]);
    loop ()
  in
  loop ()
