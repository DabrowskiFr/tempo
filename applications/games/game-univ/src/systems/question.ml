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

let process (bus : Bus.t) (world : world) =
  let rec loop () =
    let input = Tempo.await bus.input in
    if input.ask then (
      let closest, d2 = nearest_student world in
      match closest with
      | Some id when d2 <= interaction_radius_sq ->
          let s = world.students.(id) in
          if s.cheating then (
            world.score <- world.score + 10;
            world.message <- Printf.sprintf "Etudiant %d pris en flagrant delit" id;
            Tempo.emit bus.evt
              [ Ask_success id; Ask_feedback ({ x = s.pos.x; y = s.pos.y }, true, "+10 Flagrant delit") ])
          else (
            world.score <- world.score - 4;
            world.message <- Printf.sprintf "Faux positif sur l'etudiant %d" id;
            Tempo.emit bus.evt
              [ Ask_miss (Some id); Ask_feedback ({ x = s.pos.x; y = s.pos.y }, false, "-4 Faux positif") ])
      | _ ->
          world.score <- world.score - 1;
          world.message <- "Personne a interroger a proximite";
          Tempo.emit bus.evt
            [
              Ask_miss None;
              Ask_feedback
                ({ x = world.professor.pos.x; y = world.professor.pos.y -. 30.0 }, false, "-1 Personne proche");
            ]);
    loop ()
  in
  loop ()
