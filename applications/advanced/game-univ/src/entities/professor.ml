open Types

let process (bus : Bus.t) (world : world) =
  let coffee_pickup_radius_sq = 28.0 *. 28.0 in
  let drink_frames = 72.0 in
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
  let rec movement_process () =
    let input = Tempo.await bus.input in
    if world.started && (not world.paused) && not world.game_over then
      Movement.apply_input ~bounds_w:960.0 ~bounds_h:640.0 world.professor input;
    movement_process ()
  in
  let rec stamina_process () =
    let input = Tempo.await bus.input in
    if world.started && (not world.paused) && not world.game_over then (
      let moving = input.up || input.down || input.left || input.right in
      let decay =
        0.018
        +. (if moving then 0.035 else 0.0)
        +. (if world.focus_left > 0 then 0.012 else 0.0)
      in
      world.energy <- Time_helpers.clamp (world.energy -. decay) ~min:0.0 ~max:100.0;
      if world.focus_left > 0 then world.focus_left <- world.focus_left - 1;
      world.detection_radius <- recompute_detection_radius ());
    stamina_process ()
  in
  let rec energy_event_process () =
    let deltas = Tempo.await bus.energy in
    let total = List.fold_left ( +. ) 0.0 deltas in
    if total <> 0.0 then (
      world.energy <- Time_helpers.clamp (world.energy +. total) ~min:0.0 ~max:100.0;
      world.detection_radius <- recompute_detection_radius ());
    energy_event_process ()
  in
  let rec coffee_process () =
    let input = Tempo.await bus.input in
    if world.started && (not world.paused) && not world.game_over then (
      let moving = input.up || input.down || input.left || input.right in
      match world.coffee_active with
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
            world.drink_progress <- Time_helpers.clamp (world.drink_progress +. (1.0 /. drink_frames)) ~min:0.0 ~max:1.0;
            if world.drink_progress >= 1.0 then (
              world.energy <- Time_helpers.clamp (world.energy +. 55.0) ~min:0.0 ~max:100.0;
              world.focus_left <- 6 * 60;
              world.coffee_active <- None;
              world.coffee_respawn <- 8 * 60;
              world.drink_progress <- 0.0;
              world.detection_radius <- recompute_detection_radius ();
              Tempo.emit bus.status [ "Cafe avale: energie rechargee et focus actif" ]))
          else world.drink_progress <- max 0.0 (world.drink_progress -. 0.05);
          if world.coffee_ttl = 0 then (
            world.coffee_active <- None;
            world.coffee_respawn <- 6 * 60;
            world.drink_progress <- 0.0;
            Tempo.emit bus.status [ "Le cafe a refroidi..." ])
      | None ->
          world.coffee_respawn <- max 0 (world.coffee_respawn - 1);
          if world.coffee_respawn = 0 then activate_next_coffee ());
    coffee_process ()
  in
  let rec restart_process () =
    let () = Tempo.await bus.restart in
    world.professor.pos.x <- 76.0;
    world.professor.pos.y <- 170.0;
    world.detection_radius <- 110.0;
    world.energy <- 100.0;
    world.focus_left <- 0;
    world.coffee_active <- Some 0;
    world.coffee_ttl <- 14 * 60;
    world.coffee_respawn <- 0;
    world.coffee_cursor <- 1;
    world.drink_progress <- 0.0;
    restart_process ()
  in
  let rec draw_process () =
    let _ = Tempo.await bus.input in
    Tempo.emit bus.draw
      [ Draw_professor (world.professor.pos, world.professor.radius, world.detection_radius) ];
    (match world.coffee_active with
    | Some idx -> Tempo.emit bus.draw [ Draw_coffee (world.coffee_spots.(idx), world.drink_progress) ]
    | None -> ());
    draw_process ()
  in
  Tempo.parallel
    [ movement_process; stamina_process; energy_event_process; coffee_process; restart_process; draw_process ]
