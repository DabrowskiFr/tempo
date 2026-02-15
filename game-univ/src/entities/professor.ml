open Types

let process (bus : Bus.t) (world : world) =
  let radius_step = 3.0 in
  let rec loop () =
    let input = Tempo.await bus.input in
    (match input.radius_set_t with
    | Some t ->
        let t = Time_helpers.clamp t ~min:0.0 ~max:1.0 in
        world.detection_radius <-
          world.min_detection_radius
          +. (t *. (world.max_detection_radius -. world.min_detection_radius))
    | None -> ());
    if input.radius_down then
      world.detection_radius <-
        Time_helpers.clamp
          (world.detection_radius -. radius_step)
          ~min:world.min_detection_radius
          ~max:world.max_detection_radius;
    if input.radius_up then
      world.detection_radius <-
        Time_helpers.clamp
          (world.detection_radius +. radius_step)
          ~min:world.min_detection_radius
          ~max:world.max_detection_radius;
    Movement.apply_input ~bounds_w:960.0 ~bounds_h:640.0 world.professor input;
    Tempo.emit bus.draw
      [ Draw_professor (world.professor.pos, world.professor.radius, world.detection_radius) ];
    loop ()
  in
  loop ()
