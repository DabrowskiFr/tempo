open Types

let frame_process (bus : Bus.t) (world : world) =
  let rec loop () =
    let _ = Tempo.await bus.input in
    Tempo.emit bus.draw
      [
        Draw_room;
        Draw_hud
          ( world.score,
            world.flagged,
            world.message,
            world.detection_radius,
            world.min_detection_radius,
            world.max_detection_radius );
      ];
    loop ()
  in
  loop ()

let flush (bus : Bus.t) (world : world) =
  let rec loop () =
    let cmds = Tempo.await bus.draw in
    let frame =
      {
        draw = List.rev cmds;
        score = world.score;
        flagged = world.flagged;
        message = world.message;
      }
    in
    Tempo.emit bus.output frame;
    loop ()
  in
  loop ()
