open Types

let initial_world () =
  let professor = { pos = { x = 120.0; y = 120.0 }; speed = 3.2; radius = 16.0 } in
  let min_detection_radius = professor.radius *. 2.0 in
  let max_detection_radius = ((min 760.0 460.0) /. 2.0) -. 25.0 in
  {
    professor;
    students =
      [|
        { id = 0; pos = { x = 224.0; y = 168.0 }; cheating = false };
        { id = 1; pos = { x = 384.0; y = 168.0 }; cheating = false };
        { id = 2; pos = { x = 544.0; y = 168.0 }; cheating = false };
        { id = 3; pos = { x = 224.0; y = 388.0 }; cheating = false };
        { id = 4; pos = { x = 384.0; y = 388.0 }; cheating = false };
        { id = 5; pos = { x = 544.0; y = 388.0 }; cheating = false };
      |];
    detection_radius = 110.0;
    min_detection_radius;
    max_detection_radius;
    score = 0;
    flagged = 0;
    message = "Patrouillez, E pour interroger, [/] pour rayon";
  }

let process bus world =
  let student_processes =
    Array.to_list (Array.map (fun s () -> Student.process bus s) world.students)
  in
  let stop = Tempo.new_signal () in
  let gameplay () =
    Tempo.parallel
      ((fun () -> Professor.process bus world)
      :: (fun () -> Suspicion.process bus world)
      :: (fun () -> Flag_counter.process bus world)
      :: (fun () -> Question.process bus world)
      :: (fun () -> Action_feedback.process bus)
      :: (fun () -> Render.frame_process bus world)
      :: (fun () -> Render.flush bus world)
      :: student_processes)
  in
  let quit_watcher () =
    let rec loop () =
      let inp = Tempo.await bus.input in
      if inp.quit then Tempo.emit stop () else loop ()
    in
    loop ()
  in
  Tempo.parallel [ (fun () -> Tempo.watch stop gameplay); quit_watcher ]
