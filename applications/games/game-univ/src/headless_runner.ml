open Types

let input_for_frame i =
  {
    up = false;
    down = i mod 160 >= 100 && i mod 160 < 118;
    left = false;
    right = i mod 160 < 80;
    ask = i mod 90 = 0;
    radius_down = false;
    radius_up = false;
    radius_set_t = None;
    quit = false;
  }

let build_script instants =
  Array.init (instants + 1) (fun i ->
      if i = instants then
        Some
          {
            up = false;
            down = false;
            left = false;
            right = false;
            ask = false;
            radius_down = false;
            radius_up = false;
            radius_set_t = None;
            quit = true;
          }
      else Some (input_for_frame i))

let run_simulation instants =
  let world = Game.initial_world () in
  let adapter = Headless_adapter.create (build_script instants) in
  Tempo_runtime.run ~io:adapter.io (fun input output ->
      let bus = Bus.create ~input ~output in
      let stop = Tempo.new_signal () in
      let quit_watcher () =
        let rec loop () =
          let inp = Tempo.await bus.input in
          if inp.quit then Tempo.emit stop () else loop ()
        in
        loop ()
      in
      Tempo.parallel
        [ (fun () -> Tempo.watch stop (fun () -> Game.process bus world)); quit_watcher ]);
  let frames = Headless_adapter.frames adapter in
  (world, frames)

let () =
  let instants = 900 in
  let world, frames = run_simulation instants in
  let produced = List.length frames in
  let last_message =
    match List.rev frames with [] -> "<aucune frame>" | f :: _ -> f.message
  in
  Printf.printf "Headless simulation finished\n";
  Printf.printf "Instants target: %d\n" instants;
  Printf.printf "Frames produced: %d\n" produced;
  Printf.printf "Final score: %d\n" world.score;
  Printf.printf "Final flags: %d\n" world.flagged;
  Printf.printf "Last message: %s\n" last_message;
  if produced < (instants / 2) then failwith "Headless run produced too few frames"
