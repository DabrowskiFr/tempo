open Types

let initial_message = "Appuyez sur Entree ou Espace pour demarrer"

let process (bus : Bus.t) (world : world) =
  let rec status_loop () =
    let messages = Tempo.await bus.status in
    (match List.rev messages with
    | [] -> ()
    | msg :: _ -> world.message <- msg);
    status_loop ()
  in
  let rec restart_loop () =
    let () = Tempo.await bus.restart in
    world.message <- initial_message;
    restart_loop ()
  in
  Tempo.parallel [ status_loop; restart_loop ]
