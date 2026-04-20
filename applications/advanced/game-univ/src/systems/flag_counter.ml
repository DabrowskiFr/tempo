open Types

let process (bus : Bus.t) (world : world) =
  let rec loop () =
    let events = Tempo.await bus.evt in
    let flagged_now =
      List.fold_left
        (fun acc -> function Student_flagged _ -> acc + 1 | _ -> acc)
        0
        events
    in
    if flagged_now > 0 then world.flagged <- world.flagged + flagged_now;
    loop ()
  in
  let rec restart_loop () =
    let () = Tempo.await bus.restart in
    world.flagged <- 0;
    restart_loop ()
  in
  Tempo.parallel [ loop; restart_loop ]
