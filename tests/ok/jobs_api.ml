open Tempo

let () =
  let pool = Tempo_jobs.create_pool ~domains:1 () in
  Fun.protect
    ~finally:(fun () -> Tempo_jobs.shutdown_pool pool)
    (fun () ->
      execute (fun _ _ ->
          let handle =
            Tempo_jobs.start ~pool
              ~work:(fun ~report_progress ~is_cancelled:_ ->
                report_progress 1;
                report_progress 2;
                "ok")
              ()
          in
          let rec loop saw_terminal =
            ignore (Tempo_jobs.pump handle);
            let saw_terminal =
              match Low_level.peek (Tempo_jobs.updates handle) with
              | None -> saw_terminal
              | Some batch ->
                  let batch = List.rev batch in
                  List.fold_left
                    (fun acc -> function
                      | Tempo_jobs.Progress n ->
                          Printf.printf "progress=%d\n%!" n;
                          acc
                      | Tempo_jobs.Succeeded result ->
                          Printf.printf "done=%s\n%!" result;
                          true
                      | Tempo_jobs.Failed exn ->
                          Printf.printf "failed=%s\n%!" (Printexc.to_string exn);
                          true
                      | Tempo_jobs.Cancelled ->
                          Printf.printf "cancelled\n%!";
                          true)
                    saw_terminal batch
            in
            if saw_terminal then ()
            else (
              pause ();
              loop false)
          in
          loop false))
