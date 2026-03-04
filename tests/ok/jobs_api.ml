open Tempo

let input =
  {
    poll = (fun () -> None);
    wait =
      (fun () ->
        Unix.sleepf 0.005;
        Option.iter notify_wakeup (current_wakeup ()));
  }

let () =
  let pool = Tempo_jobs.create_pool ~domains:1 () in
  Fun.protect
    ~finally:(fun () -> Tempo_jobs.shutdown_pool pool)
    (fun () ->
      run_interactive ~input (fun _ _ ->
          let handle =
            Tempo_jobs.start ~pool
              ~work:(fun ~report_progress ~is_cancelled:_ ->
                report_progress 1;
                Unix.sleepf 0.01;
                report_progress 2;
                "ok")
              ()
          in
          let rec loop () =
            let batch = List.rev (await (Tempo_jobs.updates handle)) in
            List.iter
              (function
                | Tempo_jobs.Progress n ->
                    Printf.printf "progress=%d\n%!" n
                | Tempo_jobs.Succeeded result ->
                    Printf.printf "done=%s\n%!" result
                | Tempo_jobs.Failed exn ->
                    Printf.printf "failed=%s\n%!" (Printexc.to_string exn)
                | Tempo_jobs.Cancelled ->
                    Printf.printf "cancelled\n%!")
              batch;
            match Tempo_jobs.status handle with
            | Tempo_jobs.Running -> loop ()
            | Tempo_jobs.Finished -> ()
          in
          loop ()))
