open Tempo

let log tag message = Format.printf "[%s] %s@.%!" tag message

let string_of_batch batch =
  match batch with [] -> "[]" | items -> "[" ^ String.concat "; " items ^ "]"

let scenario () =
  let collect_signal =
    new_signal_agg ~initial:[] ~combine:(fun acc payload -> payload :: acc)
  in
  let consumer () =
    log "consumer" "waiting for batch #0";
    let batch0 = await collect_signal in
    log "consumer"
      (Printf.sprintf "received batch #0 = %s" (string_of_batch batch0));
    log "consumer" "waiting for batch #1";
    let batch1 = await collect_signal in
    log "consumer"
      (Printf.sprintf "received batch #1 = %s" (string_of_batch batch1))
  and producer () =
    log "producer" "emit \"alpha\", \"beta\", \"gamma\"";
    emit collect_signal "alpha";
    emit collect_signal "beta";
    emit collect_signal "gamma";
    pause ();
    log "producer" "emit \"delta\" puis \"epsilon\"";
    emit collect_signal "delta";
    emit collect_signal "epsilon"
  in
  parallel [ consumer; producer ]

let () = execute (fun _ _ -> scenario ())
