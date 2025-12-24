open Tempo

let log tag instant message =
  Format.printf "[%s][t=%02d] %s@.%!" tag instant message

let scenario () =
  let sum_signal =
    new_signal_agg ~initial:0 ~combine:(fun acc payload -> acc + payload)
  in
  let producer () =
    log "driver" 0 "emit 1, 2, 3";
    emit sum_signal 1;
    emit sum_signal 2;
    emit sum_signal 3;
    pause ();
    log "driver" 1 "emit 4, 5,6";
    emit sum_signal 4;
    emit sum_signal 5;
    emit sum_signal 6
  in
  let consumer_first () =
    log "consumer-1" 0 "await sum #0";
    let total = await sum_signal in
    log "consumer-1" 0 (Printf.sprintf "sum=%d" total)
  and consumer_second () =
    pause ();
    log "consumer-2" 1 "await sum #1";
    let total = await sum_signal in
    log "consumer-2" 1 (Printf.sprintf "sum=%d" total)
  in
  parallel [consumer_first; consumer_second; producer]

let () = execute (fun _ _ -> scenario ())
