open Types

let fx_ttl = 32

type feedback = vec2 * bool * string

let pick_feedback (events : event list) : feedback option =
  let rec aux = function
    | [] -> None
    | Ask_feedback (pos, success, label) :: _ -> Some (pos, success, label)
    | _ :: rest -> aux rest
  in
  aux events

let rec emit_feedback (bus : Bus.t) ((pos, success, label) : feedback) ttl =
  if ttl <= 0 then ()
  else (
    Tempo.emit bus.draw [ Draw_action_fx (pos, success, label, ttl) ];
    Tempo.pause ();
    emit_feedback bus (pos, success, label) (ttl - 1))

let process (bus : Bus.t) =
  let trigger = Tempo.new_signal () in
  let rec ingest_events () =
    let events = Tempo.await bus.evt in
    (match pick_feedback events with
    | Some fx -> Tempo.emit trigger fx
    | None -> ());
    ingest_events ()
  in
  let rec run_feedback () =
    let fx = Tempo.await trigger in
    Tempo.watch trigger (fun () -> emit_feedback bus fx fx_ttl);
    run_feedback ()
  in
  Tempo.parallel [ ingest_events; run_feedback ]
