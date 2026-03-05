open Tempo_base

type ('emit, 'agg, 'mode) signal_core = ('emit, 'agg, 'mode) Tempo_types.signal_core
type 'a signal = ('a, 'a, Tempo_types.event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, Tempo_types.aggregate) signal_core

let new_signal = new_signal
let new_signal_agg = new_signal_agg
let emit = emit
let await = await
let await_immediate = await_immediate
let pause = pause

let when_ (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) : unit =
  Tempo_low_level.with_guard s body

let watch (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) : unit =
  let kill = Tempo_low_level.new_kill () in
  let _ = Tempo_low_level.fork (fun () -> when_ s (fun () -> Tempo_low_level.abort_kill kill)) in
  Tempo_low_level.with_kill kill body;
  if !(kill.Tempo_types.alive) then Tempo_low_level.abort_kill kill

let parallel procs =
  let threads = List.map Tempo_low_level.fork procs in
  List.iter Tempo_low_level.join threads

let rec loop p () =
  p ();
  pause ();
  loop p ()

let rec idle () =
  pause ();
  idle ()
