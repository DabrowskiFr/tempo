open Effect
open Tempo_types

type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_types.signal_core

type 'a signal = ('a, 'a, event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core

let new_signal : unit -> 'a signal = fun () -> perform (New_signal ())

let new_signal_agg :
    initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal
    =
 fun ~initial ~combine -> perform (New_signal_agg (initial, combine))

let emit : type emit agg mode. (emit, agg, mode) signal_core -> emit -> unit =
 fun s v -> perform (Emit (s, v))

let await : type emit agg mode. (emit, agg, mode) signal_core -> agg =
 fun s -> perform (Await s)

let await_immediate : 'a signal -> 'a = fun s -> perform (Await_immediate s)
let pause : unit -> unit = fun () -> perform Pause
let fork (proc : unit -> unit) : thread = perform (Fork proc)
let join (thread_id : thread) : unit = perform (Join thread_id)

let when_ (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) : unit =
  perform (With_guard (s, body))

let watch (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) : unit =
  let kill = Tempo_low_level.new_kill () in
  perform (Register_kill_watcher (s, kill));
  Tempo_low_level.with_kill kill body;
  Tempo_low_level.abort_kill kill

let fork_join2 a b =
  let t = fork b in
  a ();
  join t

let parallel procs =
  let rec spawn acc = function
    | [] -> acc
    | p :: ps -> spawn (fork p :: acc) ps
  in
  let rec join_all = function
    | [] -> ()
    | t :: ts ->
        join t;
        join_all ts
  in
  match procs with
  | [] -> ()
  | [ p ] -> p ()
  | [ a; b ] -> fork_join2 a b
  | p :: ps ->
      let threads = spawn [] ps in
      p ();
      join_all threads
