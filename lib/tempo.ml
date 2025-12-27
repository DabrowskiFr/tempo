(*---------------------------------------------------------------------------
 * Tempo - synchronous runtime for OCaml
 * Copyright (C) 2025 Frédéric Dabrowski
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *---------------------------------------------------------------------------*)

open Effect
open Tempo_types

type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_types.signal_core

type 'a signal = ('a, 'a, event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, aggregate) signal_core
type kill = Tempo_types.kill

let new_signal : unit -> 'a signal =
  fun () -> perform (New_signal ())

let new_signal_agg :
    initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal =
 fun ~initial ~combine -> perform (New_signal_agg (initial, combine))

let emit : type emit agg mode. (emit, agg, mode) signal_core -> emit -> unit =
 fun s v -> perform (Emit (s, v))

let await : type emit agg mode. (emit, agg, mode) signal_core -> agg =
 fun s -> perform (Await s)

let await_immediate : 'a signal -> 'a =
  fun s -> perform (Await_immediate s)

let pause : unit -> unit =
  fun () -> perform Pause

let fork (proc : unit -> unit) : thread =
  perform (Fork proc)

let join (thread_id : thread) : unit =
  perform (Join thread_id)

module Low_level = struct
  type kill = Tempo_types.kill

  let new_kill () = { alive = ref true; cleanup = None }

  let abort_kill (k : kill) =
    if !(k.alive) then k.alive := false;
    match k.cleanup with
    | None -> ()
    | Some cleanup ->
        k.cleanup <- None;
        cleanup ()

  let with_kill (k : kill) (f : unit -> unit) =
    perform (With_kill (k, f))

  let fork (proc : unit -> unit) : thread = perform (Fork proc)

  let join (thread_id : thread) : unit = perform (Join thread_id)
end
let when_ (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) : unit =
  perform (With_guard (s, body))

let watch (s : ('emit, 'agg, 'mode) signal_core) (body : unit -> unit) : unit =
  let kill = Low_level.new_kill () in
  let _ = fork (fun () -> when_ s (fun () -> Low_level.abort_kill kill)) in
  Low_level.with_kill kill body;
  if !(kill.alive) then Low_level.abort_kill kill

let present_then_else
    (s : ('emit, 'agg, 'mode) signal_core)
    (then_ : unit -> unit)
    (else_ : unit -> unit) : unit =
  let seen = ref false in
  let kill = Low_level.new_kill () in
  let _ =
    fork (fun () ->
        Low_level.with_kill kill (fun () ->
            perform
              (With_guard
                 ( s
                 , fun () ->
                     seen := true;
                     then_ () ))))
  in
  pause ();
  if not !seen then else_ ();
  Low_level.abort_kill kill

let parallel procs =
  let threads = List.map fork procs in
  List.iter join threads

let execute = Tempo_engine.execute
