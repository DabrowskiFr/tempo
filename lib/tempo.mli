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

type ('emit, 'agg, 'mode) signal_core =
  ('emit, 'agg, 'mode) Tempo_types.signal_core

type 'a signal = ('a, 'a, Tempo_types.event) signal_core
type ('emit, 'agg) agg_signal = ('emit, 'agg, Tempo_types.aggregate) signal_core
type kill = Tempo_types.kill

val new_signal : unit -> 'a signal

val new_signal_agg :
  initial:'agg -> combine:('agg -> 'emit -> 'agg) -> ('emit, 'agg) agg_signal

val emit : ('emit, 'agg, 'mode) signal_core -> 'emit -> unit
val await : ('emit, 'agg, 'mode) signal_core -> 'agg
val await_immediate : 'a signal -> 'a
val pause : unit -> unit
(* val fork : (unit -> unit) -> Tempo_types.thread
val join : Tempo_types.thread -> unit *)
val when_ : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val watch : ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> unit
val parallel : (unit -> unit) list -> unit

val present_then_else :
  ('emit, 'agg, 'mode) signal_core -> (unit -> unit) -> (unit -> unit) -> unit

val loop : (unit -> unit) -> unit -> 'a

val idle : unit -> 'a

val control : (unit, unit, Tempo_types.event) signal_core -> (unit -> unit) -> 'a

val alternate : unit signal -> (unit -> unit) -> (unit -> unit) -> 'a

val execute :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ?output:('output -> unit)
  -> ('input signal -> 'output signal -> unit)
  -> unit

module Low_level : sig
  type kill = Tempo_types.kill

  val new_kill : unit -> kill
  val abort_kill : kill -> unit
  val with_kill : kill -> (unit -> unit) -> unit
  val fork : (unit -> unit) -> Tempo_types.thread
  val join : Tempo_types.thread -> unit
end
