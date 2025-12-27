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

val fresh_event_signal : Tempo_types.scheduler_state -> 'a Tempo_types.signal
val fresh_aggregate_signal :
  Tempo_types.scheduler_state ->
  initial:'agg ->
  combine:('agg -> 'emit -> 'agg) ->
  ('emit, 'agg) Tempo_types.agg_signal

val guard_ok : Tempo_types.any_signal list -> bool
val missing_guards : Tempo_types.any_signal list -> Tempo_types.any_signal list

val update_signal :
  Tempo_types.scheduler_state ->
  ('emit, 'agg, 'mode) Tempo_types.signal_core ->
  'emit -> unit

val emit_event_from_host :
  Tempo_types.scheduler_state ->
  ('a, 'a, Tempo_types.event) Tempo_types.signal_core ->
  'a -> unit

val finalize_signals : Tempo_types.scheduler_state -> unit
