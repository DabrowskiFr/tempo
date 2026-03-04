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
type wakeup

type 'input interactive_source = {
  poll : unit -> 'input option;
  wait : unit -> unit;
}

val current_wakeup : unit -> wakeup option
val notify_wakeup : wakeup -> unit
val register_wakeup_poller : wakeup -> (unit -> bool) -> unit

val emit_from_host :
  ('emit, 'agg, 'mode) Tempo_types.signal_core -> 'emit -> unit

val execute :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ?output:('output -> unit)
  -> ('input Tempo_types.signal -> 'output Tempo_types.signal -> unit)
  -> unit

val run_interactive :
     ?output:('output -> unit)
  -> input:'input interactive_source
  -> ('input Tempo_types.signal -> 'output Tempo_types.signal -> unit)
  -> unit
