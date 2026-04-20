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

type ('emit, 'agg, 'mode) signal_core = ('emit, 'agg, 'mode) Tempo_core.signal_core
type 'a signal = 'a Tempo_core.signal
type ('emit, 'agg) agg_signal = ('emit, 'agg) Tempo_core.agg_signal

let new_signal = Tempo_core.new_signal
let new_signal_agg = Tempo_core.new_signal_agg
let emit = Tempo_core.emit
let await = Tempo_core.await
let await_immediate = Tempo_core.await_immediate
let pause = Tempo_core.pause
let when_ = Tempo_core.when_
let watch = Tempo_core.watch
let parallel = Tempo_core.parallel

module Constructs = Tempo_constructs

let execute = Tempo_engine.execute
