# Tempo

## 1. Overview
Tempo is an OCaml synchronous reactive programming library built on algebraic effects (OCaml 5). It runs concurrent processes over a sequence of **logical instants**.

Core idea:
- execution advances instant by instant;
- signals emitted during an instant are observed depending on primitives (`await`, `await_immediate`, `when_`, `watch`);
- signal absence is decided at the end of the instant, which supports deterministic behavior.

## 2. Execution model
### 2.1 Instants
An instant is one synchronous logical frame of computation.

### 2.2 Signals
Two signal families:
- `new_signal ()`: event signal (at most one emission per instant)
- `new_signal_agg ~initial ~combine`: aggregate signal (multiple emissions per instant, folded with `combine`)

### 2.3 High-level primitives
- `emit s v`
- `await s`
- `await_immediate s`
- `pause ()`
- `parallel [ ... ]`
- `when_ g body`
- `watch s body`
- `present_then_else s t e`

### 2.4 Execution
- `execute ?instants ?input ?output main`
- `execute_trace ~inputs main`: capture outputs
- `execute_timeline ~inputs main`: per-instant input/output trace
- `execute_inspect ~on_instant ...`: runtime inspection snapshots (tasks/signals)

## 3. Main API modules
### 3.1 Core
- `Tempo`: synchronous primitives, signals, execution
- `Tempo.Low_level`: fork/join/kill (advanced usage)

### 3.2 State/process tools
- `new_state/get_state/set_state/modify_state/await_state`
- `Tempo.State.create/get/set/modify/await/update_and_get`
- `Tempo.Dynamic`: `spawn/stop/join/spawn_many`

### 3.3 Gameplay helpers
- `Tempo.Game.after_n`
- `Tempo.Game.every_n`
- `Tempo.Game.timeout`
- `Tempo.Game.cooldown`

### 3.4 Game-dev modules
- `Tempo.Scene`: scene transitions
- `Tempo.Resource`: resource lifecycle
- `Tempo.Input_map`: action mapping
- `Tempo.Event_bus`: typed event bus
- `Tempo.Fixed_step`: fixed-step loop / interpolation
- `Tempo.Rng`: deterministic RNG
- `Tempo.Netcode`: frame-tagged snapshots + rollback on `state`
- `Tempo.Profiler`: timing measurements
- `Tempo.Tick_tags`: instant tags
- `Tempo.Runtime_snapshot`: runtime state capture/restore
- `Tempo.Error_bus`: safe execution and structured error reporting
- `Tempo.Timeline_json.of_timeline_with`: named-argument JSON timeline export
- `Tempo.Dev_hud`: text rendering of inspection snapshots
- `Tempo.Entity_set`: dynamic entity spawn/despawn/broadcast

## 4. `tempo.game` package
In addition to `tempo`, this repository exposes `tempo.game` (module `Tempo_game`) that re-exports game-oriented modules:
- `Scene`, `Resource`, `Input_map`, `Event_bus`, `Fixed_step`, `Rng`, `Netcode`, `Profiler`, `Tick_tags`, `Runtime_snapshot`, `Entity_set`, `Dev_hud`, `Error_bus`, `Timeline_json`.

## 5. Dependencies
### 5.1 Tempo library dependencies
- OCaml >= 5.3.0
- dune >= 3.19
- logs >= 0.10.0
- mtime >= 2.1.0

### 5.2 Optional dependencies for graphical games (example)
Tempo does not require a rendering engine. For a 2D game like `game-univ`, you can add:
- `raylib` (OCaml bindings)
- optional audio/assets stack depending on your setup

## 6. Installation
### 6.1 From this repository (local pin)
```bash
opam switch create 5.4.0   # or 5.3.x+
eval $(opam env)
opam install dune logs mtime
opam pin add tempo /Users/fredericdabrowski/Repos/tempo
```

### 6.2 From GitHub
```bash
opam pin add tempo https://github.com/DabrowskiFr/tempo.git
```

## 7. Build, tests, benchmarks, docs
### 7.1 Build
```bash
cd /Users/fredericdabrowski/Repos/tempo
dune build
```

### 7.2 Tests
```bash
dune runtest
```

### 7.3 Benchmarks
```bash
dune exec bench/basic_bench.exe
```

### 7.4 Repository docs
- Game quickstart: `docs/GAME_QUICKSTART.md`
- Main README: `README.md`

## 8. Using Tempo in a Dune project
Example `dune`:
```lisp
(executable
 (name main)
 (libraries tempo))
```

If you want game-oriented modules:
```lisp
(executable
 (name main)
 (libraries tempo tempo.game))
```

In your code:
```ocaml
open Tempo
```

## 9. Minimal example
```ocaml
open Tempo

let () =
  execute (fun input output ->
    let rec loop tick =
      when_ input (fun () ->
        let v = await_immediate input in
        emit output (tick + v));
      pause ();
      loop (tick + 1)
    in
    loop 0)
```

## 10. Recommended game architecture
Typical structure:
- one platform adapter (raylib/headless/etc.)
- `execute ~input ~output`
- parallel Tempo processes for:
  - input mapping
  - gameplay/entities
  - UI/HUD
  - audio/events

Best practices:
- keep gameplay logic in Tempo, not in rendering adapters;
- use `Event_bus` to decouple gameplay/UI/audio;
- use `execute_timeline` + `Timeline_json.of_timeline_with` for deterministic debugging;
- use `Dynamic`/`Entity_set` for runtime spawn/despawn.

## 11. Error handling
- Wrap unsafe code with `Error_bus.safe`.
- Use `Error_bus.execute_safe` for top-level safe execution and structured error publication on a dedicated signal.

## 12. Runtime inspection and debugging
- `execute_inspect` provides, per instant:
  - `instant`
  - `current_tasks`
  - `blocked_tasks`
  - `next_tasks`
  - `signal_count`
- `Dev_hud.to_string` converts snapshots into text suitable for in-game debug overlays.

## 13. Version/API
- `Tempo.version_string`
- `Tempo.api_level`
- `Tempo.require_api_level n`

## 14. Current limits (v1)
- Runtime snapshots currently rely on registered `state` cells (not full scheduler serialization).
- `tempo.game` is practical utility surface; some APIs may evolve.

## 15. Repository references
- Public API: `lib/tempo.mli`
- Implementation: `lib/tempo.ml`
- Game re-export module: `lib/tempo_game.mli`, `lib/tempo_game.ml`
- Tests: `tests/ok`
- Tempo-based game example: `applications/games/game-univ/`
