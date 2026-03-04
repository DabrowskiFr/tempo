# Tempo

## 1. Overview

Tempo is an OCaml synchronous reactive programming library built around logical instants.

Its core model is:
- processes communicate through signals;
- each instant is one synchronous reaction step;
- signal absence is decided only at the end of the instant;
- concurrency stays deterministic as long as execution remains inside that model.

## 2. Main surface

### 2.1 Recommended modules

The recommended public surface is:
- `Tempo.Core` for synchronous primitives;
- `Tempo.Constructs` for higher-level derived operators;
- `Tempo.Observe` for tracing and inspection;
- `Tempo.Meta` for version and API-level metadata.

The same primitives are also available at the top level of `Tempo` for compatibility.

### 2.2 `Tempo.Core`

`Tempo.Core` is the canonical programming surface:
- `new_signal`
- `new_signal_agg`
- `emit`
- `await`
- `await_immediate`
- `pause`
- `loop`
- `when_`
- `watch`
- `parallel`
- `execute`
- `run_interactive`

### 2.3 `Tempo.Constructs`

`Tempo.Constructs` gathers higher-level reactive operators:
- `present_then_else`
- `after_n`
- `every_n`
- `timeout`
- `cooldown`
- `rising_edge`
- `falling_edge`
- `edge_by`
- `pulse_n`
- `supervise_until`

### 2.4 `Tempo.Low_level`

`Tempo.Low_level` is meant for extension authors and advanced runtime work:
- `with_guard`
- `new_kill`
- `with_kill`
- `abort_kill`
- `abort_kill_next`
- `fork`
- `join`
- `peek`
- `is_present`

### 2.5 `Tempo.Observe`

`Tempo.Observe` contains runtime observation helpers:
- `execute_trace`
- `execute_timeline`
- `execute_inspect`

### 2.6 `Tempo.Meta`

`Tempo.Meta` exposes:
- `version_string`
- `api_level`
- `require_api_level`

## 3. Execution

Tempo exposes two execution styles:
- `execute` for batch/scripted runs;
- `run_interactive` for long-lived applications.

The interactive runtime uses a hybrid model:
- `push` wakes the runtime up;
- `poll` imports external data at a controlled inter-instant boundary.

This matters for host inputs and `tempo-jobs`:
- external producers do not mutate the runtime directly;
- they notify wakeups;
- data is imported when the runtime opens the next logical instant.

## 4. Companion packages

### 4.1 `tempo-app`

`tempo-app` provides lightweight application structuring:
- `Tempo_app.App`
- `Tempo_app.Loop`
- `Tempo_app.Scene`

### 4.2 `tempo-jobs`

`tempo-jobs` runs external jobs on OCaml domains and bridges their progress/completion back into Tempo.

It is useful when you need:
- real parallelism;
- a disciplined bridge back into the synchronous runtime;
- cooperative cancellation.

### 4.3 `tempo-raylib`

`tempo-raylib` provides Raylib helpers for demos and graphical applications.

### 4.4 `tempo-fluidsynth`

`tempo-fluidsynth` provides a FluidSynth backend for:
- better sounding musical demos;
- MIDI import;
- SoundFont-based playback.

### 4.5 `tempo-frp`

`tempo-frp` remains available as an FRP/SF encoding on top of Tempo. It is not the main recommended entry point for learning the library.

## 5. Minimal example

```ocaml
open Tempo

let () =
  Core.execute (fun input output ->
    let rec loop () =
      Core.when_ input (fun () ->
        let v = Core.await_immediate input in
        Core.emit output (v + 1));
      Core.pause ();
      loop ()
    in
    loop ())
```

## 6. Demo applications

Important advanced applications in this repository:
- `applications/advanced/game-univ`
- `applications/advanced/music_score_player`
- `applications/advanced/refactor`
- `applications/advanced/tempo-core-studio`

Simple demos:
- `applications/simple-demos/snake-raylib`
- `applications/simple-demos/boids-raylib`

## 7. Build and test

```bash
cd /Users/fredericdabrowski/Repos/tempo/tempo-dev/tempo
dune build
dune runtest
dune build @doc
```

## 8. Useful references

- Public API: `lib/core/tempo.mli`
- Main implementation: `lib/core/tempo.ml`
- Tests: `tests/ok`
- Odoc pages: `doc/`
