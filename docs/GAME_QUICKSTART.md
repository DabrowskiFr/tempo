# Tempo Game Quickstart

## Core Patterns

- Timers: `Tempo.Game.after_n`, `Tempo.Game.every_n`, `Tempo.Game.cooldown`
- Dynamic entities: `Tempo.Dynamic`, `Tempo.Entity_set`
- State cells: `Tempo.new_state` + `Tempo.await_state`
- Scene transitions: `Tempo.Scene`

## Deterministic Testing

- `Tempo.execute_trace` to capture outputs
- `Tempo.execute_timeline` to inspect per-instant IO
- `Tempo.Timeline_json.of_timeline` to export traces as JSON

## Debug & Profiling

- `Tempo.execute_inspect` for runtime snapshots
- `Tempo.Dev_hud.to_string` for quick debug overlays
- `Tempo.Profiler.measure` for timed sections

## Snapshot / Rollback

- `Tempo.Runtime_snapshot.capture`
- `Tempo.Runtime_snapshot.restore`
- `Tempo.Netcode.snapshot` for frame-tagged game state snapshots
