# Tempo Game Quickstart

## Core Patterns

- Timers and reactive helpers: `Tempo.Constructs.after_n`, `Tempo.Constructs.every_n`, `Tempo.Constructs.cooldown`
- Preemption and orchestration: `Tempo.watch`, `Tempo.parallel`, `Tempo.when_`
- Scene transitions: `Tempo_app.Scene`
- Interactive execution: `Tempo.run_interactive`
- External jobs: `Tempo_jobs`

## Deterministic Testing

- `Tempo.Observe.execute_trace` to capture outputs
- `Tempo.Observe.execute_timeline` to inspect per-instant IO
- `Tempo.Timeline_json.of_timeline_with` to export traces as JSON

## Debug & Profiling

- `Tempo.Observe.execute_inspect` for runtime snapshots
- `Tempo.Dev_hud.to_string` for quick debug overlays
- `Tempo.Profiler.measure` for timed sections

## Snapshot / Rollback

- `Tempo.Runtime_snapshot.capture`
- `Tempo.Runtime_snapshot.restore`
- `Tempo.Netcode.snapshot` for frame-tagged game state snapshots
