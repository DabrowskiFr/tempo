# Tempo Core Bench Report

Each benchmark is measured with warmup/repeated runs and exported as CSV/SVG.

## Core pause() loop (`pause_loop`)

- max total time: 78.209 ms
- best normalized cost: 1931.362 ns/op
- chart: `pause_loop_total_ms.svg`

## emit + await (event signal) (`emit_await`)

- max total time: 31.813 ms
- best normalized cost: 1981.055 ns/op
- chart: `emit_await_total_ms.svg`

## await_immediate handshake (`await_immediate`)

- max total time: 38.577 ms
- best normalized cost: 9644.153 ns/op
- chart: `await_immediate_total_ms.svg`

## when_ guard cost (`when_guard`)

- max total time: 38.536 ms
- best normalized cost: 4788.281 ns/op
- chart: `when_guard_total_ms.svg`

## watch preemption cost (`watch_preempt`)

- max total time: 7.909 ms
- best normalized cost: 1287.386 ns/op
- chart: `watch_preempt_total_ms.svg`

## parallel fanout branches (`parallel_fanout`)

- max total time: 11.398 ms
- best normalized cost: 1294.022 ns/op
- chart: `parallel_fanout_total_ms.svg`

## combined core operators (`combined_core`)

- max total time: 45.881 ms
- best normalized cost: 1911.715 ns/op
- chart: `combined_core_total_ms.svg`

## Combined chart

- `core_per_op_ns.svg`
