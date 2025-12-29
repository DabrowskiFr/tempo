# Tempo Core Bench Report

Each benchmark is measured with warmup/repeated runs and exported as CSV/SVG.

## Core pause() loop (`pause_loop`)

- max total time: 77.235 ms
- best normalized cost: 1882.617 ns/op
- chart: `pause_loop_total_ms.svg`

## emit + await (event signal) (`emit_await`)

- max total time: 32.347 ms
- best normalized cost: 2021.658 ns/op
- chart: `emit_await_total_ms.svg`

## await_immediate handshake (`await_immediate`)

- max total time: 37.914 ms
- best normalized cost: 9426.318 ns/op
- chart: `await_immediate_total_ms.svg`

## when_ guard cost (`when_guard`)

- max total time: 37.679 ms
- best normalized cost: 4709.900 ns/op
- chart: `when_guard_total_ms.svg`

## watch preemption cost (`watch_preempt`)

- max total time: 7.630 ms
- best normalized cost: 1271.606 ns/op
- chart: `watch_preempt_total_ms.svg`

## parallel fanout branches (`parallel_fanout`)

- max total time: 10.657 ms
- best normalized cost: 1231.079 ns/op
- chart: `parallel_fanout_total_ms.svg`

## combined core operators (`combined_core`)

- max total time: 44.902 ms
- best normalized cost: 1870.923 ns/op
- chart: `combined_core_total_ms.svg`

## Combined chart

- `core_per_op_ns.svg`
