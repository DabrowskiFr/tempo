# Tempo Core Bench Report

Each benchmark is measured with warmup/repeated runs and exported as CSV/SVG.

## Core pause() loop (`pause_loop`)

- max total time: 155.140 ms
- best normalized cost: 1934.272 ns/op
- chart: `pause_loop_total_ms.svg`

## emit + await (event signal) (`emit_await`)

- max total time: 63.638 ms
- best normalized cost: 1966.895 ns/op
- chart: `emit_await_total_ms.svg`

## await_immediate handshake (`await_immediate`)

- max total time: 80.890 ms
- best normalized cost: 9643.262 ns/op
- chart: `await_immediate_total_ms.svg`

## when_ guard cost (`when_guard`)

- max total time: 78.522 ms
- best normalized cost: 4869.666 ns/op
- chart: `when_guard_total_ms.svg`

## watch preemption cost (`watch_preempt`)

- max total time: 15.703 ms
- best normalized cost: 1297.991 ns/op
- chart: `watch_preempt_total_ms.svg`

## parallel fanout branches (`parallel_fanout`)

- max total time: 77.384 ms
- best normalized cost: 1341.171 ns/op
- chart: `parallel_fanout_total_ms.svg`

## combined core operators (`combined_core`)

- max total time: 93.069 ms
- best normalized cost: 1938.946 ns/op
- chart: `combined_core_total_ms.svg`

## Combined chart

- `core_per_op_ns.svg`
