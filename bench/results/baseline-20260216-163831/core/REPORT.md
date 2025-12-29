# Tempo Core Bench Report

Each benchmark is measured with warmup/repeated runs and exported as CSV/SVG.

## Core pause() loop (`pause_loop`)

- max total time: 78.881 ms
- best normalized cost: 1957.822 ns/op
- chart: `pause_loop_total_ms.svg`

## emit + await (event signal) (`emit_await`)

- max total time: 32.390 ms
- best normalized cost: 2006.543 ns/op
- chart: `emit_await_total_ms.svg`

## await_immediate handshake (`await_immediate`)

- max total time: 39.839 ms
- best normalized cost: 9682.812 ns/op
- chart: `await_immediate_total_ms.svg`

## when_ guard cost (`when_guard`)

- max total time: 39.178 ms
- best normalized cost: 4862.646 ns/op
- chart: `when_guard_total_ms.svg`

## watch preemption cost (`watch_preempt`)

- max total time: 8.046 ms
- best normalized cost: 1325.326 ns/op
- chart: `watch_preempt_total_ms.svg`

## parallel fanout branches (`parallel_fanout`)

- max total time: 11.324 ms
- best normalized cost: 1288.910 ns/op
- chart: `parallel_fanout_total_ms.svg`

## combined core operators (`combined_core`)

- max total time: 46.981 ms
- best normalized cost: 1957.560 ns/op
- chart: `combined_core_total_ms.svg`

## Combined chart

- `core_per_op_ns.svg`
