# Tempo Core Bench Report

Each benchmark is measured with warmup/repeated runs and exported as CSV/SVG.

## Core pause() loop (`pause_loop`)

- max total time: 39.049 ms
- best normalized cost: 1952.456 ns/op
- chart: `pause_loop_total_ms.svg`

## emit + await (event signal) (`emit_await`)

- max total time: 16.073 ms
- best normalized cost: 1982.910 ns/op
- chart: `emit_await_total_ms.svg`

## await_immediate handshake (`await_immediate`)

- max total time: 19.326 ms
- best normalized cost: 9617.578 ns/op
- chart: `await_immediate_total_ms.svg`

## when_ guard cost (`when_guard`)

- max total time: 19.355 ms
- best normalized cost: 4798.926 ns/op
- chart: `when_guard_total_ms.svg`

## watch preemption cost (`watch_preempt`)

- max total time: 4.002 ms
- best normalized cost: 1292.806 ns/op
- chart: `watch_preempt_total_ms.svg`

## parallel fanout branches (`parallel_fanout`)

- max total time: 4.326 ms
- best normalized cost: 1286.621 ns/op
- chart: `parallel_fanout_total_ms.svg`

## combined core operators (`combined_core`)

- max total time: 23.066 ms
- best normalized cost: 1922.180 ns/op
- chart: `combined_core_total_ms.svg`

## Combined chart

- `core_per_op_ns.svg`
