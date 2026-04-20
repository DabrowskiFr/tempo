# Tempo

A lightweight synchronous runtime inspired by Esterel, Boussinot’s FairThreads, and ReactiveML.
All three are grounded in the idea that deterministic concurrency can be made tractable by discretizing time into logical instants, during which all components react to the signals emitted in that instant. In Esterel, the absence of a signal is decided immediately, which enforces a static scheduling discipline and consequently rules out many forms of dynamic behavior. FairThreads addressed this limitation by postponing the observation of absence until the end of the instant, thereby enabling more flexible execution patterns. ReactiveML carried these ideas into OCaml, retaining Boussinot’s delayed-absence semantics while enriching the model with higher-order programming constructs. Tempo keeps that “reaction delayed to absence” principle while leveraging OCaml 5 effects to experiment with modern implementations.

## Table of contents

- [Instants and execution model](#instants-and-execution-model)
- [Fundamental primitives](#fundamental-primitives)
- [Derived functions](#derived-functions)
- [Guards and preemption](#guards-and-preemption)
- [Installation](#installation)
- [Build](#build)
- [Run tests](#run-tests)
- [Run sample applications](#run-sample-applications)
- [Run graphical demos](#run-graphical-demos)
- [Create and run your own Tempo application](#create-and-run-your-own-tempo-application)
- [Logging and runtime flags](#logging-and-runtime-flags)
- [Other useful commands](#other-useful-commands)

---

## Instants and execution model

A Tempo program executes over a sequence of logical instants. It supports synchronous parallel composition of behaviors that communicate through valued signals. At each instant, a signal is either present or absent. A signal is present if and only if a value is provided by the environment or emitted by the program during that instant.

The absence of a signal can be observed only once the instant has completed. Observers may then react in the following instant. This delayed observation of absence is the key mechanism that preserves determinism.

---

## Guards and preemption

Tempo supports guarded execution and weak preemption, allowing behaviors to be conditionally activated and preempted without violating the synchronous execution model.

---

## Fundamental primitives

Tempo manipulates signals that carry values during a logical instant. Two kinds of signals are supported:

- **Event signals**: created with `new_signal ()`, accept at most one emission per instant.
- **Aggregate signals**: created with `new_signal_agg ~initial ~combine`, may accumulate multiple emissions within a single instant by folding successive values using the provided combine function.

Reactive programs are built from seven primitive operations:

- **`emit signal value`** marks signal as present in the current instant and delivers value. All tasks waiting on the signal are resumed, either immediately or at the next instant, depending on how they were suspended.
- **`await signal`** suspends the current task until signal becomes present. The continuation always resumes at the beginning of the next instant, receiving the value carried by the signal.
- **`await_immediate` signal** behaves like await, except that if the signal is already present, the continuation resumes within the same instant. To preserve determinism, await_immediate is restricted to event signals, which allow at most one emission per instant.
- **`pause ()`** suspends the current task and schedules it for execution in the next instant.
- **`parallel [p1; …; pn]`** executes all programs in the list concurrently within the same instant.
- **`when_ guard body`** executes body only if guard is present in the current instant. Otherwise, the task is blocked and automatically resumes when the guard becomes present.
- **`watch signal body`** executes body until signal is emitted. Such an emission causes body to be terminated at the end of the current instant, implementing weak preemption.

--- 
## Derived functions

A few helpers are built on top of the primitives and exported by `Tempo`:

- `present_then_else s then_ else_` — run `then_` if `s` is present in the current instant, otherwise run `else_` in the next instant.
- `loop f` — repeat `f` forever with a `pause` between iterations.
- `idle` — a `pause`-forever process.
- `control toggle proc` — start/stop `proc` each time `toggle` is emitted (starts stopped).
- `alternate toggle proc_a proc_b` — run `proc_a` immediately, then switch between `proc_a` and `proc_b` on each `toggle` emission.

---

## Installation

Requirements:

- OCaml >= 5.3
- dune >= 3
- opam (recommended)

From the repository root:

```sh
opam install . --deps-only --with-test --with-doc
```

If you want to pin this local checkout:

```sh
opam pin add tempo . --no-action
opam install . --deps-only --with-test --with-doc
```

---

## Build

Build everything:

```sh
dune build
```

Build only the public library/installable targets:

```sh
dune build @install
```

---

## Run tests

Run the full test suite:

```sh
dune runtest
```

Equivalent shorthand:

```sh
dune test
```

Run a single test executable:

```sh
dune exec tests/ok/emit_once_await_one.exe
```

---

## Run sample applications

The `samples/` directory contains small CLI-oriented programs:

- `emit_twice`
- `when_emit_parallel`
- `awaiters_log`
- `all_effects_log`

Run one sample:

```sh
dune exec samples/all_effects_log.exe -- --log-level info
```

You can also run tests as small scenario applications from `tests/ok/`:

```sh
dune exec tests/ok/watch_nested.exe -- --log-level info
```

---

## Run graphical demos

The `examples/` directory contains graphical demos (Graphics and TSDL backends).

Available executables include:

- `balls_control_graphics`
- `balls_alternate_graphics`
- `boids_graphics`
- `boids_tsdl`
- `snake_tsdl`
- `solar_system_graphics`
- `solar_system_tsdl`
- `ca_continuous_graphics`
- `ca_continuous_tsdl`

Run one demo:

```sh
dune exec examples/boids_tsdl.exe
```

Note: these demos require additional libraries (`graphics`, `tsdl`, `tsdl-ttf`) and a working graphical environment.

---

## Create and run your own Tempo application

In your dune file:

```lisp
(executable
 (name my_app)
 (libraries tempo))
```

In your OCaml file:

```ocaml
open Tempo

let () =
  let s = new_signal () in
  run (parallel [
    (fun () ->
       let v = await s in
       Format.printf "received %d@." v);
    (fun () -> emit s 42)
  ])
```

Run it:

```sh
dune exec path/to/my_app.exe
```

---

## Logging and runtime flags

Tempo supports runtime logging with CLI flags and environment variables.

CLI:

- `--log-level debug|info|warning|error|quiet`

Environment variables:

- `RML_LOG_LEVEL=debug|info|warning|error|off`
- `RML_LOG_COLOR=0|1` (disable/enable ANSI colors)
- `RML_TRACE_GUARDS=0|1` (extra guard tracing)

Example:

```sh
RML_LOG_LEVEL=debug RML_TRACE_GUARDS=1 dune exec samples/awaiters_log.exe
```