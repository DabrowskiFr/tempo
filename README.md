# Tempo

> A lightweight synchronous runtime inspired by Esterel, Boussinot’s FairThreads, and ReactiveML.

All three are grounded in the idea that deterministic concurrency can be made tractable by discretizing time into logical instants, during which all components react to the signals emitted in that instant. In Esterel, the absence of a signal is decided immediately, which enforces a static scheduling discipline and consequently rules out many forms of dynamic behavior. FairThreads addressed this limitation by postponing the observation of absence until the end of the instant, thereby enabling more flexible execution patterns. ReactiveML carried these ideas into OCaml, retaining Boussinot’s delayed-absence semantics while enriching the model with higher-order programming constructs. Tempo keeps that “reaction delayed to absence” principle while leveraging OCaml 5 effects to experiment with modern implementations.

## Table of contents

- [Instants and execution model](#instants-and-execution-model)
- [Fundamental primitives](#fundamental-primitives)
- [Guards and preemption](#guards-and-preemption)
- [Installation & usage](#installation--usage)
- [Examples](#examples)

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

> **Warning**  
> Tempo also exposes a low-level module, Tempo.Low_level, intended for advanced use cases. This module provides direct access to primitive operations such as task termination (kill), explicit fork/join, and manual guard management. Unless you are implementing custom control structures, the high-level primitives should be preferred.

---

## Installation & usage

Dependencies: OCaml 5.3 or later and dune.

### Build the library

```sh
dune build
```

### Execute the test suite

```sh
dune test
```

### Run an example

```sh
dune exec tests/ok/emit_once_await_one.exe -- --log-level info
```

Passing --log-level enables logging of logical instants, making it possible to trace the progression of instants during execution.

### Generate the documentation 

```sh 
dune ocaml doc
```

To use Tempo in your own project, add `tempo` to your dune file and `open Tempo` to your source file. 

---

## Examples 

See examples in `tests/ok`