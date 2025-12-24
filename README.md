# Tempo

Tempo is a lightweight synchronous runtime that draws a direct lineage from Esterel, Boussinot’s FairThreads and the more recent ReactiveML. All three share the intuition that deterministic concurrency can be made tractable by slicing time into logical instants where every component reacts to the signals emitted during that instant. 

In Esterel, absence is decided immediately, which posed challenges once processes could appear and disappear dynamically. FairThreads answered this by delaying the observation of absence until the end of the instant, preserving determinism in a dynamic setting. ReactiveML transported these ideas into OCaml, keeping Boussinot’s delayed absence semantics while offering higher-order signals and a functional syntax.

Tempo keeps that “reaction delayed to absence” principle while leveraging OCaml 5 effects to experiment with modern implementations.

- [Instants and execution model](#instants-and-execution-model)
- [Fundamental primitives](#fundamental-primitives)
- [Guards and preemption](#guards-and-preemption)
- [Installation & usage](#installation--usage)

## Instants and execution model

A Tempo program is a collection of reactive components scheduled over successive logical instants. During each instant, every task:

1. observes the presence of signals emitted so far,
2. performs computations (including new `emit` calls),
3. yields control via primitives such as `pause`, `await`, or `when_`.

Only when the instant completes does the runtime confirm the *absence* of each signal and re-queue tasks that were waiting on it. This delayed reaction to absence is what preserves determinism even when tasks are created dynamically.

## Fundamental primitives

Tempo manipulates *signals* that carry values during an instant. Two kinds of signals are supported:

- Event signals, (created with `new_signal ()`) accept at most one emission per instant. They work with both `await` and `await_immediate`.
- Aggregate signals, (created via `new_signal_agg ~initial ~combine`) can accumulate multiple emissions within a single instant by folding successive values with the provided `combine` function. They are consumed with `await` and deliver the aggregated value at the start of the next instant.

Once a signal is created, four primitives form the basis of every reactive process:

- `emit signal value` marks a signal as present for the current instant and delivers `value`. All tasks waiting on the signal resume either immediately or at the next instant depending on how they were suspended.
- `await signal` yields the current task until `signal` becomes present. The continuation always resumes at the beginning of the next instant with the value carried by the signal.
- `await_immediate signal` is similar to `await` but, if the signal is already present, the continuation resumes within the same instant. `await_immediate` is restricted to event signals (at most one emission per instant) to preserve determinism.
- `pause ()` suspends the current task and schedules it for the next instant.

Reactive components can be composed sequentially or in parallel using `parallel : (unit -> unit) list -> unit`, which runs every component from the list.


```ocaml
open Tempo

let counter s () =
  let rec loop n =
    emit s n; pause (); loop (n + 1)
  in loop 0

let observer s () =
  let instant_value = await_immediate s in
  Format.printf "same instant: %d@." instant_value

let scenario () =
  let signal = new_signal () in
    parallel [counter signal; observer signal]

let () = execute scenario
```

`counter` emits an increasing integer every instant, while `observer` uses
`await_immediate s` to react within the same instant.

## Guards and preemption

- `when_ guard body` evaluates `body` only when `guard` is present in the current instant. Otherwise the task is blocked and resumes automatically when the guard becomes present.
- `watch signal body` executes `body` until `signal` is emitted. The emission terminates `body` at the end of the instant (weak preemption).

```ocaml
let guarded_action guard () =
  when_ guard (fun () ->
      Format.printf "[when_] instant %d: guard present@." 0)

let timed_watch trigger () =
  watch trigger (fun () ->
      Format.printf "[watch] start@.";
      pause ();
      Format.printf "[watch] still running@.")

let guard_and_watch guard trigger () =
  watch trigger (fun () ->
      when_ guard (fun () ->
          Format.printf "[combo] guard present while watch active@."))
```

In the first example, `guarded_action` only prints when the guard signal is present. `timed_watch` terminates its body as soon as `trigger` fires. The last snippet combines both: `when_` restricts the body while `watch` ensures the entire block stops one instant after `trigger` is emitted.

Tempo also exposes a low-level module (`Tempo.Low_level`) for advanced use cases, providing direct access to kills, manual fork/join, and guard management. Prefer the high-level primitives unless you need to implement custom control structures.

## Installation & usage

Dependencies: OCaml 5.3 or later and dune.

Build the library:

```sh
dune build
```

Run an example:

```sh
dune exec examples/basic/emit_once_await_one.exe
```

Execute the test suite:

```sh
dune runtest tests/ok
```

To use Tempo in your own project, add `tempo.runtime` to your dune file and `open Tempo`. 
