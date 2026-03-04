(** Structured external jobs executed in parallel domains.

    Jobs run outside the synchronous runtime and only interact with Tempo at
    inter-instant boundaries through {!val:updates}. In interactive mode, jobs
    can wake the runtime so that pending updates are imported without busy
    waiting.

    Guarantees:
    - updates are imported into Tempo only at inter-instant boundaries;
    - jobs can wake an interactive runtime without forcing busy waiting;
    - cancellation is cooperative, never a forced kill of the worker.

    Non-guarantees:
    - the real-time arrival of updates is not deterministic in the same sense
      as pure Tempo code;
    - ordering between different jobs depends on the external scheduler. *)

type ('progress, 'result) update =
  | Progress of 'progress
  | Succeeded of 'result
  | Failed of exn
  | Cancelled

type status =
  | Running
  | Finished

type pool
type ('progress, 'result) handle

(** [create_pool ?domains ()] creates a worker pool backed by OCaml domains. *)
val create_pool : ?domains:int -> unit -> pool

(** [shutdown_pool pool] stops the worker pool once the queued jobs have
    completed or been discarded. *)
val shutdown_pool : pool -> unit

val start :
     pool:pool
  -> work:
       (report_progress:('progress -> unit) ->
        is_cancelled:(unit -> bool) ->
        'result)
  -> unit
  -> ('progress, 'result) handle
(** [start ~pool ~work ()] launches a job on the worker pool.

    The job can report intermediate values with [report_progress]. It should
    observe [is_cancelled ()] regularly and terminate itself when cancellation
    is requested. *)

val start_with_cancel :
     pool:pool
  -> cancel_on:unit Tempo.signal
  -> work:
       (report_progress:('progress -> unit) ->
        is_cancelled:(unit -> bool) ->
        'result)
  -> unit
  -> ('progress, 'result) handle
(** [start_with_cancel ~cancel_on ...] is a convenience wrapper that starts a
    job and links it to a Tempo signal used as a cooperative cancellation
    trigger. *)

(** [cancel handle] requests cooperative cancellation of the job. *)
val cancel : ('progress, 'result) handle -> unit

(** [cancel_on signal handle] waits for [signal] and then requests
    cancellation. *)
val cancel_on :
  unit Tempo.signal ->
  ('progress, 'result) handle ->
  unit

(** [status handle] reports whether the job is still running. *)
val status : ('progress, 'result) handle -> status

val updates :
  ('progress, 'result) handle ->
  (('progress, 'result) update, ('progress, 'result) update list) Tempo.agg_signal
(** [updates handle] is the aggregate signal receiving the imported updates of
    the job. *)

(** [pump handle] imports pending external updates into {!val:updates}.

    This is mainly useful in batch/scripted execution. In interactive mode,
    attaching a wakeup usually makes explicit pumping unnecessary. *)
val pump : ('progress, 'result) handle -> int

(** [attach_wakeup wakeup handle] connects a job to an interactive Tempo
    wakeup so the runtime can be resumed when the job publishes a new update. *)
val attach_wakeup : Tempo.wakeup -> ('progress, 'result) handle -> unit
