(** Structured external jobs executed in parallel domains.

    Jobs run outside the synchronous runtime and only interact with Tempo at
    inter-instant boundaries through {!val:updates}. In interactive mode, jobs
    can wake the runtime so that pending updates are imported without busy
    waiting. *)

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

val create_pool : ?domains:int -> unit -> pool
val shutdown_pool : pool -> unit

val start :
     pool:pool
  -> work:
       (report_progress:('progress -> unit) ->
        is_cancelled:(unit -> bool) ->
        'result)
  -> unit
  -> ('progress, 'result) handle

val start_with_cancel :
     pool:pool
  -> cancel_on:unit Tempo.signal
  -> work:
       (report_progress:('progress -> unit) ->
        is_cancelled:(unit -> bool) ->
        'result)
  -> unit
  -> ('progress, 'result) handle

val cancel : ('progress, 'result) handle -> unit
val cancel_on :
  unit Tempo.signal ->
  ('progress, 'result) handle ->
  unit

val status : ('progress, 'result) handle -> status

val updates :
  ('progress, 'result) handle ->
  (('progress, 'result) update, ('progress, 'result) update list) Tempo.agg_signal

val pump : ('progress, 'result) handle -> int

val attach_wakeup : Tempo.wakeup -> ('progress, 'result) handle -> unit
