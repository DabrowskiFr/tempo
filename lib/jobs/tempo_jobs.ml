type ('progress, 'result) update =
  | Progress of 'progress
  | Succeeded of 'result
  | Failed of exn
  | Cancelled

type status =
  | Running
  | Finished

type job = Job : (unit -> unit) -> job

type pool = {
    queue_mutex : Mutex.t
  ; queue_cond : Condition.t
  ; queue : job Queue.t
  ; stop : bool Atomic.t
  ; mutable workers : unit Domain.t array
}

type ('progress, 'result) handle = {
    updates_signal :
      (('progress, 'result) update, ('progress, 'result) update list) Tempo.agg_signal
  ; updates_mutex : Mutex.t
  ; updates_queue : ('progress, 'result) update Queue.t
  ; cancelled : bool Atomic.t
  ; finished : bool Atomic.t
  ; mutable wakeup : Tempo.wakeup option
}

let rec worker_loop pool =
  let job =
    Mutex.lock pool.queue_mutex;
    while Queue.is_empty pool.queue && not (Atomic.get pool.stop) do
      Condition.wait pool.queue_cond pool.queue_mutex
    done;
    let next =
      if Atomic.get pool.stop then None else Some (Queue.take pool.queue)
    in
    Mutex.unlock pool.queue_mutex;
    next
  in
  match job with
  | None -> ()
  | Some (Job run) ->
      run ();
      worker_loop pool

let create_pool ?(domains = 1) () =
  let pool =
    {
      queue_mutex = Mutex.create ();
      queue_cond = Condition.create ();
      queue = Queue.create ();
      stop = Atomic.make false;
      workers = [||];
    }
  in
  let workers =
    Array.init domains (fun _ -> Domain.spawn (fun () -> worker_loop pool))
  in
  pool.workers <- workers;
  pool

let shutdown_pool pool =
  Atomic.set pool.stop true;
  Mutex.lock pool.queue_mutex;
  Condition.broadcast pool.queue_cond;
  Mutex.unlock pool.queue_mutex;
  Array.iter Domain.join pool.workers

let push_update handle update =
  Mutex.lock handle.updates_mutex;
  Queue.add update handle.updates_queue;
  Mutex.unlock handle.updates_mutex;
  match handle.wakeup with
  | None -> ()
  | Some wakeup -> Tempo.notify_wakeup wakeup

let enqueue pool run =
  Mutex.lock pool.queue_mutex;
  Queue.add (Job run) pool.queue;
  Condition.signal pool.queue_cond;
  Mutex.unlock pool.queue_mutex

let pump handle =
  let drained = ref 0 in
  let local = Queue.create () in
  Mutex.lock handle.updates_mutex;
  while not (Queue.is_empty handle.updates_queue) do
    Queue.add (Queue.take handle.updates_queue) local;
    incr drained
  done;
  Mutex.unlock handle.updates_mutex;
  Queue.iter (fun update -> Tempo.emit_from_host handle.updates_signal update) local;
  !drained

let attach_wakeup wakeup handle =
  handle.wakeup <- Some wakeup;
  Tempo.register_wakeup_poller wakeup (fun () -> pump handle > 0)

let start ~pool ~work () =
  let handle =
    {
      updates_signal = Tempo.new_signal_agg ~initial:[] ~combine:(fun acc item -> item :: acc);
      updates_mutex = Mutex.create ();
      updates_queue = Queue.create ();
      cancelled = Atomic.make false;
      finished = Atomic.make false;
      wakeup = None;
    }
  in
  Option.iter (fun wakeup -> attach_wakeup wakeup handle) (Tempo.current_wakeup ());
  let report_progress value = push_update handle (Progress value) in
  let is_cancelled () = Atomic.get handle.cancelled in
  enqueue pool (fun () ->
      try
        let result = work ~report_progress ~is_cancelled in
        Atomic.set handle.finished true;
        if is_cancelled () then push_update handle Cancelled
        else push_update handle (Succeeded result)
      with exn ->
        Atomic.set handle.finished true;
        if is_cancelled () then push_update handle Cancelled
        else push_update handle (Failed exn));
  handle

let cancel handle = Atomic.set handle.cancelled true

let cancel_on (signal : unit Tempo.signal) handle =
  Tempo.await signal |> ignore;
  cancel handle

let start_with_cancel ~pool ~cancel_on:(cancel_signal : unit Tempo.signal) ~work () =
  let handle = start ~pool ~work () in
  let _ = Tempo.Low_level.fork (fun () -> cancel_on cancel_signal handle) in
  handle

let status handle =
  if Atomic.get handle.finished then Finished else Running

let updates handle = handle.updates_signal
