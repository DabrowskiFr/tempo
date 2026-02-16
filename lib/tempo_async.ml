type overflow =
  [ `Drop_oldest
  | `Drop_newest
  | `Reject
  ]

type push_result =
  [ `Ok
  | `Dropped
  ]

type stats = {
  size : int;
  capacity : int;
  pushed : int;
  dropped_oldest : int;
  dropped_newest : int;
  rejected : int;
}

type 'a t = {
  q : 'a Queue.t;
  capacity : int;
  overflow : overflow;
  mutable pushed : int;
  mutable dropped_oldest : int;
  mutable dropped_newest : int;
  mutable rejected : int;
}

let create ~capacity ~overflow () =
  if capacity <= 0 then invalid_arg "Tempo_async.create: capacity must be > 0";
  {
    q = Queue.create ();
    capacity;
    overflow;
    pushed = 0;
    dropped_oldest = 0;
    dropped_newest = 0;
    rejected = 0;
  }

let size t = Queue.length t.q

let push_external t v =
  if Queue.length t.q < t.capacity then (
    Queue.add v t.q;
    t.pushed <- t.pushed + 1;
    `Ok)
  else
    match t.overflow with
    | `Drop_oldest ->
        ignore (Queue.take t.q);
        t.dropped_oldest <- t.dropped_oldest + 1;
        Queue.add v t.q;
        t.pushed <- t.pushed + 1;
        `Dropped
    | `Drop_newest ->
        t.dropped_newest <- t.dropped_newest + 1;
        `Dropped
    | `Reject ->
        t.rejected <- t.rejected + 1;
        `Dropped

let drain_tick t =
  let rec loop acc =
    if Queue.is_empty t.q then List.rev acc
    else loop (Queue.take t.q :: acc)
  in
  loop []

let clear t = Queue.clear t.q

let stats t =
  {
    size = Queue.length t.q;
    capacity = t.capacity;
    pushed = t.pushed;
    dropped_oldest = t.dropped_oldest;
    dropped_newest = t.dropped_newest;
    rejected = t.rejected;
  }

let pump_to_bus t bus =
  let events = drain_tick t in
  List.iter (Tempo.Event_bus.publish bus) events;
  List.length events
