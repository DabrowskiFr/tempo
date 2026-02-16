(** Tempo_async provides a bounded mailbox used as an async-to-sync bridge.
    External producers push values whenever they arrive; a Tempo process drains
    and injects them at tick boundaries. *)

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

type 'a t

(** [create ~capacity ~overflow ()] creates a bounded mailbox.
    @raise Invalid_argument if [capacity <= 0]. *)
val create : capacity:int -> overflow:overflow -> unit -> 'a t

(** [push_external mb v] pushes [v] from an external producer.
    The return value reflects whether the push was accepted or dropped by the
    overflow policy. *)
val push_external : 'a t -> 'a -> push_result

(** [drain_tick mb] returns all queued values in FIFO order and empties [mb]. *)
val drain_tick : 'a t -> 'a list

val clear : 'a t -> unit
val size : 'a t -> int
val stats : 'a t -> stats

(** [pump_to_bus mb bus] drains [mb], publishes all values to [bus], and
    returns the number of published events. *)
val pump_to_bus : 'a t -> 'a Tempo.Event_bus.channel -> int
