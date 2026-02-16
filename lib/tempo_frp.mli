open Tempo

module Frp : sig
  val map : ('a -> 'b) -> 'a signal -> 'b signal
  val filter : ('a -> bool) -> 'a signal -> 'a signal
  val fold : initial:'s -> ('s -> 'a -> 's) -> 'a signal -> 's state
  val hold : initial:'a -> 'a signal -> 'a state
  val sample : 'a state -> 'a
  val once : ('emit, 'agg, 'mode) signal_core -> 'agg signal
  val edge : bool signal -> unit signal
  val throttle_n : int -> ('emit, 'agg, 'mode) signal_core -> 'agg signal
  val debounce_n : int -> ('emit, 'agg, 'mode) signal_core -> 'agg signal
  val switch_once : 'a signal -> ('a -> unit -> unit) -> unit
  val switch_latest : 'a signal -> ('a -> unit -> unit) -> unit
end

module SF : sig
  module Event : sig
    type 'a t =
      | NoEvent
      | Event of 'a

    val map : ('a -> 'b) -> 'a t -> 'b t
    val is_event : 'a t -> bool
  end

  type ('a, 'b) t

  val run : ('a, 'b) t -> 'a signal -> 'b signal
  val arr : ('a -> 'b) -> ('a, 'b) t
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val fanout : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val hold : initial:'b -> ('a, 'b Event.t) t -> ('a, 'b) t
  val edge : ('a, bool) t -> ('a, unit Event.t) t
  val switch : ('a, 'b * 'c Event.t) t -> ('c -> ('a, 'b) t) -> ('a, 'b) t
  val integral : ?initial:float -> dt:float -> unit -> (float, float) t
end
