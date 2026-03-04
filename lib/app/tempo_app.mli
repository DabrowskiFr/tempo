(** Application-oriented helpers built on top of {!Tempo}.

    This package is intentionally small. It groups three reusable patterns:

    - {!module:App} for message-driven applications with commands,
    - {!module:Loop} for a simpler ticked state loop,
    - {!module:Scene} for scene switching with enter/exit callbacks. *)

module App : sig
  type 'msg dispatch = 'msg -> unit
  type 'msg command = dispatch:'msg dispatch -> unit

  val none : 'msg command
  val emit : 'msg -> 'msg command
  val after_n : int -> 'msg -> 'msg command
  val every_n : int -> 'msg -> 'msg command
  val tick_every : int -> tick:'msg -> 'msg command
  val tick_if : bool -> int -> tick:'msg -> 'msg command
  val command_if : bool -> 'msg command -> 'msg command
  val command_when : bool -> then_:'msg command -> else_:'msg command -> 'msg command
  val batch : 'msg command list -> 'msg command

  (** [present_then_else s then_branch else_branch] executes [then_branch]
      immediately when [s] is present in the current instant; otherwise
      [else_branch] runs in the next logical instant. *)
  val present_then_else :
    ('emit, 'agg, 'mode) Tempo.signal_core ->
    (unit -> unit) ->
    (unit -> unit) ->
    unit

  val boot_once_input : boot:'msg -> (unit -> 'msg option) -> unit -> 'msg option
  val input_union : (unit -> 'msg option) list -> unit -> 'msg option

  val with_boot_and_tick :
    boot:'msg ->
    tick:'msg ->
    tick_every:int ->
    input:(unit -> 'msg option) ->
    (unit -> 'msg option) * 'msg command

  type ('model, 'msg) program = {
    init : 'model;
    update : 'model -> 'msg -> 'model * 'msg command;
  }

  val run :
    ?instants:int ->
    ?input:(unit -> 'msg option) ->
    ?on_model:('model -> unit) ->
    ('model, 'msg) program ->
    unit

  val run_with_view :
    ?instants:int ->
    ?input:(unit -> 'msg option) ->
    ?equal_view:('view -> 'view -> bool) ->
    view:('model -> 'view) ->
    ?output:('view -> unit) ->
    ('model, 'msg) program ->
    unit
end

module Loop : sig
  type ('input, 'output, 'state) config = {
    init : 'state;
    input : unit -> 'input option;
    step : 'state -> 'input option -> 'state * 'output option;
    output : 'output -> unit;
  }

  val run : ?instants:int -> ('input, 'output, 'state) config -> unit
end

module Scene : sig
  type 'id t

  val create :
    ?equal:('id -> 'id -> bool) ->
    on_enter:('id -> unit) ->
    on_exit:('id -> unit) ->
    unit ->
    'id t

  val request : 'id t -> 'id -> unit
  val current : 'id t -> 'id option
  val process : 'id t -> unit
end
