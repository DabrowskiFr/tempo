module Scene = Tempo.Scene
module Resource = Tempo.Resource
module Input_map = Tempo.Input_map
module Event_bus = Tempo.Event_bus
module Fixed_step = Tempo.Fixed_step
module Rng = Tempo.Rng
module Netcode = Tempo.Netcode
module Profiler = Tempo.Profiler
module Tick_tags = Tempo.Tick_tags
module Runtime_snapshot = Tempo.Runtime_snapshot
module Entity_set = Tempo.Entity_set
module Dev_hud = Tempo.Dev_hud
module Error_bus = Tempo.Error_bus
module Timeline_json = Tempo.Timeline_json
module App = Tempo.App
module Loop = Tempo.Loop

module Sync : sig
  type 'evt bus = 'evt Event_bus.channel

  (** [bus ()] creates a synchronous multi-producer event bus for game intents. *)
  val bus : unit -> 'evt bus

  (** Publish one domain event into the current logical instant. *)
  val publish : 'evt bus -> 'evt -> unit

  (** Await all events published for the current instant (resumes next instant). *)
  val await_batch : 'evt bus -> 'evt list

  (** [pump_input decode input_signal bus] reads one input value when present,
      decodes it into domain events, publishes all events, then advances to the
      next instant. *)
  val pump_input :
    ('raw -> 'evt list) ->
    'raw Tempo.signal ->
    'evt bus ->
    unit

  (** [reducer_loop bus ~init ~step ~on_state] is the canonical synchronous game
      state owner. At each instant it collects batched events from [bus], applies
      [step], emits/records via [on_state], then loops. *)
  val reducer_loop :
    'evt bus ->
    init:'state ->
    step:('state -> 'evt list -> 'state) ->
    on_state:('state -> unit) ->
    unit

  val every_n : int -> (unit -> unit) -> unit
  val after_n : int -> (unit -> unit) -> unit
  val timeout : int -> on_timeout:(unit -> unit) -> (unit -> unit) -> unit
  val cooldown :
    int -> ('emit, 'agg, 'mode) Tempo.signal_core -> ('agg -> unit) -> unit
end

module Ui : sig
  type rect = { x : float; y : float; w : float; h : float }
  type pointer = { x : float; y : float }
  type interaction = {
    pointer : pointer;
    down : bool;
    pressed : bool;
  }

  type button = {
    id : string;
    rect : rect;
    label : string;
    enabled : bool;
  }

  val button : id:string -> rect -> label:string -> ?enabled:bool -> unit -> button
  val contains : rect -> pointer -> bool
  val button_pressed : interaction -> button -> bool

  type int_stepper = {
    id : string;
    dec_rect : rect;
    inc_rect : rect;
    value : int;
    min_value : int;
    max_value : int;
    step : int;
  }

  val stepper_int :
    id:string ->
    rect ->
    value:int ->
    min_value:int ->
    max_value:int ->
    step:int ->
    int_stepper
  val stepper_delta : interaction -> int_stepper -> int

  type slider = {
    id : string;
    rect : rect;
    t : float;
  }

  val slider_v : id:string -> rect:rect -> t:float -> slider
  val slider_set : interaction -> slider -> float option
end

module Hud : sig
  type panel = {
    rect : Ui.rect;
    title : string;
  }

  type badge = {
    label : string;
    value : string;
  }

  type bar = {
    label : string;
    value : float;
    max_value : float;
  }

  type timer = {
    seconds_left : int;
  }

  val panel : rect:Ui.rect -> title:string -> panel
  val badge : label:string -> value:string -> badge
  val bar : label:string -> value:float -> max_value:float -> bar
  val timer : seconds_left:int -> timer
  val timer_text : timer -> string
end

module Fx : sig
  type kind =
    | Success
    | Warn
    | Error_kind
    | Info

  type toast = {
    text : string;
    kind : kind;
    ttl : int;
  }

  type floating_text = {
    text : string;
    x : float;
    y : float;
    kind : kind;
    ttl : int;
  }

  type screen_pulse = {
    kind : kind;
    ttl : int;
  }

  type item =
    | Toast of toast
    | Floating_text of floating_text
    | Screen_pulse of screen_pulse

  type t

  val empty : t
  val add : t -> item -> t
  val toast : ttl:int -> text:string -> kind:kind -> item
  val floating_text : ttl:int -> text:string -> x:float -> y:float -> kind:kind -> item
  val screen_pulse : ttl:int -> kind:kind -> item
  val update : t -> t
  val active : t -> item list
end

module Audio : sig
  type cue = {
    freq_hz : float;
    duration_s : float;
    volume : float;
  }

  type music_mode =
    | Silent
    | Patrol
    | Tension
    | Results

  type policy_state = {
    started : bool;
    paused : bool;
    game_over : bool;
    tension_count : int;
  }

  val cue : freq_hz:float -> duration_s:float -> volume:float -> cue
  val play_on_event : ('event -> cue option) -> 'event -> cue option
  val music_policy : policy_state -> music_mode
end

module Scene_view : sig
  type scene =
    | Menu
    | Game
    | Pause
    | Results
    | Custom of string

  type overlay =
    | Dim
    | Modal of {
        title : string;
        body : string;
      }

  type cmd =
    | Set_scene of scene
    | Push_overlay of overlay
    | Pop_overlay

  type t = {
    scene : scene;
    overlays : overlay list;
  }

  val initial : scene -> t
  val transition : scene -> scene -> cmd list
  val apply : t -> cmd -> t
  val apply_all : t -> cmd list -> t
end

module Integration : sig
  (** Three-phase synchronous integration contract:
      - phase 1: read external API state and decode domain events,
      - phase 2: run synchronous domain reduction in Tempo,
      - phase 3: project state to output commands and write them externally. *)

  module type BACKEND = sig
    type raw_input
    type output_cmd

    val read : unit -> raw_input
    val write : output_cmd list -> unit
  end

  type ('raw, 'evt) input_adapter = 'raw -> 'evt list
  type ('state, 'cmd) projector = 'state -> 'cmd list

  (** Phase 1 runner: poll backend input, decode semantic events, publish on bus. *)
  val run_read_phase :
    read:(unit -> 'raw) ->
    decode:('raw -> 'evt list) ->
    'evt Sync.bus ->
    unit

  (** Phase 2 runner: synchronous deterministic state owner. *)
  val run_sync_phase :
    'evt Sync.bus ->
    init:'state ->
    step:('state -> 'evt list -> 'state) ->
    on_state:('state -> unit) ->
    unit

  (** Build a phase-3 callback that projects state into output commands and writes
      them through the external backend port. *)
  val make_write_phase :
    project:('state -> 'cmd list) ->
    write:('cmd list -> unit) ->
    'state ->
    unit
end

module Store : sig
  type ('model, 'msg, 'view) app = {
    init : 'model;
    update : 'model -> 'msg -> 'model;
    view : 'model -> 'view;
    equal_view : 'view -> 'view -> bool;
  }

  val run :
    ?instants:int ->
    ?input:(unit -> 'msg option) ->
    ?output:('view -> unit) ->
    ('model, 'msg, 'view) app ->
    unit
end
