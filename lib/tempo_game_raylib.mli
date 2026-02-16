(** Raylib backend interpreter for Tempo game abstractions.
    The public intent is to keep game logic synchronous and backend-agnostic:
    domain events and state live in Tempo/Tempo_game; this module interprets
    those abstractions on raylib. *)

module Backend : sig
  module Input : sig
    val interaction_from_mouse : unit -> Tempo_game.Ui.interaction
  end

  module Ui_draw : sig
    val draw_button : ?active:bool -> Tempo_game.Ui.button -> unit
    val draw_stepper_int : Tempo_game.Ui.int_stepper -> unit
    val draw_slider_v : Tempo_game.Ui.slider -> unit
  end

  module Hud_draw : sig
    val draw_panel : Tempo_game.Hud.panel -> unit
    val draw_badge : x:int -> y:int -> Tempo_game.Hud.badge -> unit
    val draw_bar : x:int -> y:int -> w:int -> h:int -> Tempo_game.Hud.bar -> unit
    val draw_timer : x:int -> y:int -> Tempo_game.Hud.timer -> unit
    val draw_message_bar : x:int -> y:int -> w:int -> h:int -> text:string -> unit
  end

  module Fx_draw : sig
    val draw : Tempo_game.Fx.t -> unit
  end

  module Audio_backend : sig
    val init : unit -> unit
    val shutdown : unit -> unit
    val make_tone : Tempo_game.Audio.cue -> Raylib.Sound.t
    val play_cue : Tempo_game.Audio.cue -> unit
    val play_tones : (float * float * float) list -> unit
  end
end

module Sync : sig
  (** [pump_actions poll bus] repeatedly polls domain actions and publishes
      them to a synchronous Tempo_game bus, one logical instant at a time. *)
  val pump_actions :
    (unit -> 'evt list) ->
    'evt Tempo_game.Sync.bus ->
    unit

  (** [pump_mouse_interaction decode bus] samples raylib mouse state,
      decodes it into domain events, and publishes events to [bus]
      under Tempo logical instants. *)
  val pump_mouse_interaction :
    (Tempo_game.Ui.interaction -> 'evt list) ->
    'evt Tempo_game.Sync.bus ->
    unit
end

(** Raylib-specific implementation of the generic 3-phase external API contract.
    - [read] is used by phase 1 (input adaptation),
    - [write] is used by phase 3 (projection side-effect boundary). *)
module External_api : sig
  type raw_input = Tempo_game.Ui.interaction
  type output_cmd = unit -> unit

  val read : unit -> raw_input
  val write : output_cmd list -> unit
end

module As_backend :
  Tempo_game.Integration.BACKEND
    with type raw_input = External_api.raw_input
     and type output_cmd = External_api.output_cmd

(** Backward-compatible aliases.
    New code should prefer [Backend.*] modules to make backend boundaries explicit. *)
module Ui : sig
  val interaction_from_mouse : unit -> Tempo_game.Ui.interaction
  val draw_button : ?active:bool -> Tempo_game.Ui.button -> unit
  val draw_stepper_int : Tempo_game.Ui.int_stepper -> unit
  val draw_slider_v : Tempo_game.Ui.slider -> unit
end

module Hud : sig
  val draw_panel : Tempo_game.Hud.panel -> unit
  val draw_badge : x:int -> y:int -> Tempo_game.Hud.badge -> unit
  val draw_bar : x:int -> y:int -> w:int -> h:int -> Tempo_game.Hud.bar -> unit
  val draw_timer : x:int -> y:int -> Tempo_game.Hud.timer -> unit
  val draw_message_bar : x:int -> y:int -> w:int -> h:int -> text:string -> unit
end

module Fx : sig
  val draw : Tempo_game.Fx.t -> unit
end

module Audio : sig
  val init : unit -> unit
  val shutdown : unit -> unit
  val make_tone : Tempo_game.Audio.cue -> Raylib.Sound.t
  val play_cue : Tempo_game.Audio.cue -> unit
  val play_tones : (float * float * float) list -> unit
end
