(** Raylib presentation helpers and lightweight game-oriented models.
    This package is intentionally backend-facing: it provides small pure UI/HUD
    data types together with Raylib interpreters for rendering and simple audio
    cues. *)

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

  type int_stepper = {
    id : string;
    dec_rect : rect;
    inc_rect : rect;
    value : int;
    min_value : int;
    max_value : int;
    step : int;
  }

  type slider = {
    id : string;
    rect : rect;
    t : float;
  }

  val button : id:string -> rect -> label:string -> ?enabled:bool -> unit -> button
  val contains : rect -> pointer -> bool
  val button_pressed : interaction -> button -> bool
  val stepper_int :
    id:string ->
    rect ->
    value:int ->
    min_value:int ->
    max_value:int ->
    step:int ->
    int_stepper
  val stepper_delta : interaction -> int_stepper -> int
  val slider_v : id:string -> rect:rect -> t:float -> slider
  val slider_set : interaction -> slider -> float option

  val interaction_from_mouse : unit -> interaction
  val draw_button : ?active:bool -> button -> unit
  val draw_stepper_int : int_stepper -> unit
  val draw_slider_v : slider -> unit
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

  val draw_panel : panel -> unit
  val draw_badge : x:int -> y:int -> badge -> unit
  val draw_bar : x:int -> y:int -> w:int -> h:int -> bar -> unit
  val draw_timer : x:int -> y:int -> timer -> unit
  val draw_message_bar : x:int -> y:int -> w:int -> h:int -> text:string -> unit
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
  val draw : t -> unit
end

module Audio : sig
  type cue = {
    freq_hz : float;
    duration_s : float;
    volume : float;
  }

  val cue : freq_hz:float -> duration_s:float -> volume:float -> cue
  val init : unit -> unit
  val shutdown : unit -> unit
  val make_tone : cue -> Raylib.Sound.t
  val play_cue : cue -> unit
  val play_tones : (float * float * float) list -> unit
end

module Backend : sig
  module Input : sig
    val interaction_from_mouse : unit -> Ui.interaction
  end

  module Ui_draw : sig
    val draw_button : ?active:bool -> Ui.button -> unit
    val draw_stepper_int : Ui.int_stepper -> unit
    val draw_slider_v : Ui.slider -> unit
  end

  module Hud_draw : sig
    val draw_panel : Hud.panel -> unit
    val draw_badge : x:int -> y:int -> Hud.badge -> unit
    val draw_bar : x:int -> y:int -> w:int -> h:int -> Hud.bar -> unit
    val draw_timer : x:int -> y:int -> Hud.timer -> unit
    val draw_message_bar : x:int -> y:int -> w:int -> h:int -> text:string -> unit
  end

  module Fx_draw : sig
    val draw : Fx.t -> unit
  end

  module Audio_backend : sig
    val init : unit -> unit
    val shutdown : unit -> unit
    val make_tone : Audio.cue -> Raylib.Sound.t
    val play_cue : Audio.cue -> unit
    val play_tones : (float * float * float) list -> unit
  end
end
