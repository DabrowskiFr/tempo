open Raylib

let to_int f = int_of_float f

module Ui_model = struct
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

  let button ~id rect ~label ?(enabled = true) () = { id; rect; label; enabled }

  let contains (r : rect) (p : pointer) =
    p.x >= r.x && p.x <= r.x +. r.w && p.y >= r.y && p.y <= r.y +. r.h

  let button_pressed i b = b.enabled && i.pressed && contains b.rect i.pointer

  type int_stepper = {
    id : string;
    dec_rect : rect;
    inc_rect : rect;
    value : int;
    min_value : int;
    max_value : int;
    step : int;
  }

  let stepper_int ~id rect ~value ~min_value ~max_value ~step =
    let half = rect.w /. 2.0 in
    {
      id;
      dec_rect = { x = rect.x; y = rect.y; w = half; h = rect.h };
      inc_rect = { x = rect.x +. half; y = rect.y; w = half; h = rect.h };
      value;
      min_value;
      max_value;
      step;
    }

  let stepper_delta i s =
    if i.pressed && contains s.dec_rect i.pointer then -s.step
    else if i.pressed && contains s.inc_rect i.pointer then s.step
    else 0

  type slider = {
    id : string;
    rect : rect;
    t : float;
  }

  let slider_v ~id ~rect ~t =
    let t = if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t in
    { id; rect; t }

  let slider_set i s =
    if i.down && contains s.rect i.pointer then
      let local = (i.pointer.y -. s.rect.y) /. s.rect.h in
      let t = 1.0 -. local in
      let t = if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t in
      Some t
    else None
end

module Hud_model = struct
  type panel = {
    rect : Ui_model.rect;
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

  let panel ~rect ~title = { rect; title }
  let badge ~label ~value = { label; value }
  let bar ~label ~value ~max_value = { label; value; max_value }
  let timer ~seconds_left = { seconds_left = max 0 seconds_left }

  let timer_text t =
    let m = t.seconds_left / 60 in
    let s = t.seconds_left mod 60 in
    Printf.sprintf "%02d:%02d" m s
end

module Fx_model = struct
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

  type t = item list

  let empty = []
  let add st fx = fx :: st
  let toast ~ttl ~text ~kind = Toast { ttl; text; kind }
  let floating_text ~ttl ~text ~x ~y ~kind = Floating_text { ttl; text; x; y; kind }
  let screen_pulse ~ttl ~kind = Screen_pulse { ttl; kind }

  let update_one = function
    | Toast t when t.ttl > 1 -> Some (Toast { t with ttl = t.ttl - 1 })
    | Floating_text t when t.ttl > 1 ->
        Some (Floating_text { t with ttl = t.ttl - 1; y = t.y -. 1.0 })
    | Screen_pulse p when p.ttl > 1 -> Some (Screen_pulse { p with ttl = p.ttl - 1 })
    | _ -> None

  let update st = List.filter_map update_one st
  let active st = List.rev st
end

module Audio_model = struct
  type cue = {
    freq_hz : float;
    duration_s : float;
    volume : float;
  }

  let cue ~freq_hz ~duration_s ~volume = { freq_hz; duration_s; volume }
end

module Tempo_game = struct
  module Ui = Ui_model
  module Hud = Hud_model
  module Fx = Fx_model
  module Audio = Audio_model
end

let rect_contains (r : Ui_model.rect) (p : Ui_model.pointer) = Ui_model.contains r p

module Backend = struct
  module Input = struct
    let interaction_from_mouse () =
      let p = get_mouse_position () in
      {
        Tempo_game.Ui.pointer = { x = Vector2.x p; y = Vector2.y p };
        down = is_mouse_button_down MouseButton.Left;
        pressed = is_mouse_button_pressed MouseButton.Left;
      }
  end

  module Ui_draw = struct
    let draw_button ?(active = false) (b : Tempo_game.Ui.button) =
      let i = Input.interaction_from_mouse () in
      let hovered = rect_contains b.rect i.pointer in
      let fill =
        if not b.enabled then Color.create 68 78 88 210
        else if active then Color.create 101 177 239 255
        else if hovered then Color.create 81 148 208 245
        else Color.create 47 73 96 240
      in
      let border = if hovered then Color.create 222 242 255 255 else Color.create 171 206 234 255 in
      draw_rectangle (to_int b.rect.x) (to_int b.rect.y) (to_int b.rect.w) (to_int b.rect.h) fill;
      draw_rectangle_lines
        (to_int b.rect.x)
        (to_int b.rect.y)
        (to_int b.rect.w)
        (to_int b.rect.h)
        border;
      draw_text b.label (to_int b.rect.x + 10) (to_int b.rect.y + 10) 16 Color.raywhite

    let draw_stepper_int (s : Tempo_game.Ui.int_stepper) =
      let dec_btn = Tempo_game.Ui.button ~id:(s.id ^ ":dec") s.dec_rect ~label:"-" () in
      let inc_btn = Tempo_game.Ui.button ~id:(s.id ^ ":inc") s.inc_rect ~label:"+" () in
      draw_button dec_btn;
      draw_button inc_btn;
      draw_text
        (string_of_int s.value)
        (to_int s.dec_rect.x + (to_int s.dec_rect.w / 2))
        (to_int s.dec_rect.y + to_int s.dec_rect.h + 8)
        14 Color.raywhite

    let draw_slider_v (s : Tempo_game.Ui.slider) =
      let x = to_int s.rect.x in
      let y = to_int s.rect.y in
      let w = to_int s.rect.w in
      let h = to_int s.rect.h in
      let fill_h = int_of_float (s.t *. float_of_int h) in
      let knob_y = y + h - fill_h in
      draw_rectangle x y w h (Color.create 40 60 80 255);
      draw_rectangle x knob_y w fill_h (Color.create 105 190 255 255);
      draw_rectangle_lines x y w h (Color.create 171 206 234 255);
      draw_circle (x + (w / 2)) knob_y 7.0 (Color.create 225 244 255 255)
  end

  module Hud_draw = struct
    let draw_panel (p : Tempo_game.Hud.panel) =
      draw_rectangle
        (to_int p.rect.x)
        (to_int p.rect.y)
        (to_int p.rect.w)
        (to_int p.rect.h)
        (Color.create 13 24 36 220);
      draw_rectangle_lines
        (to_int p.rect.x)
        (to_int p.rect.y)
        (to_int p.rect.w)
        (to_int p.rect.h)
        (Color.create 109 152 186 255);
      draw_text p.title (to_int p.rect.x + 12) (to_int p.rect.y + 10) 18 Color.raywhite

    let draw_badge ~x ~y (b : Tempo_game.Hud.badge) =
      draw_text (b.label ^ " " ^ b.value) x y 18 (Color.create 235 244 252 255)

    let draw_bar ~x ~y ~w ~h (b : Tempo_game.Hud.bar) =
      let max_v = if b.max_value <= 0.0 then 1.0 else b.max_value in
      let t = b.value /. max_v in
      let t = if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t in
      let fw = int_of_float (t *. float_of_int w) in
      draw_rectangle x y w h (Color.create 28 37 45 255);
      draw_rectangle x y fw h (if t > 0.45 then Color.create 97 199 132 255 else Color.create 234 136 87 255);
      draw_rectangle_lines x y w h (Color.create 190 215 235 255);
      draw_text b.label x (y - 16) 14 Color.raywhite

    let draw_timer ~x ~y t =
      draw_text (Tempo_game.Hud.timer_text t) x y 18 (Color.create 208 237 255 255)

    let draw_message_bar ~x ~y ~w ~h ~text =
      draw_rectangle x y w h (Color.create 255 249 230 245);
      draw_rectangle_lines x y w h (Color.create 176 144 88 255);
      draw_text text (x + 12) (y + 5) 18 Color.black
  end

  module Fx_draw = struct
    let color_of_kind = function
      | Tempo_game.Fx.Success -> Color.create 49 159 108 255
      | Tempo_game.Fx.Warn -> Color.create 234 178 66 255
      | Tempo_game.Fx.Error_kind -> Color.create 202 83 64 255
      | Tempo_game.Fx.Info -> Color.create 103 167 228 255

    let color_of_kind_alpha kind a =
      match kind with
      | Tempo_game.Fx.Success -> Color.create 49 159 108 a
      | Tempo_game.Fx.Warn -> Color.create 234 178 66 a
      | Tempo_game.Fx.Error_kind -> Color.create 202 83 64 a
      | Tempo_game.Fx.Info -> Color.create 103 167 228 a

    let draw fx_state =
      let toasts : Tempo_game.Fx.toast list ref = ref [] in
      let floats : Tempo_game.Fx.floating_text list ref = ref [] in
      let pulses : Tempo_game.Fx.screen_pulse list ref = ref [] in
      List.iter
        (function
          | Tempo_game.Fx.Toast t -> toasts := t :: !toasts
          | Tempo_game.Fx.Floating_text f -> floats := f :: !floats
          | Tempo_game.Fx.Screen_pulse p -> pulses := p :: !pulses)
        (Tempo_game.Fx.active fx_state);
      List.iteri
        (fun i (t : Tempo_game.Fx.toast) ->
          let y = 20 + (i * 32) in
          let c = color_of_kind t.Tempo_game.Fx.kind in
          draw_rectangle 20 y 360 24 c;
          draw_text t.Tempo_game.Fx.text 30 (y + 4) 16 Color.raywhite)
        (List.rev !toasts);
      List.iter
        (fun (f : Tempo_game.Fx.floating_text) ->
          draw_text
            f.Tempo_game.Fx.text
            (to_int f.Tempo_game.Fx.x)
            (to_int f.Tempo_game.Fx.y)
            18
            (color_of_kind f.Tempo_game.Fx.kind))
        (List.rev !floats);
      List.iter
        (fun (p : Tempo_game.Fx.screen_pulse) ->
          let a = min 220 (10 + (p.Tempo_game.Fx.ttl * 4)) in
          draw_rectangle
            0
            0
            (get_screen_width ())
            (get_screen_height ())
            (color_of_kind_alpha p.Tempo_game.Fx.kind a))
        !pulses
  end

  module Audio_backend = struct
    let set_u16_le b off v =
      Bytes.set b off (Char.chr (v land 0xFF));
      Bytes.set b (off + 1) (Char.chr ((v lsr 8) land 0xFF))

    let set_u32_le b off v =
      Bytes.set b off (Char.chr Int32.(to_int (logand v 0xFFl)));
      Bytes.set b (off + 1) (Char.chr Int32.(to_int (logand (shift_right_logical v 8) 0xFFl)));
      Bytes.set b (off + 2) (Char.chr Int32.(to_int (logand (shift_right_logical v 16) 0xFFl)));
      Bytes.set b (off + 3) (Char.chr Int32.(to_int (logand (shift_right_logical v 24) 0xFFl)))

    let make_tone (cue : Tempo_game.Audio.cue) =
      let sample_rate = 22050 in
      let samples = max 1 (int_of_float (cue.duration_s *. float_of_int sample_rate)) in
      let data_size = samples * 2 in
      let total = 44 + data_size in
      let b = Bytes.make total '\000' in
      Bytes.blit_string "RIFF" 0 b 0 4;
      set_u32_le b 4 Int32.(of_int (36 + data_size));
      Bytes.blit_string "WAVE" 0 b 8 4;
      Bytes.blit_string "fmt " 0 b 12 4;
      set_u32_le b 16 16l;
      set_u16_le b 20 1;
      set_u16_le b 22 1;
      set_u32_le b 24 Int32.(of_int sample_rate);
      set_u32_le b 28 Int32.(of_int (sample_rate * 2));
      set_u16_le b 32 2;
      set_u16_le b 34 16;
      Bytes.blit_string "data" 0 b 36 4;
      set_u32_le b 40 Int32.(of_int data_size);
      for i = 0 to samples - 1 do
        let t = float_of_int i /. float_of_int sample_rate in
        let raw = sin (2.0 *. Float.pi *. cue.freq_hz *. t) *. 28000.0 in
        let v = int_of_float raw in
        let v = if v < -32768 then -32768 else if v > 32767 then 32767 else v in
        let u = if v < 0 then v + 65536 else v in
        let p = 44 + (i * 2) in
        set_u16_le b p u
      done;
      let wave = load_wave_from_memory ".wav" (Bytes.unsafe_to_string b) total in
      let snd = load_sound_from_wave wave in
      unload_wave wave;
      set_sound_volume snd cue.volume;
      snd

    let init () = if not (is_audio_device_ready ()) then init_audio_device ()

    let shutdown () =
      if is_audio_device_ready () then close_audio_device ()

    let play_cue cue =
      init ();
      let snd = make_tone cue in
      play_sound snd;
      unload_sound snd

    let play_tones tones =
      List.iter
        (fun (freq_hz, duration_s, volume) ->
          play_cue (Tempo_game.Audio.cue ~freq_hz ~duration_s ~volume))
        tones
  end
end

module Ui = struct
  include Ui_model
  let interaction_from_mouse = Backend.Input.interaction_from_mouse
  let draw_button = Backend.Ui_draw.draw_button
  let draw_stepper_int = Backend.Ui_draw.draw_stepper_int
  let draw_slider_v = Backend.Ui_draw.draw_slider_v
end

module Hud = struct
  include Hud_model
  let draw_panel = Backend.Hud_draw.draw_panel
  let draw_badge = Backend.Hud_draw.draw_badge
  let draw_bar = Backend.Hud_draw.draw_bar
  let draw_timer = Backend.Hud_draw.draw_timer
  let draw_message_bar = Backend.Hud_draw.draw_message_bar
end

module Fx = struct
  include Fx_model
  let draw = Backend.Fx_draw.draw
end

module Audio = struct
  include Audio_model
  let init = Backend.Audio_backend.init
  let shutdown = Backend.Audio_backend.shutdown
  let make_tone = Backend.Audio_backend.make_tone
  let play_cue = Backend.Audio_backend.play_cue
  let play_tones = Backend.Audio_backend.play_tones
end
