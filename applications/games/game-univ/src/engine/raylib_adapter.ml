open Types
open Raylib

let width = 960
let height = 640

let to_int f = int_of_float f

let c_floor_dark = Color.create 31 45 61 255
let c_floor_light = Color.create 58 80 105 255
let c_room_bg = Color.create 225 217 198 255
let c_room_border = Color.create 107 85 58 255
let c_desk = Color.create 181 156 117 255
let c_desk_edge = Color.create 120 96 68 255
let c_shadow = Color.create 0 0 0 55
let c_hud_bg = Color.create 13 24 36 220
let c_panel_border = Color.create 109 152 186 255
let c_msg_bg = Color.create 255 249 230 245
let c_msg_border = Color.create 176 144 88 255
let c_prof_fill = Color.create 60 136 255 255
let c_prof_outline = Color.create 12 51 112 255
let c_student_safe = Color.create 84 184 123 255
let c_student_cheat = Color.create 238 127 52 255
let c_susp_low = Color.create 113 196 90 255
let c_susp_mid = Color.create 247 194 68 255
let c_susp_high = Color.create 230 75 59 255
let c_skin = Color.create 254 224 189 255
let c_hair_dark = Color.create 55 42 39 255
let c_glasses = Color.create 35 35 48 255
let c_shirt_prof = Color.create 79 149 255 255
let c_shirt_student = Color.create 121 196 152 255
let c_shirt_student_cheat = Color.create 247 171 96 255
let c_student_neutral = Color.create 162 168 178 255
let c_student_neutral_dark = Color.create 117 124 136 255
let frame_tick = ref 0
let fx_start_ttl = 32
let wheel_track_x = 874
let wheel_track_y = 146
let wheel_track_h = 324
let wheel_track_w = 14

let success_sound : Sound.t option ref = ref None
let fail_sound : Sound.t option ref = ref None
let success_pulse = ref 0
let fail_pulse = ref 0

let set_u16_le b off v =
  Bytes.set b off (Char.chr (v land 0xFF));
  Bytes.set b (off + 1) (Char.chr ((v lsr 8) land 0xFF))

let set_u32_le b off v =
  Bytes.set b off (Char.chr Int32.(to_int (logand v 0xFFl)));
  Bytes.set b (off + 1) (Char.chr Int32.(to_int (logand (shift_right_logical v 8) 0xFFl)));
  Bytes.set b (off + 2) (Char.chr Int32.(to_int (logand (shift_right_logical v 16) 0xFFl)));
  Bytes.set b (off + 3) (Char.chr Int32.(to_int (logand (shift_right_logical v 24) 0xFFl)))

let make_tone_sound ~freq ~duration ~volume =
  let sample_rate = 22050 in
  let samples = max 1 (int_of_float (duration *. float_of_int sample_rate)) in
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
    let raw = sin (2.0 *. Float.pi *. freq *. t) *. 28000.0 in
    let v = int_of_float raw in
    let v = if v < -32768 then -32768 else if v > 32767 then 32767 else v in
    let u = if v < 0 then v + 65536 else v in
    let p = 44 + (i * 2) in
    set_u16_le b p u
  done;
  let wave = load_wave_from_memory ".wav" (Bytes.unsafe_to_string b) total in
  let snd = load_sound_from_wave wave in
  unload_wave wave;
  set_sound_volume snd volume;
  snd

let init_audio_assets () =
  if not (is_audio_device_ready ()) then init_audio_device ();
  if !success_sound = None then
    success_sound := Some (make_tone_sound ~freq:880.0 ~duration:0.11 ~volume:0.45);
  if !fail_sound = None then
    fail_sound := Some (make_tone_sound ~freq:220.0 ~duration:0.16 ~volume:0.5)

let shutdown_audio_assets () =
  (match !success_sound with Some s -> unload_sound s | None -> ());
  (match !fail_sound with Some s -> unload_sound s | None -> ());
  success_sound := None;
  fail_sound := None;
  if is_audio_device_ready () then close_audio_device ()
let draw_background () =
  draw_rectangle_gradient_v 0 0 width height c_floor_light c_floor_dark;
  for y = 0 to (height / 32) do
    let yy = y * 32 in
    draw_line 0 yy width yy (Color.create 255 255 255 10)
  done

let draw_room () =
  let rx = 70 and ry = 80 and rw = 760 and rh = 460 in
  draw_rectangle (rx + 8) (ry + 8) rw rh c_shadow;
  draw_rectangle rx ry rw rh c_room_bg;
  draw_rectangle_lines rx ry rw rh c_room_border;
  draw_rectangle_lines (rx - 2) (ry - 2) (rw + 4) (rh + 4) (Color.create 70 53 35 180);
  for row = 0 to 1 do
    for col = 0 to 2 do
      let dx = 180 + (col * 160) in
      let dy = 145 + (row * 220) in
      draw_rectangle (dx + 4) (dy + 4) 88 46 c_shadow;
      draw_rectangle dx dy 88 46 c_desk;
      draw_rectangle_lines dx dy 88 46 c_desk_edge
    done
  done;
  draw_text "Salle TP - Controle anti-triche" 84 52 24 c_room_border

let suspicion_color suspicion =
  if suspicion < 35.0 then c_susp_low else if suspicion < 70.0 then c_susp_mid else c_susp_high

let draw_student_hud x y suspicion =
  let bar_w = 42 in
  let filled = int_of_float ((suspicion /. 100.0) *. float_of_int bar_w) in
  let fill_color = suspicion_color suspicion in
  draw_rectangle (x - (bar_w / 2)) (y - 32) bar_w 7 (Color.create 20 20 20 110);
  draw_rectangle (x - (bar_w / 2) + 1) (y - 31) filled 5 fill_color

let draw_radius_wheel detection_radius min_radius max_radius =
  let t =
    if max_radius <= min_radius then 0.0
    else (detection_radius -. min_radius) /. (max_radius -. min_radius)
  in
  let t = max 0.0 (min 1.0 t) in
  let fill_h = int_of_float (t *. float_of_int wheel_track_h) in
  let knob_y = wheel_track_y + wheel_track_h - fill_h in
  draw_text "Rayon" 853 114 17 Color.raywhite;
  draw_rectangle
    (wheel_track_x - 3)
    (wheel_track_y - 3)
    (wheel_track_w + 6)
    (wheel_track_h + 6)
    (Color.create 8 16 24 155);
  draw_rectangle wheel_track_x wheel_track_y wheel_track_w wheel_track_h (Color.create 42 65 85 255);
  draw_rectangle wheel_track_x knob_y wheel_track_w fill_h (Color.create 106 191 255 255);
  draw_rectangle_lines
    wheel_track_x
    wheel_track_y
    wheel_track_w
    wheel_track_h
    (Color.create 163 201 230 255);
  draw_circle
    (wheel_track_x + (wheel_track_w / 2))
    knob_y
    9.0
    (Color.create 225 244 255 255);
  draw_circle_lines
    (wheel_track_x + (wheel_track_w / 2))
    knob_y
    9.0
    (Color.create 68 116 152 255);
  draw_text (Printf.sprintf "%.0f" detection_radius) 858 486 16 Color.raywhite;
  draw_text "[ / ]" 858 507 13 (Color.create 204 226 245 255)

let draw_hud score flagged message detection_radius min_radius max_radius =
  draw_rectangle 18 10 924 44 c_hud_bg;
  draw_rectangle_lines 18 10 924 44 c_panel_border;
  draw_text (Printf.sprintf "Score %d" score) 34 20 20 Color.raywhite;
  draw_text (Printf.sprintf "Signalements %d" flagged) 206 20 20 (Color.create 255 214 102 255);
  draw_text "Fleches: bouger | E: interroger | [ / ]: rayon" 440 20 16 (Color.create 204 226 245 255);
  draw_rectangle 18 592 924 36 c_msg_bg;
  draw_rectangle_lines 18 592 924 36 c_msg_border;
  draw_text message 30 601 20 Color.black;
  draw_radius_wheel detection_radius min_radius max_radius

let draw_face ~blink x y =
  draw_circle x y 8.0 c_skin;
  if blink then (
    draw_line (x - 5) (y - 1) (x - 1) (y - 1) Color.black;
    draw_line (x + 1) (y - 1) (x + 5) (y - 1) Color.black)
  else (
    draw_circle (x - 3) (y - 1) 1.2 Color.black;
    draw_circle (x + 3) (y - 1) 1.2 Color.black);
  draw_line (x - 2) (y + 3) (x + 2) (y + 3) (Color.create 120 58 44 255)

let draw_professor_cartoon x y =
  let bob = int_of_float (sin (float_of_int !frame_tick *. 0.09) *. 2.0) in
  let y = y + bob in
  let blink = !frame_tick mod 140 < 6 in
  draw_circle x (y + 4) 16.0 c_shadow;
  draw_circle x y 14.0 c_shirt_prof;
  draw_circle_lines x y 14.0 c_prof_outline;
  draw_face ~blink x (y - 2);
  draw_rectangle (x - 8) (y - 12) 16 4 c_hair_dark;
  draw_rectangle (x - 10) (y + 9) 20 5 (Color.create 43 89 170 255);
  draw_line (x - 17) (y + 3) (x - 9) (y + 8) c_prof_outline;
  draw_line (x + 17) (y + 3) (x + 9) (y + 8) c_prof_outline

let draw_student_cartoon ~id ~x ~y ~cheating ~in_range ~suspicion =
  let panic = cheating && in_range in
  let shirt =
    if not in_range then c_student_neutral
    else if cheating then c_shirt_student_cheat
    else c_shirt_student
  in
  let accent =
    if not in_range then c_student_neutral_dark
    else if cheating then c_student_cheat
    else c_student_safe
  in
  let bob =
    int_of_float (sin ((float_of_int (!frame_tick + (id * 11))) *. 0.13) *. 1.8)
  in
  let jitter_x =
    if panic then int_of_float (sin (float_of_int (!frame_tick + (id * 9)) *. 0.8) *. 2.0) else 0
  in
  let jitter_y =
    if panic then int_of_float (cos (float_of_int (!frame_tick + (id * 7)) *. 0.7) *. 2.0) else 0
  in
  let blink = (!frame_tick + (id * 17)) mod 160 < 5 in
  let x = x + jitter_x in
  let y = y + bob + jitter_y in
  draw_circle x (y + 4) 14.0 c_shadow;
  draw_ring
    (Vector2.create (float_of_int x) (float_of_int y))
    15.0
    17.5
    0.0
    360.0
    36
    (if in_range then suspicion_color suspicion else c_student_neutral_dark);
  draw_circle x y 12.5 shirt;
  draw_circle_lines x y 12.5 (Color.create 20 33 47 220);
  draw_face ~blink x (y - 3);
  draw_rectangle (x - 8) (y - 14) 16 4 c_hair_dark;
  draw_rectangle (x - 10) (y + 8) 20 5 accent;
  draw_text (Printf.sprintf "S%d" id) (x - 12) (y + 17) 15 (Color.create 24 24 26 255);
  if panic then (
    draw_rectangle_lines (x - 8) (y - 5) 7 5 c_glasses;
    draw_rectangle_lines (x + 1) (y - 5) 7 5 c_glasses;
    draw_line x (y - 3) x (y - 3) c_glasses;
    draw_text "!" (x - 3) (y - 22) 18 (Color.create 103 40 20 255))

let draw_action_feedback (pos : vec2) success label ttl =
  let age = fx_start_ttl - ttl in
  let offset = int_of_float (float_of_int age *. 1.8) in
  let x = to_int pos.x in
  let y = to_int pos.y - 40 - offset in
  let alpha = int_of_float (255.0 *. (float_of_int ttl /. float_of_int fx_start_ttl)) in
  let bubble =
    if success then Color.create 49 159 108 alpha else Color.create 202 83 64 alpha
  in
  let txt = if success then Color.create 236 255 244 alpha else Color.create 255 238 233 alpha in
  draw_circle x (y + 6) 20.0 (Color.create 0 0 0 (alpha / 4));
  draw_rectangle (x - 64) (y - 8) 128 22 bubble;
  draw_rectangle_lines (x - 64) (y - 8) 128 22 (Color.create 25 33 44 alpha);
  draw_text label (x - 57) (y - 2) 16 txt

let draw_screen_pulse () =
  if !success_pulse > 0 then (
    let a = 8 + (!success_pulse * 3) in
    draw_rectangle 0 0 width height (Color.create 71 179 129 a);
    decr success_pulse);
  if !fail_pulse > 0 then (
    let a = 10 + (!fail_pulse * 3) in
    draw_rectangle 0 0 width height (Color.create 214 79 63 a);
    decr fail_pulse)

let render_frame (frame : frame) =
  incr frame_tick;
  let room_present = ref false in
  let students = ref [] in
  let professors = ref [] in
  let action_fx = ref None in
  let hud = ref None in
  List.iter
    (function
      | Draw_room -> room_present := true
      | Draw_student _ as cmd -> students := cmd :: !students
      | Draw_professor _ as cmd -> professors := cmd :: !professors
      | Draw_action_fx (pos, success, label, ttl) ->
          action_fx := Some (pos, success, label, ttl);
          if ttl = fx_start_ttl then (
            if success then (
              success_pulse := 18;
              match !success_sound with Some s -> play_sound s | None -> ())
            else (
              fail_pulse := 18;
              match !fail_sound with Some s -> play_sound s | None -> ()))
      | Draw_hud (score, flagged, message, detection_radius, min_radius, max_radius) ->
          hud := Some (score, flagged, message, detection_radius, min_radius, max_radius))
    frame.draw;
  begin_drawing ();
  draw_background ();
  if !room_present then draw_room ();
  let professor_state =
    match List.rev !professors with
    | Draw_professor (p, _, detection_radius) :: _ -> Some (p.x, p.y, detection_radius)
    | _ -> None
  in
  List.iter
    (function
      | Draw_student (id, p, radius, cheating, suspicion) ->
          let px = to_int p.x in
          let py = to_int p.y in
          let in_range =
            match professor_state with
            | None -> true
            | Some (prof_x, prof_y, detection_radius) ->
                let dx = p.x -. prof_x in
                let dy = p.y -. prof_y in
                (dx *. dx) +. (dy *. dy) <= detection_radius *. detection_radius
          in
          ignore radius;
          draw_student_cartoon ~id ~x:px ~y:py ~cheating ~in_range ~suspicion;
          if in_range then draw_student_hud px py suspicion
      | _ -> ())
    (List.rev !students);
  List.iter
    (function
      | Draw_professor (p, radius, detection_radius) ->
          let px = to_int p.x in
          let py = to_int p.y in
          draw_circle px py detection_radius (Color.create 110 191 255 24);
          draw_circle_lines px py detection_radius (Color.create 110 191 255 115);
          draw_circle_lines px py (radius +. 10.0) (Color.create 106 191 255 160);
          draw_professor_cartoon px py;
          draw_text "Prof" (px - 16) (py + 18) 16 c_prof_outline
      | _ -> ())
    (List.rev !professors);
  (match !action_fx with
  | Some (pos, success, label, ttl) -> draw_action_feedback pos success label ttl
  | None -> ());
  draw_screen_pulse ();
  (match !hud with
  | Some (score, flagged, message, detection_radius, min_radius, max_radius) ->
      draw_hud score flagged message detection_radius min_radius max_radius
  | None -> ());
  end_drawing ()

let read_input () =
  if window_should_close () || is_key_pressed Key.Escape then
    Some
      {
        up = false;
        down = false;
        left = false;
        right = false;
        ask = false;
        radius_down = false;
        radius_up = false;
        radius_set_t = None;
        quit = true;
      }
  else
    let mouse = get_mouse_position () in
    let mx = Vector2.x mouse in
    let my = Vector2.y mouse in
    let on_wheel =
      mx >= float_of_int (wheel_track_x - 16)
      && mx <= float_of_int (wheel_track_x + wheel_track_w + 16)
      && my >= float_of_int wheel_track_y
      && my <= float_of_int (wheel_track_y + wheel_track_h)
    in
    let wheel_move = get_mouse_wheel_move () in
    let drag_t =
      if on_wheel && is_mouse_button_down MouseButton.Left then
        let local_y = Time_helpers.clamp (my -. float_of_int wheel_track_y) ~min:0.0 ~max:(float_of_int wheel_track_h) in
        Some (1.0 -. (local_y /. float_of_int wheel_track_h))
      else None
    in
    Some
      {
        up = is_key_down Key.Up;
        down = is_key_down Key.Down;
        left = is_key_down Key.Left;
        right = is_key_down Key.Right;
        ask = is_key_pressed Key.E;
        radius_down = is_key_down Key.Left_bracket || (on_wheel && wheel_move < 0.0);
        radius_up = is_key_down Key.Right_bracket || (on_wheel && wheel_move > 0.0);
        radius_set_t = drag_t;
        quit = false;
      }

let spec : (input_state, frame) Raylib_platform.spec =
  {
    width;
    height;
    title = "Cheat Detector - Tempo x Raylib";
    fps = 60;
    on_init = init_audio_assets;
    on_shutdown = shutdown_audio_assets;
    read_input;
    render = render_frame;
  }
