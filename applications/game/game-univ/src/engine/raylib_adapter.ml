open Types
open Raylib

let width = 960
let height = 640

let to_int f = int_of_float f

let c_shadow = Color.create 0 0 0 55
let c_hud_bg = Color.create 10 22 35 232
let c_panel_border = Color.create 120 170 210 255
let c_board = Color.create 18 48 43 244
let c_board_deep = Color.create 11 34 30 248
let c_chalk = Color.create 231 244 255 255
let c_chalk_soft = Color.create 176 214 232 255
let c_wood = Color.create 125 84 48 255
let c_wood_dark = Color.create 90 58 33 255
let c_wood_light = Color.create 158 112 68 255
let c_btn_on = Color.create 49 113 131 255
let c_btn_off = Color.create 34 72 84 255
let c_btn_border = Color.create 146 197 216 255
let c_btn_border_off = Color.create 103 144 160 255
let c_prof_fill = Color.create 72 154 255 255
let c_prof_outline = Color.create 17 38 71 255
let c_student_safe = Color.create 94 201 136 255
let c_student_cheat = Color.create 245 133 69 255
let c_susp_low = Color.create 113 196 90 255
let c_susp_mid = Color.create 247 194 68 255
let c_susp_high = Color.create 230 75 59 255
let c_skin = Color.create 254 224 189 255
let c_hair_dark = Color.create 71 52 45 255
let c_glasses = Color.create 38 31 42 255
let c_shirt_prof = Color.create 79 149 255 255
let c_shirt_student = Color.create 121 196 152 255
let c_shirt_student_cheat = Color.create 247 171 96 255
let c_student_neutral = Color.create 170 173 182 255
let c_student_neutral_dark = Color.create 98 106 121 255
let frame_tick = ref 0
let fx_start_ttl = 32
let btn_x = 804
let btn_w = 138
let btn_h = 40
let btn_start_y = 196
let btn_pause_y = 246
let btn_restart_y = 296
let btn_cheat_y = 362
let btn_diff_h = 26
let btn_diff_gap = 6
let tone_cache : (string, Sound.t) Hashtbl.t = Hashtbl.create 128

module Tokens = struct
  let panel_x = width - 166
  let panel_w = 156
  let panel_y = 162
  let panel_h = 276
  let room_x = 70
  let room_y = 94
  let room_w = 720
  let room_h = 466
end

type ambience = {
  label : string;
  floor_dark : Color.t;
  floor_light : Color.t;
  room_bg : Color.t;
  room_border : Color.t;
  desk : Color.t;
  desk_edge : Color.t;
  accent : Color.t;
}

let ambience_of_round round_index =
  match (max 1 round_index - 1) mod 3 with
  | 0 ->
      {
        label = "Matin";
        floor_dark = Color.create 27 52 82 255;
        floor_light = Color.create 84 141 192 255;
        room_bg = Color.create 241 231 208 255;
        room_border = Color.create 114 87 53 255;
        desk = Color.create 218 170 102 255;
        desk_edge = Color.create 125 82 45 255;
        accent = Color.create 121 186 242 255;
      }
  | 1 ->
      {
        label = "Apres-midi";
        floor_dark = Color.create 52 68 77 255;
        floor_light = Color.create 171 128 87 255;
        room_bg = Color.create 238 221 186 255;
        room_border = Color.create 122 88 49 255;
        desk = Color.create 212 147 78 255;
        desk_edge = Color.create 126 78 39 255;
        accent = Color.create 244 183 106 255;
      }
  | _ ->
      {
        label = "Soir";
        floor_dark = Color.create 21 32 58 255;
        floor_light = Color.create 104 95 160 255;
        room_bg = Color.create 223 214 198 255;
        room_border = Color.create 91 78 92 255;
        desk = Color.create 161 128 156 255;
        desk_edge = Color.create 95 72 102 255;
        accent = Color.create 168 158 236 255;
      }

let init_audio_assets () =
  Tempo_game_raylib.Audio.init ()

let shutdown_audio_assets () =
  Hashtbl.iter (fun _ snd -> unload_sound snd) tone_cache;
  Hashtbl.reset tone_cache;
  Tempo_game_raylib.Audio.shutdown ()

let tone_key freq duration volume =
  Printf.sprintf "%.3f|%.3f|%.3f" freq duration volume

let play_tone_cached freq duration volume =
  let key = tone_key freq duration volume in
  let snd =
    match Hashtbl.find_opt tone_cache key with
    | Some s -> s
    | None ->
        let s =
          Tempo_game_raylib.Audio.make_tone
            (Tempo_game.Audio.cue ~freq_hz:freq ~duration_s:duration ~volume)
        in
        Hashtbl.add tone_cache key s;
        s
  in
  play_sound snd
let draw_background (a : ambience) =
  draw_rectangle_gradient_v 0 0 width height a.floor_light a.floor_dark;
  for y = 0 to (height / 32) do
    let yy = y * 32 in
    draw_line 0 yy width yy (Color.create 255 255 255 10)
  done;
  for x = 0 to (width / 48) do
    let xx = x * 48 in
    draw_line xx 0 xx height (Color.create 255 255 255 6)
  done;
  draw_rectangle 0 0 width 86 (Color.create 7 18 30 110);
  let glow = 26 + (int_of_float ((sin (float_of_int !frame_tick *. 0.03) +. 1.0) *. 8.0)) in
  draw_rectangle_gradient_h 0 0 width 86 (Color.create 255 255 255 glow) (Color.create 255 255 255 0)

let draw_room (a : ambience) =
  let rx = Tokens.room_x
  and ry = Tokens.room_y
  and rw = Tokens.room_w
  and rh = Tokens.room_h in
  draw_rectangle (rx + 14) (ry + 14) rw rh (Color.create 0 0 0 78);
  draw_rectangle rx ry rw rh a.room_bg;
  draw_rectangle_lines rx ry rw rh a.room_border;
  draw_rectangle_lines (rx - 2) (ry - 2) (rw + 4) (rh + 4) (Color.create 70 53 35 210);
  for y = ry + 14 to ry + rh - 14 do
    if y mod 22 = 0 then draw_line (rx + 8) y (rx + rw - 8) y (Color.create 132 115 90 28)
  done;
  for row = 0 to 1 do
    for col = 0 to 2 do
      let dx = 160 + (col * 152) in
      let dy = 145 + (row * 220) in
      draw_rectangle (dx + 5) (dy + 6) 88 46 (Color.create 0 0 0 72);
      draw_rectangle dx dy 88 46 a.desk;
      draw_rectangle dx (dy + 29) 88 15 (Color.create 172 132 84 180);
      draw_rectangle_lines dx dy 88 46 a.desk_edge
    done
  done

let draw_right_panel_back (a : ambience) =
  draw_rectangle (Tokens.panel_x + 8) (Tokens.panel_y + 10) Tokens.panel_w Tokens.panel_h (Color.create 0 0 0 70);
  draw_rectangle (Tokens.panel_x - 2) (Tokens.panel_y - 2) (Tokens.panel_w + 4) (Tokens.panel_h + 4) c_wood_dark;
  draw_rectangle (Tokens.panel_x - 1) (Tokens.panel_y - 1) (Tokens.panel_w + 2) 6 c_wood_light;
  draw_rectangle Tokens.panel_x Tokens.panel_y Tokens.panel_w Tokens.panel_h c_wood;
  draw_rectangle (Tokens.panel_x + 6) (Tokens.panel_y + 8) (Tokens.panel_w - 12) (Tokens.panel_h - 14) (Color.create 16 39 58 200);
  draw_rectangle_lines Tokens.panel_x Tokens.panel_y Tokens.panel_w Tokens.panel_h a.accent;
  draw_rectangle_lines (Tokens.panel_x + 6) (Tokens.panel_y + 8) (Tokens.panel_w - 12) (Tokens.panel_h - 14) (Color.create 133 174 206 170);
  draw_rectangle (Tokens.panel_x + 12) (Tokens.panel_y + 10) (Tokens.panel_w - 24) 22 c_wood_dark;
  draw_rectangle_lines (Tokens.panel_x + 12) (Tokens.panel_y + 10) (Tokens.panel_w - 24) 22 c_wood_light;
  draw_line (Tokens.panel_x + 14) (Tokens.panel_y + 350) (Tokens.panel_x + Tokens.panel_w - 14) (Tokens.panel_y + 350) (Color.create 138 178 207 150)

let suspicion_color suspicion =
  if suspicion < 35.0 then c_susp_low else if suspicion < 70.0 then c_susp_mid else c_susp_high

let difficulty_label_of_factor factor =
  if factor < 0.93 then "Facile"
  else if factor < 1.1 then "Normal"
  else "Difficile"

let difficulty_level_of_factor factor =
  if factor < 0.93 then 0
  else if factor < 1.1 then 1
  else 2

let draw_student_hud ~x ~y ~suspicion ~in_range =
  if in_range then (
    let w = 56 in
    let h = 12 in
    let px = x - (w / 2) in
    let py = y - 46 in
    let pct = int_of_float (Time_helpers.clamp suspicion ~min:0.0 ~max:100.0) in
    let filled = int_of_float ((float_of_int (w - 2) *. float_of_int pct) /. 100.0) in
    let fill_color = suspicion_color suspicion in
    draw_rectangle px py w h (Color.create 16 23 30 220);
    draw_rectangle (px + 1) (py + 1) filled (h - 2) fill_color;
    draw_rectangle_lines px py w h (Color.create 195 218 238 235);
    draw_text (Printf.sprintf "%d%%" pct) (x - 13) (py - 12) 11 (Color.create 230 242 252 255))
  else (
    let w = 38 in
    let h = 10 in
    let px = x - (w / 2) in
    let py = y - 44 in
    draw_rectangle px py w h (Color.create 65 73 84 205);
    draw_rectangle_lines px py w h (Color.create 150 162 179 215))

let draw_button ~x ~y ~w ~h ~label ~active =
  let fill = if active then c_btn_on else c_btn_off in
  let border = if active then c_btn_border else c_btn_border_off in
  let text_col = if active then c_chalk else c_chalk_soft in
  draw_rectangle (x + 2) (y + 3) w h (Color.create 0 0 0 55);
  draw_rectangle x y w h fill;
  draw_rectangle (x + 1) (y + 1) (w - 2) 10 (Color.create 255 255 255 22);
  draw_rectangle_lines x y w h border;
  draw_rectangle_lines (x + 2) (y + 2) (w - 4) (h - 4) (Color.create 20 44 57 170);
  let tw = measure_text label 17 in
  draw_text label (x + ((w - tw) / 2)) (y + 10) 17 text_col

let draw_control_stack ~started ~paused ~game_over ~difficulty =
  let draw_diff_button y ~label ~active =
    draw_button ~x:btn_x ~y ~w:btn_w ~h:btn_diff_h ~label ~active
  in
  draw_rectangle (btn_x - 4) (btn_start_y - 8) (btn_w + 8) 52 (Color.create 10 28 43 195);
  draw_rectangle_lines (btn_x - 4) (btn_start_y - 8) (btn_w + 8) 52 (Color.create 90 132 165 150);
  draw_button ~x:btn_x ~y:btn_start_y ~w:btn_w ~h:btn_h
    ~label:(if started then "Start" else "Start >")
    ~active:(not started);
  draw_rectangle (btn_x - 4) (btn_pause_y - 8) (btn_w + 8) 52 (Color.create 10 28 43 195);
  draw_rectangle_lines (btn_x - 4) (btn_pause_y - 8) (btn_w + 8) 52 (Color.create 90 132 165 150);
  draw_button ~x:btn_x ~y:btn_pause_y ~w:btn_w ~h:btn_h ~label:(if paused then "> Reprise" else "|| Pause")
    ~active:(started && not game_over);
  draw_rectangle (btn_x - 4) (btn_restart_y - 8) (btn_w + 8) 52 (Color.create 10 28 43 195);
  draw_rectangle_lines (btn_x - 4) (btn_restart_y - 8) (btn_w + 8) 52 (Color.create 90 132 165 150);
  draw_button ~x:btn_x ~y:btn_restart_y ~w:btn_w ~h:btn_h ~label:"Recommencer" ~active:true;
  draw_line (btn_x - 2) (btn_cheat_y - 28) (btn_x + btn_w + 2) (btn_cheat_y - 28) (Color.create 139 186 218 130);
  draw_text "Difficulte" (btn_x + 25) (btn_cheat_y - 22) 16 c_chalk_soft;
  draw_diff_button btn_cheat_y ~label:"Facile" ~active:(difficulty = 0);
  draw_diff_button (btn_cheat_y + btn_diff_h + btn_diff_gap) ~label:"Normal" ~active:(difficulty = 1);
  draw_diff_button (btn_cheat_y + (2 * (btn_diff_h + btn_diff_gap))) ~label:"Difficile" ~active:(difficulty = 2)

let draw_hot_zone_overlay zone_idx =
  let rx = Tokens.room_x in
  let ry = Tokens.room_y in
  let rw = Tokens.room_w in
  let rh = Tokens.room_h in
  let zone_w = rw / 3 in
  let zx = rx + (zone_idx * zone_w) in
  let zw = if zone_idx = 2 then rw - (2 * zone_w) else zone_w in
  draw_rectangle zx ry zw rh (Color.create 110 214 182 16);
  draw_rectangle_lines zx ry zw rh (Color.create 103 207 176 130);
  draw_rectangle (zx + 8) (ry + 8) (zw - 16) 18 (Color.create 12 44 52 130);
  draw_text "ZONE ACTIVE" (zx + 16) (ry + 11) 12 (Color.create 186 236 226 255)

let draw_hud (h : hud_payload) =
  let e = int_of_float h.energy in
  let ew = 160 in
  let total_asks = h.catches + h.false_positives + h.empty_checks in
  let precision =
    if total_asks = 0 then 100.0
    else (float_of_int h.catches *. 100.0) /. float_of_int total_asks
  in
  let combo_seconds = h.combo_window_left / 60 in
  let difficulty = difficulty_level_of_factor h.cheat_window_factor in
  let panel =
    Tempo_game.Hud.panel
      ~rect:{ Tempo_game.Ui.x = 18.0; y = 8.0; w = 924.0; h = 80.0 }
      ~title:""
  in
  let draw_data_box x y w h =
    draw_rectangle (x - 2) (y - 2) (w + 4) (h + 4) c_wood_dark;
    draw_rectangle x y w h c_board;
    draw_rectangle_gradient_v x y w h (Color.create 255 255 255 8) (Color.create 255 255 255 0);
    draw_rectangle_lines x y w h (Color.create 108 156 145 235);
    draw_rectangle_lines (x + 2) (y + 2) (w - 4) (h - 4) (Color.create 40 88 81 230)
  in
  Tempo_game_raylib.Hud.draw_panel panel;
  draw_rectangle 16 6 928 84 c_wood_dark;
  draw_rectangle 18 8 924 80 c_wood;
  draw_rectangle 24 14 912 68 c_board_deep;
  draw_rectangle_lines 18 8 924 80 c_panel_border;
  draw_data_box 26 16 116 62;
  draw_data_box 150 16 170 62;
  draw_data_box 328 16 286 62;
  draw_data_box 622 16 312 62;
  draw_line 314 18 314 74 (Color.create 123 175 167 130);
  draw_line 618 18 618 74 (Color.create 123 175 167 130);
  draw_text "SCORE" 34 20 13 c_chalk_soft;
  draw_text (string_of_int h.score) 34 34 36 c_chalk;
  draw_text "SIGNALEMENTS" 160 22 13 c_chalk_soft;
  draw_text (Printf.sprintf "%d" h.flagged) 220 42 30 (Color.create 255 221 117 255);
  draw_text "COMBO" 338 20 13 c_chalk_soft;
  draw_text (Printf.sprintf "x%d" h.combo) 338 34 32
    (if h.combo > 0 then Color.create 255 216 118 255 else c_chalk_soft);
  draw_text (Printf.sprintf "Best x%d | Precision %.0f%%" h.combo_best precision) 426 47 17 c_chalk;
  draw_text "ENERGIE" 632 20 13 c_chalk_soft;
  Tempo_game_raylib.Hud.draw_bar ~x:744 ~y:16 ~w:ew ~h:12
    (Tempo_game.Hud.bar ~label:"" ~value:h.energy ~max_value:100.0);
  draw_text (Printf.sprintf "%d%%" e) 890 14 13 c_chalk;
  draw_text
    (Printf.sprintf "Difficulte %s" (difficulty_label_of_factor h.cheat_window_factor))
    640 43 16 c_chalk;
  draw_text (Printf.sprintf "Fenetre combo %ds" combo_seconds) 744 43 15 c_chalk;
  if h.focus_left > 0 then draw_text "FOCUS" 892 42 13 (Color.create 255 228 114 255);
  draw_text
    (Printf.sprintf "Zone %s (%ds)" h.hot_zone_label (h.hot_zone_left / 60))
    640 62 13 c_chalk_soft;
  draw_control_stack ~started:h.started ~paused:h.paused ~game_over:h.game_over
    ~difficulty;
  let msg_lower = String.lowercase_ascii h.message in
  let has_sub s sub =
    let ls = String.length s and lsub = String.length sub in
    let rec loop i =
      if i + lsub > ls then false
      else if String.sub s i lsub = sub then true
      else loop (i + 1)
    in
    loop 0
  in
  let icon_col, icon_txt =
    if has_sub msg_lower "faux" then (Color.create 206 96 75 255, "!")
    else if has_sub msg_lower "flagrant" then (Color.create 78 166 117 255, "+")
    else (Color.create 90 141 197 255, "i")
  in
  Tempo_game_raylib.Hud.draw_message_bar ~x:18 ~y:602 ~w:924 ~h:26 ~text:"";
  draw_rectangle 16 600 928 30 c_wood_dark;
  draw_rectangle 18 602 924 26 (Color.create 228 219 197 252);
  draw_rectangle_lines 18 602 924 26 (Color.create 120 97 66 210);
  draw_circle 34 615 8.0 icon_col;
  draw_text icon_txt 31 607 15 Color.raywhite;
  draw_text "Consigne:" 48 607 16 (Color.create 66 51 35 255);
  draw_text h.message 132 607 16 Color.black;
  draw_text "Fleches deplacement | E interroger | C cafe" 610 585 14 c_chalk_soft

let draw_round_hud (h : hud_payload) =
  let total_seconds = h.round_left / 60 in
  let sec = total_seconds mod 60 in
  let min_ = total_seconds / 60 in
  let timer_color =
    if h.game_over then Color.create 235 129 116 255
    else if total_seconds <= 10 then Color.create 255 142 69 255
    else Color.create 208 237 255 255
  in
  draw_text
    (Printf.sprintf "Manche %d/%d" h.round_index h.rounds_total)
    752 62 13
    (Color.create 195 225 246 255);
  draw_text
    (Printf.sprintf "Temps %02d:%02d" min_ sec)
    842 62 13
    timer_color

let draw_results_overlay score catches false_positives empty_checks combo_best =
  draw_rectangle 0 0 width height (Color.create 8 14 22 170);
  draw_rectangle 218 138 524 354 (Color.create 20 36 52 235);
  draw_rectangle_lines 218 138 524 354 (Color.create 126 177 222 255);
  draw_text "Fin de partie" 386 160 42 (Color.create 222 239 255 255);
  draw_text (Printf.sprintf "Score final: %d" score) 330 240 32 (Color.create 201 233 255 255);
  draw_text (Printf.sprintf "Flagrants delits: %d" catches) 330 290 24 (Color.create 83 210 147 255);
  draw_text (Printf.sprintf "Faux positifs: %d" false_positives) 330 322 24 (Color.create 242 122 102 255);
  draw_text (Printf.sprintf "Personne proche: %d" empty_checks) 330 354 24 (Color.create 177 194 208 255);
  draw_text (Printf.sprintf "Meilleur combo: x%d" combo_best) 330 386 24 (Color.create 255 214 124 255);
  draw_text "Appuyez sur R pour relancer une partie" 274 424 22 (Color.create 219 233 247 255)

let draw_coffee_station (p : vec2) progress =
  let x = to_int p.x in
  let y = to_int p.y in
  draw_circle x y 16.0 (Color.create 93 64 44 235);
  draw_circle x y 11.0 (Color.create 200 150 100 240);
  draw_circle x (y - 1) 6.0 (Color.create 78 51 34 255);
  draw_circle_lines x y 16.0 (Color.create 50 30 18 220);
  draw_text "CAFE" (x - 21) (y + 18) 12 (Color.create 67 44 29 255);
  if progress > 0.0 then (
    let w = 52 in
    let f = int_of_float (progress *. float_of_int w) in
    draw_rectangle (x - 26) (y - 31) w 7 (Color.create 22 30 36 180);
    draw_rectangle (x - 26) (y - 31) f 7 (Color.create 103 191 233 255);
    draw_rectangle_lines (x - 26) (y - 31) w 7 (Color.create 190 228 255 255))

let draw_face ~blink x y =
  draw_circle x y 8.0 c_skin;
  if blink then (
    draw_line (x - 5) (y - 1) (x - 1) (y - 1) Color.black;
    draw_line (x + 1) (y - 1) (x + 5) (y - 1) Color.black)
  else (
    draw_circle (x - 3) (y - 1) 1.2 Color.black;
    draw_circle (x + 3) (y - 1) 1.2 Color.black);
  draw_line (x - 2) (y + 3) (x + 2) (y + 3) (Color.create 120 58 44 255);
  draw_circle_lines x y 8.0 (Color.create 43 34 32 160)

let draw_professor_cartoon x y =
  let bob = int_of_float (sin (float_of_int !frame_tick *. 0.11) *. 3.0) in
  let y = y + bob in
  let blink = !frame_tick mod 140 < 6 in
  let body_r = 13.5 +. (sin (float_of_int !frame_tick *. 0.19) *. 1.0) in
  draw_circle x (y + 4) 17.0 (Color.create 0 0 0 76);
  draw_circle x y body_r c_shirt_prof;
  draw_circle_lines x y body_r c_prof_outline;
  draw_circle_lines x y (body_r +. 1.5) (Color.create 9 24 48 220);
  draw_face ~blink x (y - 2);
  draw_rectangle (x - 8) (y - 12) 16 4 c_hair_dark;
  draw_rectangle (x - 10) (y + 9) 20 5 (Color.create 43 89 170 255);
  draw_line (x - 17) (y + 3) (x - 9) (y + 8) c_prof_outline;
  draw_line (x + 17) (y + 3) (x + 9) (y + 8) c_prof_outline

let profile_text = function
  | Prudent -> "Prudent"
  | Opportunist -> "Opportuniste"
  | Chaotic -> "Chaotique"

let draw_student_cartoon ~id ~x ~y ~cheating ~in_range ~suspicion ~profile ~tell =
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
    int_of_float (sin ((float_of_int (!frame_tick + (id * 11))) *. 0.15) *. 2.6)
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
  let body_r = 12.0 +. (sin (float_of_int (!frame_tick + (id * 23)) *. 0.23) *. 0.9) in
  draw_circle x (y + 4) 15.0 (Color.create 0 0 0 68);
  draw_ring
    (Vector2.create (float_of_int x) (float_of_int y))
    15.0
    18.0
    0.0
    360.0
    36
    (if in_range then suspicion_color suspicion else c_student_neutral_dark);
  draw_circle x y body_r shirt;
  draw_circle_lines x y body_r (Color.create 22 28 38 220);
  draw_circle_lines x y (body_r +. 1.4) (Color.create 11 16 22 210);
  draw_face ~blink x (y - 3);
  draw_rectangle (x - 8) (y - 14) 16 4 c_hair_dark;
  draw_rectangle (x - 10) (y + 8) 20 5 accent;
  draw_text (Printf.sprintf "S%d" id) (x - 12) (y + 17) 15 (Color.create 24 24 26 255);
  if tell > 0.55 && not cheating then (
    let a = int_of_float (110.0 +. (tell *. 120.0)) in
    let cx = x in
    let cy = y - 21 in
    draw_circle cx cy 7.0 (Color.create 253 215 116 (min 240 a));
    draw_circle_lines cx cy 7.0 (Color.create 136 95 28 a);
    draw_circle cx (cy - 2) 1.1 (Color.create 95 68 18 a);
    draw_rectangle (cx - 1) cy 2 4 (Color.create 95 68 18 a));
  ignore profile;
  if cheating && (not in_range) then (
    draw_circle_lines x (y - 20) 6.5 (Color.create 226 159 74 180);
    draw_circle x (y - 20) 2.0 (Color.create 226 159 74 210));
  if panic then (
    draw_rectangle_lines (x - 8) (y - 5) 7 5 c_glasses;
    draw_rectangle_lines (x + 1) (y - 5) 7 5 c_glasses;
    draw_line x (y - 3) x (y - 3) c_glasses;
    draw_rectangle (x - 5) (y - 27) 10 10 (Color.create 210 80 56 245);
    draw_rectangle_lines (x - 5) (y - 27) 10 10 (Color.create 92 32 22 255);
    draw_rectangle (x - 1) (y - 24) 2 4 (Color.create 255 232 213 255);
    draw_rectangle (x - 1) (y - 19) 2 2 (Color.create 255 232 213 255))

let fx_kind_of_action = function
  | Flagrant -> Tempo_game.Fx.Success
  | False_positive -> Tempo_game.Fx.Error_kind
  | Empty_check -> Tempo_game.Fx.Warn

let draw_action_feedback (pos : vec2) kind label ttl =
  let k = fx_kind_of_action kind in
  let fx =
    Tempo_game.Fx.empty
    |> fun st ->
    Tempo_game.Fx.add st (Tempo_game.Fx.floating_text ~ttl ~text:label ~x:(pos.x -. 58.0) ~y:(pos.y -. 36.0) ~kind:k)
    |> fun st ->
    Tempo_game.Fx.add st (Tempo_game.Fx.screen_pulse ~ttl:(min ttl 16) ~kind:k)
  in
  Tempo_game_raylib.Fx.draw fx

let render_frame (frame : frame) =
  incr frame_tick;
  let room_present = ref false in
  let hot_zone = ref None in
  let students = ref [] in
  let professors = ref [] in
  let coffees = ref [] in
  let action_fx = ref None in
  let hud = ref None in
  let results = ref None in
  List.iter
    (function
      | Draw_room -> room_present := true
      | Draw_hot_zone z -> hot_zone := Some z
      | Draw_student _ as cmd -> students := cmd :: !students
      | Draw_professor _ as cmd -> professors := cmd :: !professors
      | Draw_coffee _ as cmd -> coffees := cmd :: !coffees
      | Draw_action_fx (pos, kind, label, ttl) ->
          action_fx := Some (pos, kind, label, ttl)
      | Draw_hud payload -> hud := Some payload
      | Draw_results (score, catches, false_positives, empty_checks, combo_best) ->
          results := Some (score, catches, false_positives, empty_checks, combo_best))
    frame.draw;
  let tones =
    List.filter_map
      (function
        | Play_cue Cue_success -> Some (880.0, 0.11, 0.45)
        | Play_cue Cue_false_positive -> Some (220.0, 0.16, 0.50)
        | Play_cue Cue_empty_check -> Some (310.0, 0.08, 0.36)
        | Play_cue Cue_warning -> Some (698.46, 0.08, 0.22)
        | Play_cue (Cue_music { freq_hz; duration_s; volume }) ->
            Some (freq_hz, duration_s, volume))
      frame.audio
  in
  List.iter
    (fun (freq, duration, volume) -> play_tone_cached freq duration volume)
    tones;
  let ambience =
    match !hud with
    | Some h -> ambience_of_round h.round_index
    | None -> ambience_of_round 1
  in
  begin_drawing ();
  draw_background ambience;
  if !room_present then draw_room ambience;
  (match !hot_zone with Some z -> draw_hot_zone_overlay z | None -> ());
  draw_right_panel_back ambience;
  List.iter
    (function Draw_coffee (p, progress) -> draw_coffee_station p progress | _ -> ())
    (List.rev !coffees);
  let professor_state =
    match List.rev !professors with
    | Draw_professor (p, _, detection_radius) :: _ -> Some (p.x, p.y, detection_radius)
    | _ -> None
  in
  List.iter
    (function
      | Draw_student (id, p, radius, cheating, suspicion, profile, tell) ->
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
          draw_student_cartoon ~id ~x:px ~y:py ~cheating ~in_range ~suspicion ~profile ~tell;
          draw_student_hud ~x:px ~y:py ~suspicion ~in_range
      | _ -> ())
    (List.rev !students);
  List.iter
    (function
      | Draw_professor (p, radius, detection_radius) ->
          let px = to_int p.x in
          let py = to_int p.y in
          let energy_level =
            match !hud with
            | Some h -> h.energy
            | None -> 100.0
          in
          let rad_col =
            if energy_level > 66.0 then Color.create 103 211 159 170
            else if energy_level > 33.0 then Color.create 240 193 83 170
            else Color.create 235 124 89 170
          in
          draw_circle px py detection_radius (Color.create 110 191 255 10);
          let seg_count = 24 in
          let seg_angle = 360.0 /. float_of_int seg_count in
          for i = 0 to seg_count - 1 do
            if i mod 2 = 0 then
              let start_a = (float_of_int i *. seg_angle) +. float_of_int (!frame_tick mod 360) in
              let end_a = start_a +. (seg_angle *. 0.7) in
              draw_ring
                (Vector2.create (float_of_int px) (float_of_int py))
                (detection_radius -. 1.0)
                (detection_radius +. 1.0)
                start_a
                end_a
                8
                rad_col
          done;
          draw_circle_lines px py (radius +. 10.0) (Color.create 106 191 255 160);
          draw_professor_cartoon px py;
          draw_text "Prof" (px - 16) (py + 18) 16 c_prof_outline
      | _ -> ())
    (List.rev !professors);
  (match !action_fx with
  | Some (pos, kind, label, ttl) -> draw_action_feedback pos kind label ttl
  | None -> ());
  (match !hud with
  | Some h ->
      draw_hud h;
      draw_round_hud h
  | None -> ());
  (match !results with
  | Some (score, catches, false_positives, empty_checks, combo_best) ->
      draw_results_overlay score catches false_positives empty_checks combo_best
  | None -> ());
  end_drawing ()

let read_input () =
  let inside_button x y by =
    x >= btn_x && x <= btn_x + btn_w && y >= by && y <= by + btn_h
  in
  let inside_diff_button x y by =
    x >= btn_x && x <= btn_x + btn_w && y >= by && y <= by + btn_diff_h
  in
  if window_should_close () || is_key_pressed Key.Escape then
    Some
      {
        start = false;
        pause_toggle = false;
        difficulty_set = None;
        up = false;
        down = false;
        left = false;
        right = false;
        ask = false;
        drink = false;
        restart = false;
        radius_down = false;
        radius_up = false;
        radius_set_t = None;
        quit = true;
      }
  else
    let mouse = get_mouse_position () in
    let mx = int_of_float (Vector2.x mouse) in
    let my = int_of_float (Vector2.y mouse) in
    let click = is_mouse_button_pressed MouseButton.Left in
    let diff1_y = btn_cheat_y in
    let diff2_y = btn_cheat_y + btn_diff_h + btn_diff_gap in
    let diff3_y = btn_cheat_y + (2 * (btn_diff_h + btn_diff_gap)) in
    let difficulty_set =
      if is_key_pressed Key.One || (click && inside_diff_button mx my diff1_y) then Some 0
      else if is_key_pressed Key.Two || (click && inside_diff_button mx my diff2_y) then Some 1
      else if is_key_pressed Key.Three || (click && inside_diff_button mx my diff3_y) then Some 2
      else None
    in
    Some
      {
        start =
          is_key_pressed Key.Enter
          || is_key_pressed Key.Space
          || (click && inside_button mx my btn_start_y);
        pause_toggle =
          is_key_pressed Key.P
          || (click && inside_button mx my btn_pause_y);
        difficulty_set;
        up = is_key_down Key.Up;
        down = is_key_down Key.Down;
        left = is_key_down Key.Left;
        right = is_key_down Key.Right;
        ask = is_key_pressed Key.E;
        drink = is_key_down Key.C;
        restart = is_key_pressed Key.R || (click && inside_button mx my btn_restart_y);
        radius_down = false;
        radius_up = false;
        radius_set_t = None;
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
