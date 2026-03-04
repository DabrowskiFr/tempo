type vec2 = { mutable x : float; mutable y : float }

type input_state = {
  start : bool;
  pause_toggle : bool;
  difficulty_set : int option;
  up : bool;
  down : bool;
  left : bool;
  right : bool;
  ask : bool;
  drink : bool;
  restart : bool;
  radius_down : bool;
  radius_up : bool;
  radius_set_t : float option;
  quit : bool;
}

type professor = {
  pos : vec2;
  speed : float;
  radius : float;
}

type student_profile =
  | Prudent
  | Opportunist
  | Chaotic

type student = {
  id : int;
  pos : vec2;
  profile : student_profile;
  mutable cheating : bool;
  mutable cheat_hold : int;
  mutable caught_cooldown : int;
  mutable tell : float;
}

type event =
  | Cheating_start of int
  | Cheating_stop of int
  | Student_flagged of int
  | Ask_success of int
  | Ask_miss of int option
  | Ask_feedback of vec2 * bool * string

type action_kind =
  | Flagrant
  | False_positive
  | Empty_check

type hud_payload = {
  score : int;
  flagged : int;
  message : string;
  round_index : int;
  rounds_total : int;
  round_left : int;
  started : bool;
  paused : bool;
  game_over : bool;
  energy : float;
  focus_left : int;
  cheat_window_factor : float;
  hot_zone_label : string;
  hot_zone_left : int;
  catches : int;
  false_positives : int;
  empty_checks : int;
  combo : int;
  combo_best : int;
  combo_window_left : int;
}

type draw_cmd =
  | Draw_room
  | Draw_hot_zone of int
  | Draw_professor of vec2 * float * float
  | Draw_coffee of vec2 * float
  | Draw_student of int * vec2 * float * bool * float * student_profile * float
  | Draw_action_fx of vec2 * action_kind * string * int
  | Draw_hud of hud_payload
  | Draw_results of int * int * int * int * int

type audio_cue =
  | Cue_success
  | Cue_false_positive
  | Cue_empty_check
  | Cue_warning
  | Cue_music of {
      freq_hz : float;
      duration_s : float;
      volume : float;
    }

type audio_cmd =
  | Play_cue of audio_cue

type frame = {
  draw : draw_cmd list;
  audio : audio_cmd list;
  score : int;
  flagged : int;
  message : string;
}

type world = {
  professor : professor;
  students : student array;
  mutable detection_radius : float;
  min_detection_radius : float;
  max_detection_radius : float;
  mutable energy : float;
  mutable focus_left : int;
  coffee_spots : vec2 array;
  mutable coffee_active : int option;
  mutable coffee_ttl : int;
  mutable coffee_respawn : int;
  mutable coffee_cursor : int;
  mutable drink_progress : float;
  mutable started : bool;
  mutable paused : bool;
  mutable difficulty : int;
  mutable cheat_window_factor : float;
  mutable hot_zone : int;
  mutable hot_zone_left : int;
  hot_zone_period : int;
  rounds_total : int;
  round_frames : int;
  mutable round_index : int;
  mutable round_left : int;
  mutable score : int;
  mutable flagged : int;
  mutable catches : int;
  mutable false_positives : int;
  mutable empty_checks : int;
  mutable combo : int;
  mutable combo_best : int;
  mutable combo_window_left : int;
  mutable game_over : bool;
  mutable message : string;
}
