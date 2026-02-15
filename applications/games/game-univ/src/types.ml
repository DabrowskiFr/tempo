type vec2 = { mutable x : float; mutable y : float }

type input_state = {
  up : bool;
  down : bool;
  left : bool;
  right : bool;
  ask : bool;
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

type student = {
  id : int;
  pos : vec2;
  mutable cheating : bool;
}

type event =
  | Cheating_start of int
  | Cheating_stop of int
  | Student_flagged of int
  | Ask_success of int
  | Ask_miss of int option
  | Ask_feedback of vec2 * bool * string

type draw_cmd =
  | Draw_room
  | Draw_professor of vec2 * float * float
  | Draw_student of int * vec2 * float * bool * float
  | Draw_action_fx of vec2 * bool * string * int
  | Draw_hud of int * int * string * float * float * float

type frame = {
  draw : draw_cmd list;
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
  mutable score : int;
  mutable flagged : int;
  mutable message : string;
}
