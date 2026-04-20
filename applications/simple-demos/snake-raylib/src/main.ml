open Tempo
open Raylib

type dir = Up | Down | Left | Right

type input_state = {
  turn : dir option;
  toggle_pause : bool;
  restart : bool;
}

type model = {
  snake : (int * int) list;
  dir : dir;
  food : int * int;
  alive : bool;
  paused : bool;
  running : bool;
  grow : int;
  score : int;
}

let cell = 20
let cols = 44
let rows = 30
let width = cols * cell
let height = rows * cell
let restart_rect =
  { Tempo_game_raylib.Ui.x = float_of_int (width - 150); y = 14.0; w = 126.0; h = 34.0 }
let last_score = ref 0
let last_alive = ref true

let initial_model () =
  {
    snake = [ (cols / 2, rows / 2) ];
    dir = Right;
    food = (4, 4);
    alive = true;
    paused = false;
    running = true;
    grow = 0;
    score = 0;
  }

let opposite a b =
  match (a, b) with
  | Up, Down | Down, Up | Left, Right | Right, Left -> true
  | _ -> false

let rec drop_last = function
  | [] -> []
  | [ _ ] -> []
  | x :: xs -> x :: drop_last xs

let spawn_food snake =
  let rec pick () =
    let p = (Random.int cols, Random.int rows) in
    if List.mem p snake then pick () else p
  in
  pick ()

let step_snake m =
  let hx, hy = List.hd m.snake in
  let nx, ny =
    match m.dir with
    | Up -> (hx, (hy - 1 + rows) mod rows)
    | Down -> (hx, (hy + 1) mod rows)
    | Left -> ((hx - 1 + cols) mod cols, hy)
    | Right -> ((hx + 1) mod cols, hy)
  in
  let eats = (nx, ny) = m.food in
  let grow' = if eats then m.grow + 1 else m.grow in
  let next_snake = if grow' > 0 then (nx, ny) :: m.snake else (nx, ny) :: drop_last m.snake in
  let tail = List.tl next_snake in
  let collide = List.exists (fun p -> p = (nx, ny)) tail in
  {
    m with
    snake = next_snake;
    grow = max 0 (grow' - 1);
    food = (if eats then spawn_food next_snake else m.food);
    alive = not collide;
    score = (if eats then m.score + 1 else m.score);
  }

let apply_input (m : model) (i : input_state) : model =
  if i.restart then
    let m' = initial_model () in
    { m' with food = spawn_food m'.snake }
  else
    let dir =
      match i.turn with
      | Some d when not (opposite m.dir d) -> d
      | _ -> m.dir
    in
    let paused = if i.toggle_pause then not m.paused else m.paused in
    { m with dir; paused }

let tick_model (m : model) : model =
  if (not m.running) || (not m.alive) || m.paused then m else step_snake m

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  let click_restart =
    if is_mouse_button_pressed MouseButton.Left then
      let i = Tempo_game_raylib.Ui.interaction_from_mouse () in
      Tempo_game_raylib.Ui.contains restart_rect i.pointer
    else false
  in
  let turn =
    if is_key_pressed Key.Up then Some Up
    else if is_key_pressed Key.Down then Some Down
    else if is_key_pressed Key.Left then Some Left
    else if is_key_pressed Key.Right then Some Right
    else None
  in
  Some
    {
      turn;
      toggle_pause = is_key_pressed Key.P || is_key_pressed Key.Space;
      restart = is_key_pressed Key.R || click_restart;
    }

let draw_model (m : model) =
  if m.score > !last_score then
    Tempo_game_raylib.Audio.play_cue
      (Tempo_game_raylib.Audio.cue ~freq_hz:740.0 ~duration_s:0.08 ~volume:0.3);
  if !last_alive && not m.alive then
    Tempo_game_raylib.Audio.play_cue
      (Tempo_game_raylib.Audio.cue ~freq_hz:190.0 ~duration_s:0.2 ~volume:0.35);
  last_score := m.score;
  last_alive := m.alive;
  begin_drawing ();
  clear_background (Color.create 18 22 24 255);
  let panel =
    Tempo_game_raylib.Hud.panel
      ~rect:{ Tempo_game_raylib.Ui.x = 8.0; y = 6.0; w = float_of_int (width - 16); h = 56.0 }
      ~title:""
  in
  Tempo_game_raylib.Hud.draw_panel panel;
  Tempo_game_raylib.Hud.draw_badge ~x:20 ~y:16
    (Tempo_game_raylib.Hud.badge ~label:"Score" ~value:(string_of_int m.score));
  Tempo_game_raylib.Hud.draw_badge ~x:170 ~y:16
    (Tempo_game_raylib.Hud.badge
       ~label:"Etat"
       ~value:(if m.paused then "PAUSE" else if m.alive then "RUNNING" else "GAME OVER"));
  draw_text "Arrows move | P/SPACE pause | ESC quit" 20 38 16 (Color.create 194 221 241 255);
  let restart_btn = Tempo_game_raylib.Ui.button ~id:"restart" restart_rect ~label:"Restart (R)" () in
  Tempo_game_raylib.Ui.draw_button ~active:true restart_btn;
  List.iter
    (fun (x, y) ->
      draw_rectangle (x * cell) (y * cell + 70) (cell - 1) (cell - 1) (Color.create 52 205 109 255))
    m.snake;
  let fx, fy = m.food in
  draw_rectangle (fx * cell) (fy * cell + 70) (cell - 1) (cell - 1) (Color.create 232 107 66 255);
  if not m.alive then draw_text "Press R to restart" (width / 2 - 95) (height / 2 + 15) 24 Color.raywhite;
  end_drawing ()

let scenario input_signal output_signal =
  let m0 = initial_model () in
  let model = ref { m0 with food = spawn_food m0.snake } in
  emit output_signal !model;
  let rec loop () =
    let i = await input_signal in
    model := apply_input !model i;
    model := tick_model !model;
    emit output_signal !model;
    loop ()
  in
  loop ()

let () =
  Random.self_init ();
  init_window width (height + 70) "Tempo Snake Raylib";
  set_target_fps 60;
  Tempo_game_raylib.Audio.init ();
  (try execute ~instants:2_000_000 ~input:poll_input ~output:draw_model scenario with Exit -> ());
  Tempo_game_raylib.Audio.shutdown ();
  close_window ()
