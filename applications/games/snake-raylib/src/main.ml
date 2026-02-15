open Tempo
open Raylib

type dir = Up | Down | Left | Right

type msg =
  | Boot
  | Tick
  | Turn of dir
  | TogglePause
  | Restart

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

let update (m : model) (msg : msg) : model * msg App.command =
  match msg with
  | Boot -> (m, App.after_n 1 Tick)
  | Restart ->
      let m' = initial_model () in
      ({ m' with food = spawn_food m'.snake }, App.after_n 1 Tick)
  | TogglePause -> ({ m with paused = not m.paused }, App.none)
  | Turn d -> if opposite m.dir d then (m, App.none) else ({ m with dir = d }, App.none)
  | Tick ->
      if (not m.running) || (not m.alive) then (m, App.none)
      else if m.paused then (m, App.after_n 1 Tick)
      else (step_snake m, App.after_n 1 Tick)

let input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  if is_key_pressed Key.R then Some Restart
  else if is_key_pressed Key.P || is_key_pressed Key.Space then Some TogglePause
  else if is_key_pressed Key.Up then Some (Turn Up)
  else if is_key_pressed Key.Down then Some (Turn Down)
  else if is_key_pressed Key.Left then Some (Turn Left)
  else if is_key_pressed Key.Right then Some (Turn Right)
  else None

let draw_model (m : model) =
  begin_drawing ();
  clear_background (Color.create 18 22 24 255);
  draw_text "Snake Raylib (Tempo.App) - arrows move, P/SPACE pause, R restart, ESC quit" 14 10 20 Color.raywhite;
  draw_text (Printf.sprintf "Score: %d" m.score) 14 38 20 Color.gold;
  draw_text (if m.paused then "PAUSED" else if m.alive then "RUNNING" else "GAME OVER") 170 38 20
    (if m.alive then (if m.paused then Color.orange else Color.lime) else Color.red);
  List.iter
    (fun (x, y) ->
      draw_rectangle (x * cell) (y * cell + 70) (cell - 1) (cell - 1) (Color.create 52 205 109 255))
    m.snake;
  let fx, fy = m.food in
  draw_rectangle (fx * cell) (fy * cell + 70) (cell - 1) (cell - 1) (Color.create 232 107 66 255);
  if not m.alive then draw_text "Press R to restart" (width / 2 - 95) (height / 2 + 15) 24 Color.raywhite;
  end_drawing ()

let () =
  Random.self_init ();
  init_window width (height + 70) "Tempo Snake Raylib";
  set_target_fps 60;
  let boot_sent = ref false in
  let input_with_boot () =
    if not !boot_sent then (
      boot_sent := true;
      Some Boot)
    else input ()
  in
  let program : (model, msg) App.program =
    { init = initial_model (); update }
  in
  (try
     App.run_with_view ~instants:2_000_000 ~input:input_with_boot ~view:Fun.id ~output:draw_model program
   with Exit -> ());
  close_window ()
