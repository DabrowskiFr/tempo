open Tempo
open Raylib

type input_state = {
  toggle_down : bool;
  reset : bool;
}

type grid = {
  a : float array array;
  b : float array array;
}

type frame = {
  grid : grid;
  paused : bool;
}

let width = 900
let height = 600
let cells_x = 90
let cells_y = 60
let cell_w = width / cells_x
let cell_h = height / cells_y

let a_diff = 1.0
let b_diff = 0.5
let feed = 0.0367
let kill = 0.0649
let dt = 1.2

let clamp01 v = if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v

let make_grid () =
  let a = Array.make_matrix cells_y cells_x 1.0 in
  let b = Array.make_matrix cells_y cells_x 0.0 in
  for y = 0 to cells_y - 1 do
    for x = 0 to cells_x - 1 do
      let central = abs (x - (cells_x / 2)) < 8 && abs (y - (cells_y / 2)) < 8 in
      if central then b.(y).(x) <- 1.0
      else if Random.float 1.0 < 0.08 then b.(y).(x) <- 0.8
    done
  done;
  { a; b }

let copy_grid g =
  { a = Array.init cells_y (fun y -> Array.copy g.a.(y)); b = Array.init cells_y (fun y -> Array.copy g.b.(y)) }

let sample arr x y =
  let nx = (x + cells_x) mod cells_x in
  let ny = (y + cells_y) mod cells_y in
  arr.(ny).(nx)

let laplacian arr x y =
  let w =
    [ (0, 0, -1.0); (1, 0, 0.2); (-1, 0, 0.2); (0, 1, 0.2); (0, -1, 0.2);
      (1, 1, 0.05); (1, -1, 0.05); (-1, 1, 0.05); (-1, -1, 0.05) ]
  in
  List.fold_left (fun acc (dx, dy, ww) -> acc +. (ww *. sample arr (x + dx) (y + dy))) 0.0 w

let step_grid g =
  let next = copy_grid g in
  for y = 0 to cells_y - 1 do
    for x = 0 to cells_x - 1 do
      let a = g.a.(y).(x) in
      let b = g.b.(y).(x) in
      let la = laplacian g.a x y in
      let lb = laplacian g.b x y in
      let reaction = a *. b *. b in
      let a' = clamp01 (a +. ((a_diff *. la) -. reaction +. (feed *. (1.0 -. a))) *. dt) in
      let b' = clamp01 (b +. ((b_diff *. lb) +. reaction -. ((kill +. feed) *. b)) *. dt) in
      next.a.(y).(x) <- a';
      next.b.(y).(x) <- b'
    done
  done;
  next

let inject_spot g =
  let x0 = Random.int cells_x in
  let y0 = Random.int cells_y in
  for dy = -2 to 2 do
    for dx = -2 to 2 do
      let x = (x0 + dx + cells_x) mod cells_x in
      let y = (y0 + dy + cells_y) mod cells_y in
      g.b.(y).(x) <- 1.0
    done
  done

let color_of_b b =
  let v = int_of_float (255.0 *. clamp01 b) in
  Color.create v v v 255

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      toggle_down = is_key_down Key.Space;
      reset = is_key_pressed Key.R;
    }

let render (f : frame) =
  begin_drawing ();
  clear_background (Color.create 12 15 18 255);
  for y = 0 to cells_y - 1 do
    for x = 0 to cells_x - 1 do
      draw_rectangle (x * cell_w) (y * cell_h) cell_w cell_h (color_of_b f.grid.b.(y).(x))
    done
  done;
  draw_text "Continuous CA Raylib - SPACE pause - R reset - ESC quit" 16 12 22 Color.raywhite;
  draw_text (if f.paused then "PAUSED" else "RUNNING") 16 42 20 (if f.paused then Color.orange else Color.lime);
  end_drawing ()

let main input output =
  let stop = new_signal () in
  let toggle_edge = new_signal () in
  let paused = new_state false in
  let grid_state = new_state (make_grid ()) in

  let input_proc () =
    let prev_down = ref false in
    let rec loop () =
      let i = await input in
      if i.toggle_down && not !prev_down then emit toggle_edge ();
      prev_down := i.toggle_down;
      if i.reset then set_state grid_state (make_grid ());
      loop ()
    in
    loop ()
  in

  let toggle_proc () =
    let rec loop () =
      let _ = await toggle_edge in
      modify_state paused not;
      loop ()
    in
    loop ()
  in

  let periodic_injection () =
    Game.every_n 180 (fun () ->
        if not (get_state paused) then (
          let g = copy_grid (get_state grid_state) in
          inject_spot g;
          set_state grid_state g))
  in

  let sim_proc () =
    let rec loop () =
      if not (get_state paused) then modify_state grid_state step_grid;
      emit output { grid = get_state grid_state; paused = get_state paused };
      pause ();
      loop ()
    in
    loop ()
  in

  parallel [ (fun () -> watch stop (fun () -> parallel [ input_proc; toggle_proc; periodic_injection; sim_proc ])) ]

let () =
  Random.self_init ();
  init_window width height "Tempo Continuous CA Raylib";
  set_target_fps 60;
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
