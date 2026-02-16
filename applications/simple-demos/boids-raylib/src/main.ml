open Tempo
open Raylib

type vec = { x : float; y : float }
type boid = { pos : vec; vel : vec; color : Color.t }

type input_state = {
  pause_toggle : bool;
  reset : bool;
}

type frame = {
  boids : boid list;
  paused : bool;
}

let width = 960
let height = 640
let n_boids = 42
let max_speed = 2.6
let neighbor_radius = 56.0
let separation_radius = 10.0
let align_weight = 0.03
let coh_weight = 0.01
let sep_weight = 0.06
let jitter = 0.09

let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let scale s v = { x = s *. v.x; y = s *. v.y }

let dist2 a b =
  let dx = a.x -. b.x in
  let dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)

let clamp_speed v =
  let n = sqrt ((v.x *. v.x) +. (v.y *. v.y)) in
  if n <= max_speed then v else scale (max_speed /. n) v

let bounce pos vel =
  let px = pos.x +. vel.x in
  let py = pos.y +. vel.y in
  let max_x = float_of_int width in
  let max_y = float_of_int height in
  let vx, px' =
    if px < 0.0 then (-.vel.x, 0.0)
    else if px > max_x then (-.vel.x, max_x)
    else (vel.x, px)
  in
  let vy, py' =
    if py < 0.0 then (-.vel.y, 0.0)
    else if py > max_y then (-.vel.y, max_y)
    else (vel.y, py)
  in
  ({ x = px'; y = py' }, { x = vx; y = vy })

let random_color rng =
  Color.create
    (110 + Rng.int rng 146)
    (110 + Rng.int rng 146)
    (110 + Rng.int rng 146)
    255

let random_vec rng mag =
  let a = Rng.float rng (2.0 *. Float.pi) in
  { x = mag *. cos a; y = mag *. sin a }

let init_boids seed =
  let rng = Rng.create seed in
  List.init n_boids (fun _ ->
      {
        pos = { x = Rng.float rng (float_of_int width); y = Rng.float rng (float_of_int height) };
        vel = random_vec rng 2.0;
        color = random_color rng;
      })

let update_boids tick boids =
  List.map
    (fun state ->
      let neighbors =
        List.filter
          (fun b ->
            let d = dist2 b.pos state.pos in
            d > 0.0 && d < (neighbor_radius *. neighbor_radius))
          boids
      in
      let sep_force =
        List.fold_left
          (fun acc b ->
            let d = dist2 b.pos state.pos in
            if d < (separation_radius *. separation_radius) && d > 0.0 then
              add acc (scale (1.0 /. d) (sub state.pos b.pos))
            else acc)
          { x = 0.0; y = 0.0 }
          neighbors
      in
      let align_force =
        match neighbors with
        | [] -> { x = 0.0; y = 0.0 }
        | _ ->
            let sum = List.fold_left (fun acc b -> add acc b.vel) { x = 0.0; y = 0.0 } neighbors in
            scale (1.0 /. float_of_int (List.length neighbors)) sum
      in
      let coh_force =
        match neighbors with
        | [] -> { x = 0.0; y = 0.0 }
        | _ ->
            let sum = List.fold_left (fun acc b -> add acc b.pos) { x = 0.0; y = 0.0 } neighbors in
            let center = scale (1.0 /. float_of_int (List.length neighbors)) sum in
            sub center state.pos
      in
      let t = float_of_int tick *. 0.07 in
      let noise =
        {
          x = sin (state.pos.x *. 0.01 +. t) *. jitter;
          y = cos (state.pos.y *. 0.01 +. t) *. jitter;
        }
      in
      let vel' =
        state.vel
        |> add (scale sep_weight sep_force)
        |> add (scale align_weight align_force)
        |> add (scale coh_weight coh_force)
        |> add noise
        |> clamp_speed
      in
      let pos', vel'' = bounce state.pos vel' in
      { state with pos = pos'; vel = vel'' })
    boids

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      pause_toggle = is_key_pressed Key.Space;
      reset = is_key_pressed Key.R;
    }

let render (frame : frame) =
  begin_drawing ();
  clear_background (Color.create 18 23 33 255);
  List.iter
    (fun b ->
      draw_circle (int_of_float b.pos.x) (int_of_float b.pos.y) 5.0 b.color;
      draw_circle_lines (int_of_float b.pos.x) (int_of_float b.pos.y) 5.0 (Color.create 14 14 14 180))
    frame.boids;
  let panel =
    Tempo_game.Hud.panel
      ~rect:{ Tempo_game.Ui.x = 8.0; y = 8.0; w = 560.0; h = 54.0 }
      ~title:""
  in
  Tempo_game_raylib.Hud.draw_panel panel;
  Tempo_game_raylib.Hud.draw_badge ~x:20 ~y:20
    (Tempo_game.Hud.badge ~label:"Boids" ~value:(string_of_int (List.length frame.boids)));
  Tempo_game_raylib.Hud.draw_badge ~x:170 ~y:20
    (Tempo_game.Hud.badge
       ~label:"Etat"
       ~value:(if frame.paused then "PAUSED" else "RUNNING"));
  draw_text "SPACE pause/resume | R reset | ESC quit" 330 24 16 (Color.create 194 221 241 255);
  end_drawing ()

let main input output =
  let paused = ref false in
  let boids_state = ref (init_boids 4242) in
  let seed = ref 4242 in
  let tick = ref 0 in
  let rec loop () =
    let i = await input in
    if i.pause_toggle then paused := not !paused;
    if i.reset then (
      incr seed;
      boids_state := init_boids !seed);
    if not !paused then (
      boids_state := update_boids !tick !boids_state;
      incr tick);
    emit output { boids = !boids_state; paused = !paused };
    loop ()
  in
  loop ()

let () =
  init_window width height "Tempo Boids Raylib";
  set_target_fps 60;
  Tempo_game_raylib.Audio.init ();
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  Tempo_game_raylib.Audio.shutdown ();
  close_window ()
