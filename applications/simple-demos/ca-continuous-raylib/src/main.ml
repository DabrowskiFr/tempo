open Tempo
open Raylib

type input_state = {
  toggle_down : bool;
  reset : bool;
  next_preset : bool;
  prev_preset : bool;
}

type grid = {
  a : float array array;
  b : float array array;
}

type frame = {
  grid : grid;
  paused : bool;
  preset_idx : int;
  activity : float;
  calibrated_ms : float;
  calibrated_grid : int * int;
}

type preset = {
  name : string;
  feed : float;
  kill : float;
  dt : float;
  inject_every : int;
  palette : (int * int * int) array;
}

type grid_cfg = {
  mutable cells_x : int;
  mutable cells_y : int;
  mutable cell_w : int;
  mutable cell_h : int;
}

let width = 900
let height = 600
let target_fps = 30

let cfg = { cells_x = 90; cells_y = 60; cell_w = 10; cell_h = 10 }
let calibration_ms = ref 0.0
let calibration_grid = ref (cfg.cells_x, cfg.cells_y)

let set_grid_dims cx cy =
  cfg.cells_x <- cx;
  cfg.cells_y <- cy;
  cfg.cell_w <- max 1 (width / cx);
  cfg.cell_h <- max 1 (height / cy)

let clamp01 v = if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v
let clamp v a b = if v < a then a else if v > b then b else v
let lerp a b t = a +. ((b -. a) *. t)

let a_diff = 1.0
let b_diff = 0.5

let presets =
  [|
    {
      name = "Coral Bloom";
      feed = 0.0367;
      kill = 0.0649;
      dt = 1.2;
      inject_every = 150;
      palette =
        [|
          (6, 10, 20);
          (42, 64, 128);
          (66, 148, 186);
          (242, 158, 87);
          (249, 236, 186);
        |];
    };
    {
      name = "Neon Night";
      feed = 0.022;
      kill = 0.051;
      dt = 1.0;
      inject_every = 130;
      palette =
        [|
          (4, 5, 14);
          (39, 11, 74);
          (62, 33, 142);
          (56, 193, 255);
          (178, 255, 232);
        |];
    };
    {
      name = "Toxic Ink";
      feed = 0.03;
      kill = 0.059;
      dt = 1.15;
      inject_every = 170;
      palette =
        [|
          (8, 12, 6);
          (26, 55, 28);
          (83, 118, 33);
          (174, 203, 60);
          (252, 240, 148);
        |];
    };
  |]

let palette_color palette t =
  let n = Array.length palette in
  if n = 0 then (255, 255, 255)
  else if n = 1 then palette.(0)
  else
    let tf = clamp01 t *. float_of_int (n - 1) in
    let i = int_of_float tf in
    let j = min (n - 1) (i + 1) in
    let local_t = tf -. float_of_int i in
    let r0, g0, b0 = palette.(i) in
    let r1, g1, b1 = palette.(j) in
    ( int_of_float (lerp (float_of_int r0) (float_of_int r1) local_t),
      int_of_float (lerp (float_of_int g0) (float_of_int g1) local_t),
      int_of_float (lerp (float_of_int b0) (float_of_int b1) local_t) )

let shade_color (r, g, b) shade =
  let s = int_of_float shade in
  (clamp (r + s) 0 255, clamp (g + s) 0 255, clamp (b + s) 0 255)

let color_of_rgb ?(a = 255) (r, g, b) = Color.create r g b a

let grid_dims g = (Array.length g.a.(0), Array.length g.a)

let make_grid () =
  let cx = cfg.cells_x in
  let cy = cfg.cells_y in
  let a = Array.make_matrix cy cx 1.0 in
  let b = Array.make_matrix cy cx 0.0 in
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      let central = abs (x - (cx / 2)) < 8 && abs (y - (cy / 2)) < 8 in
      if central then b.(y).(x) <- 1.0
      else if Random.float 1.0 < 0.08 then b.(y).(x) <- 0.8
    done
  done;
  { a; b }

let copy_grid g =
  let _, cy = grid_dims g in
  {
    a = Array.init cy (fun y -> Array.copy g.a.(y));
    b = Array.init cy (fun y -> Array.copy g.b.(y));
  }

let sample arr x y =
  let cy = Array.length arr in
  let cx = Array.length arr.(0) in
  let nx = (x + cx) mod cx in
  let ny = (y + cy) mod cy in
  arr.(ny).(nx)

let laplacian arr x y =
  let w =
    [
      (0, 0, -1.0);
      (1, 0, 0.2);
      (-1, 0, 0.2);
      (0, 1, 0.2);
      (0, -1, 0.2);
      (1, 1, 0.05);
      (1, -1, 0.05);
      (-1, 1, 0.05);
      (-1, -1, 0.05);
    ]
  in
  List.fold_left (fun acc (dx, dy, ww) -> acc +. (ww *. sample arr (x + dx) (y + dy))) 0.0 w

let step_grid p g =
  let cx, cy = grid_dims g in
  let next = copy_grid g in
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      let a = g.a.(y).(x) in
      let b = g.b.(y).(x) in
      let la = laplacian g.a x y in
      let lb = laplacian g.b x y in
      let reaction = a *. b *. b in
      let a' = clamp01 (a +. ((a_diff *. la) -. reaction +. (p.feed *. (1.0 -. a))) *. p.dt) in
      let b' = clamp01 (b +. ((b_diff *. lb) +. reaction -. ((p.kill +. p.feed) *. b)) *. p.dt) in
      next.a.(y).(x) <- a';
      next.b.(y).(x) <- b'
    done
  done;
  next

let inject_spot g =
  let cx, cy = grid_dims g in
  let x0 = Random.int cx in
  let y0 = Random.int cy in
  for dy = -2 to 2 do
    for dx = -2 to 2 do
      let x = (x0 + dx + cx) mod cx in
      let y = (y0 + dy + cy) mod cy in
      g.b.(y).(x) <- 1.0
    done
  done

let activity_score g =
  let cx, cy = grid_dims g in
  let sum = ref 0.0 in
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      let b = g.b.(y).(x) in
      let left = sample g.b (x - 1) y in
      let right = sample g.b (x + 1) y in
      let up = sample g.b x (y - 1) in
      let down = sample g.b x (y + 1) in
      let gx = abs_float (right -. left) in
      let gy = abs_float (down -. up) in
      sum := !sum +. (0.65 *. b) +. (0.35 *. (gx +. gy))
    done
  done;
  !sum /. float_of_int (cx * cy)

let render (f : frame) =
  let preset = presets.(f.preset_idx) in
  let cx, cy = grid_dims f.grid in
  begin_drawing ();
  clear_background (Color.create 7 10 16 255);
  for y = 0 to (height / 4) do
    let t = float_of_int y /. float_of_int (height / 4) in
    let c = palette_color preset.palette (0.1 +. (0.8 *. t)) |> color_of_rgb ~a:30 in
    draw_rectangle 0 (y * 4) width 4 c
  done;
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      let b = f.grid.b.(y).(x) in
      let left = sample f.grid.b (x - 1) y in
      let right = sample f.grid.b (x + 1) y in
      let up = sample f.grid.b x (y - 1) in
      let down = sample f.grid.b x (y + 1) in
      let b_blur = ((4.0 *. b) +. left +. right +. up +. down) /. 8.0 in
      let gx = right -. left in
      let gy = down -. up in
      let edge = sqrt ((gx *. gx) +. (gy *. gy)) in
      let lum = clamp01 ((0.75 *. b_blur) +. (0.25 *. edge)) in
      (* Stable shading: edge magnitude only, avoids checkerboard flicker from signed gradients. *)
      let shade = ((edge -. 0.2) *. 18.0) in
      let col =
        palette_color preset.palette (0.06 +. (0.9 *. lum))
        |> fun c -> shade_color c shade
        |> color_of_rgb
      in
      let px = x * cfg.cell_w in
      let py = y * cfg.cell_h in
      draw_rectangle px py cfg.cell_w cfg.cell_h col;
      if b_blur > 0.76 then (
        let glow_alpha = int_of_float (clamp ((b_blur -. 0.76) *. 220.0) 0.0 70.0) in
        let glow = palette_color preset.palette (0.78 +. (0.22 *. b)) |> color_of_rgb ~a:glow_alpha in
        draw_rectangle px py cfg.cell_w cfg.cell_h glow)
    done
  done;
  draw_rectangle 10 10 690 86 (Color.create 6 16 24 175);
  draw_rectangle_lines 10 10 690 86 (Color.create 121 174 216 170);
  draw_text "Continuous Cellular Automata v2" 20 18 28 (Color.create 233 245 255 255);
  draw_text "SPACE pause | R reset | Q/E preset | ESC quit" 20 50 18 (Color.create 177 210 238 255);
  draw_text
    (Printf.sprintf "Grid: %dx%d (cell %dx%d)  |  Preset: %s  |  Activity: %.4f  |  State: %s"
       cx cy cfg.cell_w cfg.cell_h preset.name f.activity (if f.paused then "PAUSED" else "RUNNING"))
    20 72 18
    (if f.paused then Color.orange else Color.lime);
  let cgx, cgy = f.calibrated_grid in
  draw_text
    (Printf.sprintf "Calibrated for %d FPS | Mean frame %.2f ms | Chosen grid %dx%d"
       target_fps f.calibrated_ms cgx cgy)
    20 94 16 (Color.create 166 201 228 255);
  let cell_count = cx * cy in
  let fps = get_fps () in
  let bottom = Printf.sprintf "Cells: %d x %d = %d  |  FPS: %d" cx cy cell_count fps in
  let tw = measure_text bottom 18 in
  let tx = width - tw - 14 in
  let ty = height - 30 in
  draw_rectangle (tx - 10) (ty - 6) (tw + 20) 28 (Color.create 3 10 18 210);
  draw_rectangle_lines (tx - 10) (ty - 6) (tw + 20) 28 (Color.create 100 150 190 170);
  draw_text bottom tx ty 18 (Color.create 222 238 252 255);
  end_drawing ()

let bench_candidate ~frames cx cy =
  set_grid_dims cx cy;
  let g = ref (make_grid ()) in
  let p = presets.(0) in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to frames do
    g := step_grid p !g;
    render
      {
        grid = !g;
        paused = false;
        preset_idx = 0;
        activity = activity_score !g;
        calibrated_ms = 0.0;
        calibrated_grid = (cx, cy);
      }
  done;
  (Unix.gettimeofday () -. t0) /. float_of_int frames

let calibrate_grid_for_fps target =
  let budget = 1.0 /. float_of_int target in
  let candidates = [ (90, 60); (105, 70); (120, 80); (135, 90); (150, 100); (165, 110); (180, 120) ] in
  let best = ref (90, 60) in
  let best_ms = ref max_float in
  let best_area = ref (90 * 60) in
  List.iter
    (fun (cx, cy) ->
      if width mod cx = 0 && height mod cy = 0 then (
        let avg = bench_candidate ~frames:12 cx cy in
        let area = cx * cy in
        if avg <= budget *. 0.96 && area >= !best_area then (
          best := (cx, cy);
          best_area := area;
          best_ms := avg)))
    candidates;
  let bx, by = !best in
  set_grid_dims bx by;
  if !best_ms = max_float then best_ms := bench_candidate ~frames:8 bx by;
  calibration_ms := !best_ms *. 1000.0;
  calibration_grid := (bx, by);
  bx, by

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      toggle_down = is_key_down Key.Space;
      reset = is_key_pressed Key.R;
      next_preset = is_key_pressed Key.E;
      prev_preset = is_key_pressed Key.Q;
    }

let main input output =
  let stop = new_signal () in
  let toggle_edge = new_signal () in
  let paused = new_state false in
  let grid_state = new_state (make_grid ()) in
  let preset_idx = new_state 0 in
  let stale_frames = new_state 0 in
  let feed_bias = new_state 0.0 in
  let kill_bias = new_state 0.0 in
  let phase = new_state 0.0 in
  let inject_counter = new_state 0 in

  let edge_proc () = Reactive.rising_edge (fun i -> i.toggle_down) input toggle_edge in

  let input_proc () =
    let rec loop () =
      let i = await input in
      if i.reset then set_state grid_state (make_grid ());
      if i.next_preset then modify_state preset_idx (fun idx -> (idx + 1) mod Array.length presets);
      if i.prev_preset then
        modify_state preset_idx (fun idx -> (idx + Array.length presets - 1) mod Array.length presets);
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

  let sim_proc () =
    let rec loop () =
      let activity = ref (activity_score (get_state grid_state)) in
      if not (get_state paused) then (
        let p0 = presets.(get_state preset_idx) in
        let a0 = !activity in
        let lo = 0.03 in
        let hi = 0.09 in
        let fb, kb =
          if a0 < lo then (get_state feed_bias +. 0.00008, get_state kill_bias -. 0.00005)
          else if a0 > hi then (get_state feed_bias -. 0.00006, get_state kill_bias +. 0.00004)
          else (get_state feed_bias *. 0.985, get_state kill_bias *. 0.985)
        in
        set_state feed_bias (clamp fb (-0.008) 0.012);
        set_state kill_bias (clamp kb (-0.01) 0.01);
        let ph = get_state phase +. 0.015 in
        set_state phase ph;
        let wave = sin ph *. 0.0015 in
        let p =
          {
            p0 with
            feed = clamp (p0.feed +. get_state feed_bias +. wave) 0.005 0.09;
            kill = clamp (p0.kill +. get_state kill_bias -. (0.5 *. wave)) 0.02 0.09;
          }
        in
        let g = step_grid p (get_state grid_state) |> copy_grid in
        activity := activity_score g;
        let ic = get_state inject_counter + 1 in
        set_state inject_counter ic;
        let timed_reseed = ic >= p0.inject_every in
        let stale =
          if !activity < 0.018 then get_state stale_frames + 1
          else max 0 (get_state stale_frames - 4)
        in
        if stale > 60 || timed_reseed then (
          let bursts =
            if stale > 100 then 4 else if stale > 70 then 3 else if timed_reseed then 2 else 1
          in
          for _ = 1 to bursts do
            inject_spot g
          done;
          set_state stale_frames 18;
          set_state inject_counter 0;
          activity := activity_score g)
        else set_state stale_frames stale;
        set_state grid_state g);
      emit output
        {
          grid = get_state grid_state;
          paused = get_state paused;
          preset_idx = get_state preset_idx;
          activity = !activity;
          calibrated_ms = !calibration_ms;
          calibrated_grid = !calibration_grid;
        };
      pause ();
      loop ()
    in
    loop ()
  in

  Reactive.supervise_until stop [ input_proc; edge_proc; toggle_proc; sim_proc ]

let () =
  Random.self_init ();
  init_window width height "Tempo Continuous CA Raylib";
  set_target_fps target_fps;
  ignore (calibrate_grid_for_fps target_fps);
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
