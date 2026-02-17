open Tempo
open Raylib

type input_state = {
  toggle_down : bool;
  reset : bool;
  next_preset : bool;
  prev_preset : bool;
  switch_mode : bool;
}

type sim_mode =
  | Matrix
  | Processes

type kernel = { offsets : (int * int * float) array; norm : float }

type preset = {
  name : string;
  radius : float;
  ring_center : float;
  ring_width : float;
  mu : float;
  sigma : float;
  dt : float;
  reseed_period : int;
  palette : (int * int * int) array;
}

type field = float array array

type frame = {
  grid : field;
  paused : bool;
  preset_idx : int;
  mode : sim_mode;
  activity : float;
  diversity : float;
  matrix_ms : float;
  process_ms : float;
  proc_level : int;
}

let width = 960
let height = 640
let target_fps = 30
let budget_ms = 1000.0 /. float_of_int target_fps

let process_candidates = [| (40, 26); (48, 32); (56, 36); (64, 42) |]
let proc_max_x, proc_max_y = process_candidates.(Array.length process_candidates - 1)

let clamp01 v = if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v
let clamp v a b = if v < a then a else if v > b then b else v
let lerp a b t = a +. ((b -. a) *. t)
let now_ms () = Unix.gettimeofday () *. 1000.0
let dims g = (Array.length g.(0), Array.length g)

let ema_update old sample =
  if old <= 0.0 then sample else (0.92 *. old) +. (0.08 *. sample)

let mode_name = function
  | Matrix -> "Matriciel"
  | Processes -> "Processus/cellule"

let presets =
  [|
    {
      name = "Pearl Drift";
      radius = 8.0;
      ring_center = 0.54;
      ring_width = 0.15;
      mu = 0.148;
      sigma = 0.016;
      dt = 0.13;
      reseed_period = 190;
      palette =
        [|
          (9, 12, 22);
          (27, 61, 119);
          (72, 160, 195);
          (199, 223, 178);
          (252, 242, 188);
        |];
    };
    {
      name = "Amber Bloom";
      radius = 7.0;
      ring_center = 0.50;
      ring_width = 0.16;
      mu = 0.145;
      sigma = 0.018;
      dt = 0.12;
      reseed_period = 170;
      palette =
        [|
          (11, 8, 7);
          (63, 25, 20);
          (138, 64, 38);
          (219, 137, 69);
          (255, 223, 166);
        |];
    };
    {
      name = "Aqua Aurora";
      radius = 9.0;
      ring_center = 0.58;
      ring_width = 0.13;
      mu = 0.154;
      sigma = 0.014;
      dt = 0.11;
      reseed_period = 210;
      palette =
        [|
          (7, 15, 26);
          (16, 55, 90);
          (25, 116, 141);
          (112, 202, 190);
          (220, 246, 228);
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

let color_of_rgb ?(a = 255) (r, g, b) = Color.create r g b a

let make_field cx cy =
  let g = Array.make_matrix cy cx 0.0 in
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      if Random.float 1.0 < 0.08 then g.(y).(x) <- Random.float 0.85
    done
  done;
  g

let resample_field g cx2 cy2 =
  let cx1, cy1 = dims g in
  let out = Array.make_matrix cy2 cx2 0.0 in
  for y = 0 to cy2 - 1 do
    for x = 0 to cx2 - 1 do
      let sx = int_of_float ((float_of_int x /. float_of_int cx2) *. float_of_int cx1) in
      let sy = int_of_float ((float_of_int y /. float_of_int cy2) *. float_of_int cy1) in
      out.(y).(x) <- g.(min (cy1 - 1) sy).(min (cx1 - 1) sx)
    done
  done;
  out

let sample g x y =
  let cx, cy = dims g in
  let nx = (x + cx) mod cx in
  let ny = (y + cy) mod cy in
  g.(ny).(nx)

let gaussian x sigma = exp (-.((x *. x) /. (2.0 *. sigma *. sigma)))
let grow u mu sigma = (2.0 *. gaussian (u -. mu) sigma) -. 1.0

let make_kernel p =
  let r = p.radius in
  let rr = int_of_float (ceil r) in
  let acc = ref [] in
  let sum = ref 0.0 in
  for dy = -rr to rr do
    for dx = -rr to rr do
      let d = sqrt ((float_of_int (dx * dx)) +. float_of_int (dy * dy)) /. r in
      if d <= 1.0 then (
        let w = gaussian (d -. p.ring_center) p.ring_width in
        if w > 1e-6 then (
          acc := (dx, dy, w) :: !acc;
          sum := !sum +. w))
    done
  done;
  { offsets = Array.of_list !acc; norm = max !sum 1e-6 }

let convolve g k x y =
  let s = ref 0.0 in
  Array.iter
    (fun (dx, dy, w) ->
      s := !s +. (w *. sample g (x + dx) (y + dy)))
    k.offsets;
  !s /. k.norm

let inject_blob g =
  let cx, cy = dims g in
  let x0 = Random.int cx in
  let y0 = Random.int cy in
  for dy = -4 to 4 do
    for dx = -4 to 4 do
      let x = (x0 + dx + cx) mod cx in
      let y = (y0 + dy + cy) mod cy in
      let d = sqrt ((float_of_int (dx * dx)) +. float_of_int (dy * dy)) /. 4.0 in
      if d <= 1.0 then g.(y).(x) <- clamp01 (g.(y).(x) +. (0.95 *. (1.0 -. d)))
    done
  done

let stats g =
  let cx, cy = dims g in
  let sum = ref 0.0 in
  let sum2 = ref 0.0 in
  let grad = ref 0.0 in
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      let v = g.(y).(x) in
      let r = sample g (x + 1) y in
      let d = sample g x (y + 1) in
      sum := !sum +. v;
      sum2 := !sum2 +. (v *. v);
      grad := !grad +. abs_float (r -. v) +. abs_float (d -. v)
    done
  done;
  let n = float_of_int (cx * cy) in
  let mean = !sum /. n in
  let var = max 0.0 ((!sum2 /. n) -. (mean *. mean)) in
  let activity = !grad /. (2.0 *. n) in
  (activity, sqrt var)

let cell_next p k g phase x y =
  let mu_drift = p.mu +. (0.006 *. sin phase) in
  let sigma_drift = clamp (p.sigma +. (0.003 *. cos (phase *. 0.7))) 0.009 0.032 in
  let u = convolve g k x y in
  let dv = p.dt *. grow u mu_drift sigma_drift in
  clamp01 (g.(y).(x) +. dv)

let step_field p k g phase =
  let cx, cy = dims g in
  let next = Array.make_matrix cy cx 0.0 in
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      next.(y).(x) <- cell_next p k g phase x y
    done
  done;
  next

let apply_updates g updates =
  let cx, cy = dims g in
  let next = Array.init cy (fun y -> Array.copy g.(y)) in
  List.iter
    (fun (x, y, v) ->
      if x >= 0 && x < cx && y >= 0 && y < cy then next.(y).(x) <- v)
    updates;
  next

let render (f : frame) =
  let p = presets.(f.preset_idx) in
  let cx, cy = dims f.grid in
  let cw = max 1 (width / cx) in
  let ch = max 1 (height / cy) in
  begin_drawing ();
  clear_background (Color.create 7 10 16 255);
  for y = 0 to (height / 4) do
    let t = float_of_int y /. float_of_int (height / 4) in
    let c = palette_color p.palette (0.08 +. (0.85 *. t)) |> color_of_rgb ~a:28 in
    draw_rectangle 0 (y * 4) width 4 c
  done;
  for y = 0 to cy - 1 do
    for x = 0 to cx - 1 do
      let v = f.grid.(y).(x) in
      let lum = clamp01 (0.08 +. (0.96 *. v)) in
      let col = palette_color p.palette lum |> color_of_rgb in
      let px = x * cw in
      let py = y * ch in
      draw_rectangle px py cw ch col;
      if v > 0.78 then
        let a = int_of_float (clamp ((v -. 0.78) *. 230.0) 0.0 70.0) in
        draw_rectangle px py cw ch (palette_color p.palette 0.95 |> color_of_rgb ~a)
    done
  done;
  draw_rectangle 12 12 900 104 (Color.create 6 16 24 186);
  draw_rectangle_lines 12 12 900 104 (Color.create 121 174 216 170);
  draw_text "Lenia (continuous life-like field)" 24 20 26 (Color.create 230 245 255 255);
  draw_text "SPACE pause | TAB/M switch mode | R reset | Q/E preset | ESC quit" 24 50 18
    (Color.create 176 212 238 255);
  draw_text
    (Printf.sprintf "Preset: %s | Mode: %s | Activity: %.4f | Diversity: %.4f | Grid: %dx%d | %s"
       p.name (mode_name f.mode) f.activity f.diversity cx cy (if f.paused then "PAUSED" else "RUNNING"))
    24 72 17
    (if f.paused then Color.orange else Color.lime);
  let ratio_text =
    if f.matrix_ms > 0.0 && f.process_ms > 0.0 then
      Printf.sprintf "ratio process/matrix %.2fx" (f.process_ms /. f.matrix_ms)
    else "ratio process/matrix n/a"
  in
  draw_text
    (Printf.sprintf "Step avg: matrix %.2f ms | process %.2f ms | %s | lvl %d/%d"
       f.matrix_ms f.process_ms ratio_text (f.proc_level + 1) (Array.length process_candidates))
    24 92 16 (Color.create 166 201 228 255);
  let cell_count = cx * cy in
  let line = Printf.sprintf "Cells: %d x %d = %d  |  FPS: %d" cx cy cell_count (get_fps ()) in
  let tw = measure_text line 18 in
  let tx = width - tw - 14 in
  let ty = height - 30 in
  draw_rectangle (tx - 10) (ty - 6) (tw + 20) 28 (Color.create 3 10 18 210);
  draw_rectangle_lines (tx - 10) (ty - 6) (tw + 20) 28 (Color.create 100 150 190 170);
  draw_text line tx ty 18 (Color.create 220 238 252 255);
  end_drawing ()

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      toggle_down = is_key_down Key.Space;
      reset = is_key_pressed Key.R;
      next_preset = is_key_pressed Key.E;
      prev_preset = is_key_pressed Key.Q;
      switch_mode = is_key_pressed Key.Tab || is_key_pressed Key.M;
    }

let main input output =
  let stop = new_signal () in
  let toggle_edge = new_signal () in
  let process_tick = new_signal () in
  let process_updates = new_signal_agg ~initial:[] ~combine:(fun acc v -> v :: acc) in
  let paused = State.create false in
  let preset_idx = State.create 0 in
  let mode_state = State.create Matrix in
  let kernel_state = State.create (make_kernel presets.(0)) in
  let proc_level = State.create 2 in
  let start_x, start_y = process_candidates.(State.get proc_level) in
  let grid_state = State.create (make_field start_x start_y) in
  let frame_count = State.create 0 in
  let phase = State.create 0.0 in
  let matrix_ms = State.create 0.0 in
  let process_ms = State.create 0.0 in
  let proc_hi = State.create 0 in
  let proc_lo = State.create 0 in

  let resize_to_level lvl =
    let nx, ny = process_candidates.(lvl) in
    State.set grid_state (resample_field (State.get grid_state) nx ny)
  in

  let edge_proc () = Reactive.rising_edge (fun i -> i.toggle_down) input toggle_edge in

  let input_proc () =
    let rec loop () =
      let i = await input in
      if i.reset then (
        let gx, gy = process_candidates.(State.get proc_level) in
        State.set grid_state (make_field gx gy));
      if i.next_preset then (
        State.modify preset_idx (fun idx -> (idx + 1) mod Array.length presets);
        State.set kernel_state (make_kernel presets.(State.get preset_idx)));
      if i.prev_preset then (
        State.modify preset_idx (fun idx -> (idx + Array.length presets - 1) mod Array.length presets);
        State.set kernel_state (make_kernel presets.(State.get preset_idx)));
      if i.switch_mode then
        State.modify mode_state (function Matrix -> Processes | Processes -> Matrix);
      loop ()
    in
    loop ()
  in

  let toggle_proc () =
    let rec loop () =
      let _ = await toggle_edge in
      State.modify paused not;
      loop ()
    in
    loop ()
  in

  let cell_process x y () =
    let rec loop () =
      let _ = await process_tick in
      let g = State.get grid_state in
      let gx, gy = dims g in
      if x < gx && y < gy then (
        let p = presets.(State.get preset_idx) in
        let k = State.get kernel_state in
        let ph = State.get phase in
        let v = cell_next p k g ph x y in
        emit process_updates (x, y, v));
      loop ()
    in
    loop ()
  in

  let all_cell_processes =
    let procs = ref [] in
    for y = 0 to proc_max_y - 1 do
      for x = 0 to proc_max_x - 1 do
        procs := cell_process x y :: !procs
      done
    done;
    !procs
  in

  let sim_proc () =
    let rec loop () =
      let p = presets.(State.get preset_idx) in
      let ph = State.get phase +. 0.02 in
      State.set phase ph;
      if not (State.get paused) then (
        let g = State.get grid_state in
        let t0 = now_ms () in
        let next =
          match State.get mode_state with
          | Matrix ->
              let g' = step_field p (State.get kernel_state) g ph in
              State.set matrix_ms (ema_update (State.get matrix_ms) (now_ms () -. t0));
              g'
          | Processes ->
              emit process_tick ();
              let g' = apply_updates g (await process_updates) in
              let dt = now_ms () -. t0 in
              State.set process_ms (ema_update (State.get process_ms) dt);
              let fps = get_fps () in
              let too_slow = dt > (budget_ms *. 1.08) || fps < (target_fps - 2) in
              let very_fast = dt < (budget_ms *. 0.68) && fps >= target_fps in
              if too_slow then (
                State.set proc_hi (State.get proc_hi + 1);
                State.set proc_lo 0)
              else if very_fast then (
                State.set proc_lo (State.get proc_lo + 1);
                State.set proc_hi 0)
              else (
                State.set proc_hi 0;
                State.set proc_lo 0);
              if State.get proc_hi >= 12 && State.get proc_level > 0 then (
                let lvl = State.get proc_level - 1 in
                State.set proc_level lvl;
                State.set proc_hi 0;
                State.set proc_lo 0;
                resize_to_level lvl)
              else if State.get proc_lo >= 80 && State.get proc_level < Array.length process_candidates - 1 then (
                let lvl = State.get proc_level + 1 in
                State.set proc_level lvl;
                State.set proc_hi 0;
                State.set proc_lo 0;
                resize_to_level lvl);
              g'
        in
        let fc = State.get frame_count + 1 in
        State.set frame_count fc;
        if fc mod p.reseed_period = 0 then inject_blob next;
        let activity, diversity = stats next in
        if activity < 0.009 || diversity < 0.035 then (
          for _ = 1 to 3 do
            inject_blob next
          done);
        State.set grid_state next);
      let activity, diversity = stats (State.get grid_state) in
      emit output
        {
          grid = State.get grid_state;
          paused = State.get paused;
          preset_idx = State.get preset_idx;
          mode = State.get mode_state;
          activity;
          diversity;
          matrix_ms = State.get matrix_ms;
          process_ms = State.get process_ms;
          proc_level = State.get proc_level;
        };
      pause ();
      loop ()
    in
    loop ()
  in

  Reactive.supervise_until stop ([ input_proc; edge_proc; toggle_proc; sim_proc ] @ all_cell_processes)

let () =
  Random.self_init ();
  init_window width height "Tempo Lenia Raylib";
  set_target_fps target_fps;
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
