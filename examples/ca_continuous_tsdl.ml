open Tempo
open Tsdl
open Bigarray

(* Continuous cellular automaton (Gray-Scott-like) rendered with SDL.
   One behavior per cell; communication only via signals. *)

type cell_state = { x : int; y : int; a : float; b : float; phase : float }
type frame = { a_grid : float array array; b_grid : float array array }

let width = 960
let height = 640
let cells_x = 160
let cells_y = 106
let cells_count = cells_x * cells_y
let density_scale = float_of_int cells_x /. 96.

(* Parameter set aimed at sustained, rich dynamics. *)
let a_diff = 1.0
let b_diff = 0.48
let base_feed = 0.029
let base_kill = 0.055
let dt = 0.85
let tau = 6.283185307179586

let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v
let clamp lo hi v = if v < lo then lo else if v > hi then hi else v
let pixel_pitch = cells_x * 4
let pixel_bytes = cells_x * cells_y * 4

let sample frame x y = frame.a_grid.(y).(x), frame.b_grid.(y).(x)

let laplacian frame x y =
  let weights =
    [ (0, 0, -1.0)
    ; (1, 0, 0.2)
    ; (-1, 0, 0.2)
    ; (0, 1, 0.2)
    ; (0, -1, 0.2)
    ; (1, 1, 0.05)
    ; (1, -1, 0.05)
    ; (-1, 1, 0.05)
    ; (-1, -1, 0.05)
    ]
  in
  List.fold_left
    (fun (la, lb) (dx, dy, w) ->
       let nx = (x + dx + cells_x) mod cells_x in
       let ny = (y + dy + cells_y) mod cells_y in
       let a, b = sample frame nx ny in
       (la +. w *. a, lb +. w *. b))
    (0., 0.) weights

let feed_kill x y phase =
  let xf = float_of_int x /. float_of_int cells_x in
  let yf = float_of_int y /. float_of_int cells_y in
  let swirl = sin (tau *. (xf +. 0.11 *. sin (0.008 *. phase))) in
  let bands = cos (tau *. (yf -. 0.09 *. cos (0.007 *. phase))) in
  let feed = base_feed +. 0.0040 *. swirl +. 0.0030 *. bands in
  let kill = base_kill +. 0.0035 *. bands -. 0.0020 *. swirl in
  (clamp 0.016 0.070 feed, clamp 0.038 0.078 kill)

let pulse_force x y phase =
  let xf = float_of_int x in
  let yf = float_of_int y in
  let source amp radius cx cy =
    let dx = xf -. cx in
    let dy = yf -. cy in
    let d2 = dx *. dx +. dy *. dy in
    let r2 = radius *. radius in
    if d2 >= r2 then 0.
    else
      let w = 1. -. (d2 /. r2) in
      amp *. w
  in
  let cx1 = float_of_int cells_x *. (0.50 +. 0.28 *. sin (0.011 *. phase)) in
  let cy1 = float_of_int cells_y *. (0.52 +. 0.24 *. cos (0.013 *. phase)) in
  let cx2 = float_of_int cells_x *. (0.46 +. 0.33 *. cos (0.009 *. phase +. 1.7)) in
  let cy2 = float_of_int cells_y *. (0.47 +. 0.30 *. sin (0.010 *. phase +. 0.9)) in
  source 0.0065 (8.0 *. density_scale) cx1 cy1
  +. source 0.0050 (7.0 *. density_scale) cx2 cy2

(* Color palette driven by chemistry and local phase. *)
let color_of_ab a b phase =
  let a = clamp01 a in
  let b = clamp01 b in
  let glow = clamp01 (0.68 *. b +. 0.32 *. (1. -. a)) in
  let wave1 = 0.5 +. 0.5 *. sin (tau *. (b +. 0.012 *. phase)) in
  let wave2 = 0.5 +. 0.5 *. sin (tau *. (0.5 *. a +. 0.008 *. phase +. 0.2)) in
  let wave3 = 0.5 +. 0.5 *. sin (tau *. ((1. -. a) *. b +. 0.006 *. phase +. 0.47)) in
  let r = int_of_float (clamp 0. 255. ((35. +. 220. *. glow *. wave1))) in
  let g = int_of_float (clamp 0. 255. ((20. +. 210. *. glow *. wave2))) in
  let bl = int_of_float (clamp 0. 255. ((30. +. 230. *. glow *. wave3))) in
  (r, g, bl)

(* Behaviors *)

let rec handle_input input_signal stop () =
  match await input_signal with
  | 'q' -> emit stop ()
  | _ -> pause (); handle_input input_signal stop ()

(* Collect aggregated cell states into a frame signal (one emission per instant). *)
let frame_collector stop state_sig frame_sig init_frame =
  watch stop (fun () ->
      emit frame_sig init_frame;
      let rec loop () =
        let cells = await state_sig in
        let a_grid = Array.make_matrix cells_y cells_x 1. in
        let b_grid = Array.make_matrix cells_y cells_x 0. in
        List.iter (fun c -> a_grid.(c.y).(c.x) <- c.a; b_grid.(c.y).(c.x) <- c.b) cells;
        emit frame_sig { a_grid; b_grid };
        pause ();
        loop ()
      in
      loop ())

(* One cell behavior: reads last frame, computes next concentrations, emits current state. *)
let cell_behavior stop frame_sig state_sig init_state =
  watch stop (fun () ->
      let rec loop state =
        let frame = await frame_sig in
        let la, lb = laplacian frame state.x state.y in
        let a = state.a in
        let b = state.b in
        let feed, kill = feed_kill state.x state.y state.phase in
        let forcing = pulse_force state.x state.y state.phase in
        let reaction = a *. b *. b in
        let a' =
          clamp01
            (a +. (a_diff *. la -. reaction +. feed *. (1. -. a) -. (0.35 *. forcing)) *. dt)
        in
        let b' =
          clamp01
            (b +. (b_diff *. lb +. reaction -. (kill +. feed) *. b +. forcing) *. dt)
        in
        let next_state = { state with a = a'; b = b'; phase = state.phase +. 1. } in
        emit state_sig next_state;
        pause ();
        loop next_state
      in
      loop init_state)

let render_loop stop window renderer texture frame_sig () =
  watch stop (fun () ->
      let pixels = Array1.create int8_unsigned c_layout pixel_bytes in
      let frames = ref 0 in
      let last_fps_tick = ref (Sdl.get_ticks ()) in
      let rec loop () =
        let frame = await frame_sig in
        for y = 0 to cells_y - 1 do
          for x = 0 to cells_x - 1 do
            let a = frame.a_grid.(y).(x) in
            let b = frame.b_grid.(y).(x) in
            let phase =
              (float_of_int x *. 0.17) +. (float_of_int y *. 0.11)
            in
            let r, g, bl = color_of_ab a b phase in
            let off = ((y * cells_x) + x) * 4 in
            Array1.unsafe_set pixels off r;
            Array1.unsafe_set pixels (off + 1) g;
            Array1.unsafe_set pixels (off + 2) bl;
            Array1.unsafe_set pixels (off + 3) 255
          done
        done;
        ignore (Sdl.update_texture texture None pixels pixel_pitch);
        ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
        ignore (Sdl.render_clear renderer);
        ignore (Sdl.render_copy renderer texture);
        Sdl.render_present renderer;
        incr frames;
        let now = Sdl.get_ticks () in
        let elapsed_ms = Int32.sub now !last_fps_tick in
        if Int32.compare elapsed_ms 1000l >= 0 then begin
          let fps =
            (float_of_int !frames *. 1000.) /. Int32.to_float elapsed_ms
          in
          Sdl.set_window_title window
            (Printf.sprintf
               "Tempo reaction-diffusion showcase (SDL) - %.1f FPS - %d cells (%dx%d)"
               fps cells_count cells_x cells_y);
          frames := 0;
          last_fps_tick := now
        end;
        loop ()
      in
      loop ())

(* Scenario *)

let scenario window renderer texture input_signal _output_signal =
  let stop = new_signal () in
  let state_sig = new_signal_agg ~initial:[] ~combine:(fun acc c -> c :: acc) in
  let frame_sig = new_signal () in
  (* Initialize with several seeds and sparse random activations. *)
  let init_grid =
    let min_dim = min cells_x cells_y in
    let r_center1 = int_of_float (0.11 *. float_of_int min_dim) in
    let r_center2 = int_of_float (0.08 *. float_of_int min_dim) in
    let r_center3 = int_of_float (0.0625 *. float_of_int min_dim) in
    let ring_inner = int_of_float (0.14 *. float_of_int min_dim) in
    let ring_outer = int_of_float (0.178 *. float_of_int min_dim) in
    List.init cells_y (fun y ->
        List.init cells_x (fun x ->
            let center1 =
              abs (x - cells_x / 2) < r_center1
              && abs (y - cells_y / 2) < r_center1
            in
            let center2 =
              abs (x - cells_x / 3) < r_center2
              && abs (y - (2 * cells_y / 3)) < r_center2
            in
            let center3 =
              abs (x - (3 * cells_x / 4)) < r_center3
              && abs (y - (cells_y / 3)) < r_center3
            in
            let ring =
              let dx = x - cells_x / 2 in
              let dy = y - cells_y / 2 in
              let d2 = dx * dx + dy * dy in
              d2 > (ring_inner * ring_inner)
              && d2 < (ring_outer * ring_outer)
            in
            let noise = Random.float 1. < 0.012 in
            let b =
              if center1 || center2 || center3 then 1.
              else if ring then 0.65
              else if noise then 0.55
              else 0.
            in
            let a = clamp01 (1. -. (0.30 *. b)) in
            let phase = Random.float tau in
            { x; y; a; b; phase }))
    |> List.flatten
  in
  let init_frame =
    let a_grid = Array.make_matrix cells_y cells_x 1. in
    let b_grid = Array.make_matrix cells_y cells_x 0. in
    List.iter (fun c -> a_grid.(c.y).(c.x) <- c.a; b_grid.(c.y).(c.x) <- c.b) init_grid;
    { a_grid; b_grid }
  in
  watch stop (fun () ->
      parallel
        ((fun () -> handle_input input_signal stop ()) ::
         (fun () -> frame_collector stop state_sig frame_sig init_frame) ::
         (fun () -> render_loop stop window renderer texture frame_sig ()) ::
         List.map (fun c -> fun () -> cell_behavior stop frame_sig state_sig c) init_grid))

(* Execution *)

let () =
  Random.self_init ();
  (* Keep nearest-neighbor scaling to preserve crisp cell colors. *)
  ignore (Sdl.set_hint "SDL_RENDER_SCALE_QUALITY" "0");
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () ->
      match Sdl.create_window ~w:width ~h:height "Tempo reaction-diffusion showcase (SDL)"
              Sdl.Window.windowed
      with
      | Error (`Msg e) ->
          Sdl.log "SDL window creation error: %s" e;
          exit 1
      | Ok window -> (
          match Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated with
          | Error (`Msg e) ->
              Sdl.log "SDL renderer creation error: %s" e;
              Sdl.destroy_window window;
              Sdl.quit ();
              exit 1
          | Ok renderer -> (
              match
                (* ABGR8888 gives a byte layout that matches how we fill the
                   streaming buffer (r,g,b,a bytes) on little-endian systems. *)
                Sdl.create_texture renderer Sdl.Pixel.format_abgr8888
                  Sdl.Texture.access_streaming ~w:cells_x ~h:cells_y
              with
              | Error (`Msg e) ->
                  Sdl.log "SDL texture creation error: %s" e;
                  Sdl.destroy_renderer renderer;
                  Sdl.destroy_window window;
                  Sdl.quit ();
                  exit 1
              | Ok texture ->
          Sdl.set_window_title window
            (Printf.sprintf
               "Tempo reaction-diffusion showcase (SDL) - %d cells (%dx%d)"
               cells_count cells_x cells_y);
          let event = Sdl.Event.create () in
          let input () =
            if Sdl.poll_event (Some event) then
              match Sdl.Event.(enum (get event typ)) with
              | `Quit -> Some 'q'
              | `Key_down ->
                  let sc = Sdl.Event.get event Sdl.Event.keyboard_scancode in
                  let kc = Sdl.Event.get event Sdl.Event.keyboard_keycode in
                  if sc = Sdl.Scancode.q || kc = Sdl.K.q || kc = Sdl.K.escape then Some 'q' else None
              | _ -> None
            else None
          in
          let output _ = () in
          execute ~input ~output (scenario window renderer texture);
          Sdl.destroy_texture texture;
          Sdl.destroy_renderer renderer;
          Sdl.destroy_window window;
          Sdl.quit ()))
