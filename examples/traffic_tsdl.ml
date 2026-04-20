open Tempo
open Tsdl

type car_state =
  { id : int
  ; s : float
  ; v : float
  ; v0 : float
  ; accel : float
  ; comfort_brake : float
  ; time_headway : float
  ; color_h : float
  }

type snapshot =
  { cars : car_state array
  ; tick : int
  }

type frame =
  { snapshot : snapshot
  ; avg_speed : float
  ; min_gap : float
  }

let width = 1360
let height = 840
let cars_count = 260
let road_length = 3600.0
let car_length = 6.0
let min_spacing = 2.5
let dt = 0.18
let model_delta = 4.0
let max_decel = 10.0
let tau = 6.283185307179586

let center_x = 0.5 *. float_of_int width
let center_y = 0.53 *. float_of_int height
let radius_x = 0.39 *. float_of_int width
let radius_y = 0.31 *. float_of_int height

let clamp lo hi v = if v < lo then lo else if v > hi then hi else v
let safe_div num den = if den <= 1e-9 then 0.0 else num /. den
let mod_pos x m = let r = mod_float x m in if r < 0.0 then r +. m else r

let hue_to_rgb h =
  let h = h -. floor h in
  let h6 = h *. 6.0 in
  let c = 1.0 in
  let x = c *. (1.0 -. abs_float ((mod_float h6 2.0) -. 1.0)) in
  let r, g, b =
    if h6 < 1.0 then c, x, 0.0
    else if h6 < 2.0 then x, c, 0.0
    else if h6 < 3.0 then 0.0, c, x
    else if h6 < 4.0 then 0.0, x, c
    else if h6 < 5.0 then x, 0.0, c
    else c, 0.0, x
  in
  let to_i v = int_of_float (255.0 *. clamp 0.0 1.0 v) in
  to_i r, to_i g, to_i b

let road_xy s =
  let angle = tau *. (s /. road_length) in
  let x = center_x +. (radius_x *. cos angle) in
  let y = center_y +. (radius_y *. sin angle) in
  (x, y)

let draw_disc renderer x y radius (cr, cg, cb) =
  ignore (Sdl.set_render_draw_color renderer cr cg cb 255);
  for dy = -radius to radius do
    let yy = float_of_int ((radius * radius) - (dy * dy)) in
    let dx = int_of_float (sqrt yy) in
    ignore (Sdl.render_draw_line renderer (x - dx) (y + dy) (x + dx) (y + dy))
  done

let make_initial_car i =
  let spacing = road_length /. float_of_int cars_count in
  let s = float_of_int i *. spacing in
  let density = float_of_int i /. float_of_int (cars_count - 1) in
  let v0 = 24.0 +. (7.0 *. sin (0.16 *. float_of_int i)) in
  let v = 12.0 +. (5.0 *. cos (0.12 *. float_of_int i)) in
  let accel = 0.9 +. (0.6 *. Random.float 1.0) in
  let comfort_brake = 1.4 +. (1.1 *. Random.float 1.0) in
  let time_headway = 0.95 +. (0.85 *. Random.float 1.0) in
  { id = i
  ; s
  ; v
  ; v0
  ; accel
  ; comfort_brake
  ; time_headway
  ; color_h = 0.58 +. (0.33 *. density)
  }

let leader_of id cars =
  let j = if id = Array.length cars - 1 then 0 else id + 1 in
  cars.(j)

let gap_to_leader car leader =
  let raw = if leader.s >= car.s then leader.s -. car.s else road_length -. car.s +. leader.s in
  max min_spacing (raw -. car_length)

let braking_zone tick s =
  let center = 0.28 *. road_length in
  let width = 0.095 *. road_length in
  let dist =
    let d = abs_float (s -. center) in
    min d (road_length -. d)
  in
  if dist > width then 0.0
  else
    let x = 1.0 -. (dist /. width) in
    let pulse = 0.52 +. (0.48 *. sin (0.021 *. float_of_int tick)) in
    1.6 *. x *. pulse

let idm_accel tick car cars =
  let leader = leader_of car.id cars in
  let gap = gap_to_leader car leader in
  let dv = car.v -. leader.v in
  let s_star =
    min_spacing
    +. max 0.0
         (car.v *. car.time_headway
          +. safe_div (car.v *. dv) (2.0 *. sqrt (car.accel *. car.comfort_brake)))
  in
  let free_term = (car.v /. car.v0) ** model_delta in
  let interact_term = (s_star /. gap) ** 2.0 in
  let base = car.accel *. (1.0 -. free_term -. interact_term) in
  let zone = braking_zone tick car.s in
  max (-.max_decel) (base -. zone)

let step_car tick car cars =
  let leader = leader_of car.id cars in
  let gap = gap_to_leader car leader in
  let a = idm_accel tick car cars in
  let v_next = clamp 0.0 40.0 (car.v +. (a *. dt)) in
  let move_raw = max 0.0 ((car.v *. dt) +. (0.5 *. a *. dt *. dt)) in
  let move_cap = max 0.0 (gap -. min_spacing) in
  let move = min move_raw move_cap in
  let v_limited = if move < move_raw then min v_next (safe_div move dt) else v_next in
  let s_next = mod_pos (car.s +. move) road_length in
  { car with s = s_next; v = v_limited }

let rec handle_input input_signal stop () =
  match await input_signal with
  | 'q' -> emit stop ()
  | _ ->
      pause ();
      handle_input input_signal stop ()

let car_behavior stop snapshot_sig updates_sig init_state =
  watch stop (fun () ->
      let rec loop state =
        let snap = await snapshot_sig in
        let next = step_car snap.tick state snap.cars in
        emit updates_sig next;
        pause ();
        loop next
      in
      loop init_state)

let frame_collector stop updates_sig snapshot_sig output_signal init_snapshot =
  watch stop (fun () ->
      emit snapshot_sig init_snapshot;
      let rec loop snap =
        let updates = await updates_sig in
        let next_cars = Array.copy snap.cars in
        List.iter (fun car -> next_cars.(car.id) <- car) updates;
        let total_speed, min_gap =
          Array.fold_left
            (fun (speed_acc, gap_min) car ->
               let leader = leader_of car.id next_cars in
               let gap = gap_to_leader car leader in
               (speed_acc +. car.v, min gap_min gap))
            (0.0, road_length) next_cars
        in
        let avg_speed = total_speed /. float_of_int cars_count in
        let next_snapshot = { cars = next_cars; tick = snap.tick + 1 } in
        emit output_signal { snapshot = next_snapshot; avg_speed; min_gap };
        emit snapshot_sig next_snapshot;
        pause ();
        loop next_snapshot
      in
      loop init_snapshot)

let draw_road renderer =
  ignore (Sdl.set_render_draw_color renderer 16 20 30 255);
  ignore (Sdl.render_clear renderer);
  let steps = 720 in
  for i = 0 to steps - 1 do
    let s1 = road_length *. float_of_int i /. float_of_int steps in
    let s2 = road_length *. float_of_int (i + 1) /. float_of_int steps in
    let x1, y1 = road_xy s1 in
    let x2, y2 = road_xy s2 in
    ignore (Sdl.set_render_draw_color renderer 58 66 84 255);
    ignore (Sdl.render_draw_line renderer (int_of_float x1) (int_of_float y1) (int_of_float x2)
              (int_of_float y2));
    if i mod 18 = 0 then (
      ignore (Sdl.set_render_draw_color renderer 220 220 210 140);
      ignore
        (Sdl.render_draw_line renderer (int_of_float (x1 *. 0.995 +. center_x *. 0.005))
           (int_of_float (y1 *. 0.995 +. center_y *. 0.005))
           (int_of_float (x1 *. 1.005 -. center_x *. 0.005))
           (int_of_float (y1 *. 1.005 -. center_y *. 0.005))))
  done

let draw_frame renderer frame =
  draw_road renderer;
  Array.iter
    (fun car ->
       let x, y = road_xy car.s in
       let speed_t = clamp 0.0 1.0 (car.v /. 34.0) in
       let hue = mod_float (car.color_h +. (0.27 *. speed_t)) 1.0 in
       let r, g, b = hue_to_rgb hue in
       draw_disc renderer (int_of_float x) (int_of_float y) 4 (r, g, b))
    frame.snapshot.cars;
  Sdl.render_present renderer

let scenario input_signal output_signal =
  let stop = new_signal () in
  let snapshot_sig = new_signal () in
  let updates_sig = new_signal_agg ~initial:[] ~combine:(fun acc car -> car :: acc) in
  let init_cars = Array.init cars_count make_initial_car in
  let init_snapshot = { cars = init_cars; tick = 0 } in
  watch stop (fun () ->
      parallel
        ((fun () -> handle_input input_signal stop ())
         :: (fun () -> frame_collector stop updates_sig snapshot_sig output_signal init_snapshot)
         :: Array.to_list
              (Array.map (fun car -> fun () -> car_behavior stop snapshot_sig updates_sig car)
                 init_cars)))

let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () ->
      match
        Sdl.create_window ~w:width ~h:height "Tempo traffic (SDL)" Sdl.Window.windowed
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
          | Ok renderer ->
              let event = Sdl.Event.create () in
              let frames = ref 0 in
              let last_tick = ref (Sdl.get_ticks ()) in
              let input () =
                if Sdl.poll_event (Some event) then
                  match Sdl.Event.(enum (get event typ)) with
                  | `Quit -> Some 'q'
                  | `Key_down ->
                      let sc = Sdl.Event.get event Sdl.Event.keyboard_scancode in
                      let kc = Sdl.Event.get event Sdl.Event.keyboard_keycode in
                      if sc = Sdl.Scancode.q || kc = Sdl.K.q || kc = Sdl.K.escape
                      then Some 'q'
                      else None
                  | _ -> None
                else None
              in
              let output frame =
                draw_frame renderer frame;
                incr frames;
                let now = Sdl.get_ticks () in
                let elapsed = Int32.sub now !last_tick in
                if Int32.compare elapsed 1000l >= 0 then (
                  let fps = (float_of_int !frames *. 1000.0) /. Int32.to_float elapsed in
                  let kmh = frame.avg_speed *. 3.6 in
                  Sdl.set_window_title window
                    (Printf.sprintf
                       "Tempo traffic (SDL) - %.1f FPS - %d cars - avg %.1f km/h - min gap %.1f"
                       fps cars_count kmh frame.min_gap);
                  frames := 0;
                  last_tick := now)
              in
              execute ~input ~output scenario;
              Sdl.destroy_renderer renderer;
              Sdl.destroy_window window;
              Sdl.quit ())
