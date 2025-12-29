open Tempo
open Tsdl

(* -- Planet physics *)

type frame = { planet : int * int; moon : int * int; paused : bool }

let width = 640
let height = 480

let cx = width / 2
let cy = height / 2

let sun_radius = 20
let planet_orbit = 140
let moon_orbit = 40
let planet_radius = 12
let moon_radius = 6

let planet_speed = 0.02
let moon_speed = 0.07

let to_xy radius angle =
  let x = float cx +. radius *. cos angle in
  let y = float cy +. radius *. sin angle in
  (int_of_float x, int_of_float y)

(* -- SDL helpers *)

let set_color r (cr, cg, cb) =
  ignore (Sdl.set_render_draw_color r cr cg cb 255)

let fill_rect r (x, y, w, h) =
  let rect = Sdl.Rect.create ~x ~y ~w ~h in
  ignore (Sdl.render_fill_rect r (Some rect))

let fill_circle r (cx, cy) radius color =
  set_color r color;
  for dy = -radius to radius do
    let dx =
      int_of_float
        (sqrt (float_of_int (radius * radius - (dy * dy))))
    in
    let x = cx - dx in
    let w = (2 * dx) + 1 in
    fill_rect r (x, cy + dy, w, 1)
  done

(* -- Drawing *)

let draw_frame renderer { planet = (px, py); moon = (mx, my); paused } =
  set_color renderer (0, 0, 0);
  ignore (Sdl.render_clear renderer);
  fill_circle renderer (cx, cy) sun_radius (255, 215, 0);
  fill_circle renderer (px, py) planet_radius
    (if paused then (70, 130, 180) else (30, 144, 255));
  fill_circle renderer (mx, my) moon_radius (192, 192, 192);
  Sdl.render_present renderer;
  Unix.sleepf 0.016

(* -- Scenario *)

let scenario input_signal output_signal =
  let stop = new_signal () in
  let toggle = new_signal () in
  let paused = ref false in
  watch stop (fun () ->
      let rec handle_input () =
        match await input_signal with
        | 'q' -> emit stop ()
        | 'p' ->
            emit toggle ();
            handle_input ()
        | _ -> handle_input ()
      in
      let rec toggle_listener () =
        let _ = await toggle in
        paused := not !paused;
        toggle_listener ()
      in
      let rec animate angle_p angle_m =
        let paused_now = !paused in
        let angle_p' =
          if paused_now then angle_p else angle_p +. planet_speed
        in
        let angle_m' = angle_m +. moon_speed in
        let px, py = to_xy (float planet_orbit) angle_p' in
        let dx = int_of_float (float moon_orbit *. cos angle_m') in
        let dy = int_of_float (float moon_orbit *. sin angle_m') in
        let mx = px + dx in
        let my = py + dy in
        emit output_signal
          { planet = (px, py); moon = (mx, my); paused = paused_now };
        pause ();
        animate angle_p' angle_m'
      in
      parallel [ handle_input; toggle_listener; (fun () -> animate 0. 0.) ])

let () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () ->
      match
        Sdl.create_window_and_renderer ~w:width ~h:height Sdl.Window.windowed
      with
      | Error (`Msg e) ->
          Sdl.log "SDL window error: %s" e;
          exit 1
      | Ok (window, renderer) ->
          Sdl.set_window_title window "Tempo solar system (SDL)";
          let event = Sdl.Event.create () in
          let input () =
            if Sdl.poll_event (Some event) then
              match Sdl.Event.(enum (get event typ)) with
              | `Quit -> Some 'q'
              | `Key_down ->
                  let key = Sdl.Event.get event Sdl.Event.keyboard_keycode in
                  if key = Sdl.K.q then Some 'q'
                  else if key = Sdl.K.p then Some 'p'
                  else None
              | _ -> None
            else None
          in
          let output frame = draw_frame renderer frame in
          execute ~input ~output scenario;
          Sdl.destroy_renderer renderer;
          Sdl.destroy_window window;
          Sdl.quit ()
