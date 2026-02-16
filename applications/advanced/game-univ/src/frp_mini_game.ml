open Tempo
open Tempo_frp
open Raylib

type scene = Menu | Play

type input_state = {
  left : bool;
  right : bool;
  toggle_down : bool;
  quit : bool;
}

type frame =
  | Menu_frame of string
  | Play_frame of float * string

let width = 900
let height = 520

let clamp v ~min ~max = if v < min then min else if v > max then max else v
let to_int f = int_of_float f

let poll_input () =
  if window_should_close () || is_key_pressed Key.Escape then raise Exit;
  Some
    {
      left = is_key_down Key.Left;
      right = is_key_down Key.Right;
      toggle_down = is_key_down Key.Tab;
      quit = false;
    }

let render = function
  | Menu_frame msg ->
      begin_drawing ();
      clear_background (Color.create 25 32 46 255);
      draw_text "Tempo FRP Mini Game" 280 120 42 Color.raywhite;
      draw_text "TAB: switch scene | ESC: quit" 270 190 24 (Color.create 196 220 242 255);
      draw_text msg 230 260 28 (Color.create 125 227 171 255);
      end_drawing ()
  | Play_frame (x, msg) ->
      begin_drawing ();
      clear_background (Color.create 246 244 236 255);
      draw_text "Play Scene (switch_latest + hold/sample)" 180 24 28 (Color.create 39 59 79 255);
      draw_rectangle 60 430 780 18 (Color.create 88 91 98 255);
      draw_rectangle (to_int x) 390 110 40 (Color.create 67 145 255 255);
      draw_text msg 180 88 24 (Color.create 42 88 65 255);
      draw_text "LEFT/RIGHT to move" 20 478 18 (Color.create 47 66 86 255);
      end_drawing ()

let main input output =
  let stop = new_signal () in
  let scene_req = new_signal () in
  let toggle_level = new_signal () in
  let toggle_edge = Frp.edge toggle_level in
  let first_toggle = Frp.once toggle_edge in
  let show_hint = new_state true in

  let quit_watcher () =
    let rec loop () =
      let i = await input in
      if i.quit then emit stop () else loop ()
    in
    loop ()
  in

  let publish_toggle_level () =
    let rec loop () =
      let i = await input in
      emit toggle_level i.toggle_down;
      loop ()
    in
    loop ()
  in

  let first_toggle_watcher () =
    let _ = await first_toggle in
    set_state show_hint false;
    let rec idle () =
      pause ();
      idle ()
    in
    idle ()
  in

  let scene_controller () =
    emit scene_req Menu;
    let current = ref Menu in
    let rec loop () =
      let _ = await toggle_edge in
      let next = match !current with Menu -> Play | Play -> Menu in
      current := next;
      emit scene_req next;
      loop ()
    in
    loop ()
  in

  let build_scene = function
    | Menu ->
        (fun () ->
          let rec loop () =
            let _ = await input in
            emit output (Menu_frame "Press TAB to enter Play scene");
            loop ()
          in
          loop ())
    | Play ->
        (fun () ->
          let x_target = new_signal () in
          let x_state = Frp.hold ~initial:395.0 x_target in
          let rec loop () =
            let i = await input in
            let x = Frp.sample x_state in
            let dx = (if i.right then 6.0 else 0.0) -. (if i.left then 6.0 else 0.0) in
            let nx = clamp (x +. dx) ~min:60.0 ~max:730.0 in
            emit x_target nx;
            let msg =
              if get_state show_hint then "First TAB detected with once()" else "Switch scenes anytime with TAB"
            in
            emit output (Play_frame (nx, msg));
            loop ()
          in
          loop ())
  in

  let core () =
    parallel
      [ publish_toggle_level
      ; first_toggle_watcher
      ; scene_controller
      ; (fun () -> Frp.switch_latest scene_req build_scene)
      ]
  in

  parallel [ (fun () -> watch stop core); quit_watcher ]

let () =
  init_window width height "Tempo FRP mini game";
  set_target_fps 60;
  (try execute ~input:poll_input ~output:render main with Exit -> ());
  close_window ()
