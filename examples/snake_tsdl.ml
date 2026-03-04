open Tempo
open Tsdl

(* Grid setup: logical grid of 1px cells; snake segments are drawn larger for visibility. *)
let cell = 1
let snake_size = 20
let cols = 640
let rows = 480
let width = cols * cell
let height = rows * cell

type dir = Up | Down | Left | Right
(* Snake state carried by the behavior. *)
type snake_state =
  { snake : (int * int) list
  ; dir : dir
  ; alive : bool
  ; grow : int
  }

let initial_state =
  { snake = [ (cols / 2, rows / 2) ]
  ; dir = Right
  ; alive = true
  ; grow = 0
  }

let overlaps (x1, y1) (x2, y2) =
  abs (x1 - x2) < snake_size && abs (y1 - y2) < snake_size

let turn dir key =
  match key, dir with
  | 'u', Down | 'd', Up | 'l', Right | 'r', Left -> dir (* ignore reverse *)
  | 'u', _ -> Up
  | 'd', _ -> Down
  | 'l', _ -> Left
  | 'r', _ -> Right
  | _ -> dir

let rec drop_last = function
  | [] -> []
  | [ _ ] -> []
  | x :: xs -> x :: drop_last xs

(* Compute next snake state given current food. Returns (state, ate?, next_food). *)
let step food { snake; dir; grow; _ } =
  let hx, hy = List.hd snake in
  let nx, ny =
    match dir with
    | Up -> (hx, (hy - 1 + rows) mod rows)
    | Down -> (hx, (hy + 1) mod rows)
    | Left -> ((hx - 1 + cols) mod cols, hy)
    | Right -> ((hx + 1) mod cols, hy)
  in
  let eats = overlaps (nx, ny) food in
  let grow' = if eats then grow + snake_size else grow in
  let grow_after_move = if grow' > 0 then grow' - 1 else 0 in
  let new_snake =
    if grow' > 0 then (nx, ny) :: snake else (nx, ny) :: drop_last snake
  in
  let rec skip n lst =
    if n <= 0 then lst else match lst with [] -> [] | _ :: tl -> skip (n - 1) tl
  in
  let tail = skip (snake_size - 1) (List.tl new_snake) in
  let collides = List.exists (fun (x, y) -> x = nx && y = ny) tail in
  let rec new_food () =
    let fx = Random.int cols in
    let fy = Random.int rows in
    if List.exists (fun p -> overlaps p (fx, fy)) new_snake then new_food () else (fx, fy)
  in
  ( { snake = new_snake
    ; dir
    ; alive = not collides
    ; grow = grow_after_move
    }
  , eats
  , if eats then new_food () else food )

(* SDL helpers *)
let set_color r (cr, cg, cb) = ignore (Sdl.set_render_draw_color r cr cg cb 255)

let fill_rect r (x, y, w, h) =
  let rect = Sdl.Rect.create ~x ~y ~w ~h in
  ignore (Sdl.render_fill_rect r (Some rect))

let draw_snake_cell r (cx, cy) color =
  set_color r color;
  (* Render a snake segment as a solid block; cheaper than per-cell drawing
     to keep frame time stable as the snake grows. *)
  let size = max snake_size cell in
  fill_rect r (cx * cell, cy * cell, size, size)

let draw_game_over renderer =
  let w = width / 2 in
  let h = height / 3 in
  let x = (width - w) / 2 in
  let y = (height - h) / 2 in
  set_color renderer (30, 0, 0);
  fill_rect renderer (x, y, w, h);
  set_color renderer (200, 50, 50);
  ignore (Sdl.render_draw_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)));
  ignore (Sdl.render_draw_line renderer x y (x + w) (y + h));
  ignore (Sdl.render_draw_line renderer (x + w) y x (y + h))

let draw_frame renderer { snake; alive; _ } food =
  set_color renderer (20, 20, 20);
  ignore (Sdl.render_clear renderer);
  List.iter (fun (x, y) -> draw_snake_cell renderer (x, y) (0, 200, 70)) snake;
  draw_snake_cell renderer food (220, 80, 40);
  if not alive then draw_game_over renderer;
  Sdl.render_present renderer;
  (* Faster tick for snappier movement. *)
  Unix.sleepf 0.005

(* Processes *)

(* Input loop: map SDL events to direction/quit signals. *)
let rec handle_input input_signal stop dir_signal () =
  match await input_signal with
  | 'q' -> emit stop ()
  | c ->
      emit dir_signal c;
      handle_input input_signal stop dir_signal ()

let snake_behavior stop current_dir state_sig () =
  watch stop (fun () ->
      let current_food = ref (5, 5) in
      (* Main loop: advances while alive, freezes (keeps emitting last state) when dead. *)
      let rec tick state =
        if not state.alive then (
          (* Keep displaying the last state, no more movement. *)
          emit state_sig (state, !current_food);
          pause ();
          tick state)
        else (
          let dir' = !current_dir in
          let state' = { state with dir = dir' } in
          let state'', _, next_food = step !current_food state' in
          emit state_sig (state'', !current_food);
          if state''.alive then begin
            pause ();
            current_food := next_food;
            tick state''
          end else (
            (* Freeze on game over. *)
            pause ();
            tick state''))
      in
      tick initial_state)

(* Scenario *)
let scenario input_signal output_signal =
  let stop = new_signal () in
  let dir_signal = new_signal () in (* carries direction chars from the input behavior *)
  (* output_signal (from execute) carries (snake_state * food) frames to the renderer *)
  let current_dir = ref initial_state.dir in
  (* Update current_dir whenever a direction is emitted. *)
  let rec dir_listener () =
    let c = await dir_signal in
    current_dir := turn !current_dir c;
    dir_listener ()
  in
  watch stop (fun () ->
      parallel
        [ handle_input input_signal stop dir_signal
        ; dir_listener
        ; snake_behavior stop current_dir output_signal
        ])

(* SDL harness *)
let () =
  Random.self_init ();
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) ->
      Sdl.log "SDL init error: %s" e;
      exit 1
  | Ok () ->
      match Sdl.create_window_and_renderer ~w:width ~h:height Sdl.Window.windowed with
      | Error (`Msg e) ->
          Sdl.log "SDL window error: %s" e;
          exit 1
      | Ok (window, renderer) ->
          Sdl.set_window_title window "Tempo snake (SDL)";
          let event = Sdl.Event.create () in
          let input () =
            if Sdl.poll_event (Some event) then
              match Sdl.Event.(enum (get event typ)) with
              | `Quit -> Some 'q'
              | `Key_down ->
                  let sc = Sdl.Event.get event Sdl.Event.keyboard_scancode in
                  let kc = Sdl.Event.get event Sdl.Event.keyboard_keycode in
                  if sc = Sdl.Scancode.q || kc = Sdl.K.q || kc = Sdl.K.escape then Some 'q'
                  else if sc = Sdl.Scancode.up || kc = Sdl.K.up then Some 'u'
                  else if sc = Sdl.Scancode.down || kc = Sdl.K.down then Some 'd'
                  else if sc = Sdl.Scancode.left || kc = Sdl.K.left then Some 'l'
                  else if sc = Sdl.Scancode.right || kc = Sdl.K.right then Some 'r'
                  else None
              | _ -> None
            else None
          in
          let output (state, food) = draw_frame renderer state food in
          execute ~input ~output scenario;
          Sdl.destroy_renderer renderer;
          Sdl.destroy_window window;
          Sdl.quit ()
