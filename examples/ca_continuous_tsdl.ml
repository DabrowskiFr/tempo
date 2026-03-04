open Tempo
open Tsdl

(* Continuous cellular automaton (Gray-Scott-like) rendered with SDL.
   One behavior per cell; communication only via signals. *)

type cell_state = { x : int; y : int; a : float; b : float }
type frame = { a_grid : float array array; b_grid : float array array }

let width = 600
let height = 400
let cells_x = 60
let cells_y = 40
let cell_w = width / cells_x
let cell_h = height / cells_y

(* Parameter set biased toward fragmentation (“mitosis” style). *)
let a_diff = 1.0
let b_diff = 0.5
let feed = 0.0367
let kill = 0.0649
let dt = 1.2

let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v

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

(* Simple grayscale based on B concentration. *)
let color_of_ab _a b =
  let v = int_of_float (clamp01 b *. 255.) in
  (v, v, v)

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
        let reaction = a *. b *. b in
        let a' = clamp01 (a +. (a_diff *. la -. reaction +. feed *. (1. -. a)) *. dt) in
        let b' = clamp01 (b +. (b_diff *. lb +. reaction -. (kill +. feed) *. b) *. dt) in
        let next_state = { state with a = a'; b = b' } in
        emit state_sig next_state;
        pause ();
        loop next_state
      in
      loop init_state)

let render_loop stop renderer frame_sig () =
  watch stop (fun () ->
      let rec loop () =
        let frame = await frame_sig in
        ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
        ignore (Sdl.render_clear renderer);
        for y = 0 to cells_y - 1 do
          for x = 0 to cells_x - 1 do
            let a = frame.a_grid.(y).(x) in
            let b = frame.b_grid.(y).(x) in
            let r, g, bl = color_of_ab a b in
            ignore (Sdl.set_render_draw_color renderer r g bl 255);
            ignore
              (Sdl.render_fill_rect renderer
                 (Some (Sdl.Rect.create ~x:(x * cell_w) ~y:(y * cell_h) ~w:cell_w ~h:cell_h)))
          done
        done;
        Sdl.render_present renderer;
        Sdl.delay 16l;
        loop ()
      in
      loop ())

(* Scenario *)

let scenario renderer input_signal _output_signal =
  let stop = new_signal () in
  let state_sig = new_signal_agg ~initial:[] ~combine:(fun acc c -> c :: acc) in
  let frame_sig = new_signal () in
  (* Initialize grid: mostly A, small B square in center. *)
  let init_grid =
    List.init cells_y (fun y ->
        List.init cells_x (fun x ->
            let central = abs (x - cells_x / 2) < 6 && abs (y - cells_y / 2) < 6 in
            let noise = Random.float 1. < 0.1 in
            let b =
              if central then 1.
              else if noise then 0.8
              else 0.
            in
            let a = 1. in
            { x; y; a; b }))
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
         (fun () -> render_loop stop renderer frame_sig ()) ::
         List.map (fun c -> fun () -> cell_behavior stop frame_sig state_sig c) init_grid))

(* Execution *)

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
          Sdl.set_window_title window "Tempo continuous CA (SDL)";
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
          execute ~input ~output (scenario renderer);
          Sdl.destroy_renderer renderer;
          Sdl.destroy_window window;
          Sdl.quit ()
