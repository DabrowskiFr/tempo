open Raylib

type ('input, 'output) spec = {
  width : int;
  height : int;
  title : string;
  fps : int;
  on_init : unit -> unit;
  on_shutdown : unit -> unit;
  read_input : unit -> 'input option;
  render : 'output -> unit;
}

let run ~(spec : ('input, 'output) spec)
    (main : 'input Tempo.signal -> 'output Tempo.signal -> unit) =
  let io : ('input, 'output) Game_io.t =
    {
      init =
        (fun () ->
          init_window spec.width spec.height spec.title;
          set_target_fps spec.fps;
          spec.on_init ());
      shutdown =
        (fun () ->
          spec.on_shutdown ();
          if is_window_ready () then close_window ());
      read_input = spec.read_input;
      write_output = spec.render;
    }
  in
  Tempo_runtime.run ~io main
