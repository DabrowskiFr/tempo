open Types

type t = {
  io : (input_state, frame) Game_io.t;
  frames_rev : frame list ref;
}

let idle_input =
  {
    start = false;
    pause_toggle = false;
    difficulty_set = None;
    up = false;
    down = false;
    left = false;
    right = false;
    ask = false;
    drink = false;
    restart = false;
    radius_down = false;
    radius_up = false;
    radius_set_t = None;
    quit = false;
  }

let create ?(default_input = Some idle_input) (script : input_state option array) =
  let cursor = ref 0 in
  let frames_rev = ref [] in
  let read_input () =
    if !cursor < Array.length script then (
      let v = script.(!cursor) in
      incr cursor;
      v)
    else default_input
  in
  let write_output frame = frames_rev := frame :: !frames_rev in
  let io =
    {
      Game_io.init = (fun () -> ());
      shutdown = (fun () -> ());
      read_input;
      write_output;
    }
  in
  { io; frames_rev }

let frames t = List.rev !(t.frames_rev)
