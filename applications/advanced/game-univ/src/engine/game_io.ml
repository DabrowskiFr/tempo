type ('input, 'output) t = {
  init : unit -> unit;
  shutdown : unit -> unit;
  read_input : unit -> 'input option;
  wait_input : unit -> unit;
  write_output : 'output -> unit;
}
