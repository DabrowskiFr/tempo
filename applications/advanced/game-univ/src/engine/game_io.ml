type ('input, 'output) t = {
  init : unit -> unit;
  shutdown : unit -> unit;
  read_input : unit -> 'input option;
  write_output : 'output -> unit;
}
