let run ?instants ~(io : ('input, 'output) Game_io.t)
    (main : 'input Tempo.signal -> 'output Tempo.signal -> unit) =
  io.init ();
  Fun.protect
    ~finally:io.shutdown
    (fun () -> Tempo.execute ?instants ~input:io.read_input ~output:io.write_output main)
