let run ?instants ~(io : ('input, 'output) Game_io.t)
    (main : 'input Tempo.signal -> 'output Tempo.signal -> unit) =
  io.init ();
  Fun.protect
    ~finally:io.shutdown
    (fun () ->
      match instants with
      | Some instants ->
          Tempo.execute ~instants ~input:io.read_input ~output:io.write_output main
      | None ->
          Tempo.run_interactive
            ~output:io.write_output
            ~input:{ Tempo.poll = io.read_input; wait = io.wait_input }
            main)
