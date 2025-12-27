val execute :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ?output:('output -> unit)
  -> ('input Tempo_types.signal -> 'output Tempo_types.signal -> unit)
  -> unit
