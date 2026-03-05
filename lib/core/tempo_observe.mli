(** Observation helpers around the runtime. *)

type 'a timeline_instant = { instant : int; output : 'a option }
type inspector_snapshot = { instant : int; runnable : int; blocked : int }

val execute_trace :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ('input Tempo_base.signal -> 'output Tempo_base.signal -> unit)
  -> 'output list

val execute_timeline :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ('input Tempo_base.signal -> 'output Tempo_base.signal -> unit)
  -> 'output timeline_instant list

val execute_inspect :
     ?instants:int
  -> ?input:(unit -> 'input option)
  -> ('input Tempo_base.signal -> 'output Tempo_base.signal -> unit)
  -> inspector_snapshot list
