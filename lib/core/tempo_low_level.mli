type kill = Tempo_types.kill

val new_kill : unit -> kill
val abort_kill : kill -> unit
val with_kill : kill -> (unit -> unit) -> unit
