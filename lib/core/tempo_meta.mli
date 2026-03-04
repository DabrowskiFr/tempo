(** Package metadata and compatibility helpers. *)

val version_string : string
(** Version string of the installed package. *)

val api_level : int
(** Monotonic API level used to gate compatibility checks. *)

val require_api_level : int -> unit
(** [require_api_level expected] raises [Invalid_argument] if the currently
    loaded runtime exposes an older API level than [expected]. *)
