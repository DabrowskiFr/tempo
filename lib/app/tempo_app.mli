(** Application-oriented helpers built on top of {!Tempo}.

    This package is intentionally small. It groups three reusable patterns:

    - {!module:App} for message-driven applications with commands,
    - {!module:Loop} for a simpler ticked state loop,
    - {!module:Scene} for scene switching with enter/exit callbacks.

    The recommended reading order is {!module:App}, then {!module:Loop}, then
    {!module:Scene}. *)

module App : sig
  include module type of Tempo.App

  (** [none] is the empty command. It leaves the current model unchanged and
      schedules no follow-up work. *)

  (** [emit msg] dispatches [msg] immediately into the current application. *)

  (** [after_n n msg] dispatches [msg] after [n] logical instants. *)

  (** [every_n n msg] dispatches [msg] periodically every [n] logical instants. *)

  (** [tick_every n ~tick] is a named alias for {!val:every_n} when the message
      models a periodic tick. *)

  (** [tick_if enabled n ~tick] enables or disables a periodic tick command. *)

  (** [command_if cond cmd] keeps [cmd] only when [cond] holds. *)

  (** [command_when cond ~then_ ~else_] selects one of two commands. *)

  (** [batch cmds] executes every command in [cmds] in order. *)

  (** [boot_once_input ~boot input] injects [boot] exactly once before
      delegating to [input]. *)

  (** [input_union inputs] queries each input source until one of them produces
      a value. *)

  (** [with_boot_and_tick ~boot ~tick ~tick_every ~input] is a convenience
      wrapper returning an input source and a boot-time command commonly used by
      small Tempo applications. *)

  (** [run program] executes a message-driven application.

      Example:
      {[
        type msg = Boot | Tick
        type model = { ticks : int }

        let input = Tempo_app.App.boot_once_input ~boot:Boot (fun () -> None)

        let update model = function
          | `Input Boot -> (model, Tempo_app.App.every_n 1 Tick)
          | `Input Tick -> ({ ticks = model.ticks + 1 }, Tempo_app.App.none)
          | `Message _ -> (model, Tempo_app.App.none)

        let () =
          Tempo_app.App.run ~instants:10 ~input
            { Tempo_app.App.init = { ticks = 0 }; update }
      ]} *)

  (** [run_with_view] is the same execution model as {!val:run}, with an extra
      pure projection from the current model to an output value. *)
end

module Loop : sig
  include module type of Tempo.Loop

  (** [run config] executes a deterministic state loop.

      At each logical instant the loop:
      - reads [config.input ()],
      - calls [config.step] with the current state and optional input,
      - emits an output when [step] returns [Some output],
      - then pauses until the next instant.

      This is useful when {!module:App} would be too structured and a single
      ticked reducer is enough. *)
end

module Scene : sig
  include module type of Tempo.Scene

  (** [create ~initial ~on_enter ~on_exit] creates a scene controller.

      Scene transitions are explicit: callers request a change with
      {!val:request}, and {!val:process} applies the transition by calling
      [on_exit] on the old scene then [on_enter] on the new one. *)

  (** [request controller scene] asks for a transition to [scene]. *)

  (** [current controller] returns the currently active scene. *)

  (** [process controller] applies pending transitions. It should typically run
      in its own Tempo process or be called regularly by the surrounding
      application loop. *)
end
