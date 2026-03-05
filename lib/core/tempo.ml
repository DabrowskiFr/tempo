include Tempo_base
include Tempo_core_api
include Tempo_constructs

type 'input interactive_source = {
  poll : unit -> 'input option;
  wait : unit -> unit;
}

type wakeup = Tempo_engine.wakeup

let current_wakeup = Tempo_engine.current_wakeup
let notify_wakeup = Tempo_engine.notify_wakeup
let register_wakeup_poller = Tempo_engine.register_wakeup_poller
let emit_from_host = Tempo_engine.emit_from_host
let execute = Tempo_engine.execute

let run_interactive ?output ~input process =
  let input' : 'a Tempo_engine.interactive_source =
    { poll = input.poll; wait = input.wait }
  in
  Tempo_engine.run_interactive ?output ~input:input' process

module Core = Tempo_core_api
module Constructs = Tempo_constructs
module Observe = Tempo_observe
module Meta = Tempo_meta
module Low_level = Tempo_low_level
