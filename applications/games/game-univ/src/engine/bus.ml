open Types

type t = {
  input : input_state Tempo.signal;
  draw : (draw_cmd list, draw_cmd list) Tempo.agg_signal;
  evt : (event list, event list) Tempo.agg_signal;
  output : frame Tempo.signal;
}

let create ~input ~output =
  {
    input;
    draw = Tempo.new_signal_agg ~initial:[] ~combine:(fun acc cmds -> cmds @ acc);
    evt = Tempo.new_signal_agg ~initial:[] ~combine:(fun acc evs -> evs @ acc);
    output;
  }
