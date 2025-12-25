open Tempo 

let worker s () = 
  let _ = await s in 
    Format.printf "done"

let driver s () = 
    emit s 1 

let scenario _ _  = 
  let s =
    new_signal_agg
      ~initial:0
      ~combine:(fun acc payload -> payload + acc)
  in parallel [driver s; worker s]

let _ = execute scenario