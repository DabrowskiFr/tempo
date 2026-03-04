open Tempo

let () =
  let p = Profiler.create () in
  Profiler.measure p ~name:"100k_inc" (fun () ->
      let st = ref 0 in
      for _ = 1 to 100_000 do
        incr st
      done);
  List.iter
    (fun s ->
      Printf.printf "%s: %Ld ns\n" s.Profiler.name s.duration_ns)
    (Profiler.snapshot p)
