open Tempo

let () =
  let run () =
    execute_trace ~instants:10 ~inputs:[ None ] (fun _input output ->
        let kill = Low_level.new_kill () in
        let parent_steps = new_state 0 in
        let child_steps = new_state 0 in
        let parent_continued = new_state false in
        let parent () =
          Low_level.with_kill kill (fun () ->
              modify_state parent_steps (fun x -> x + 1);
              let _ =
                Low_level.fork (fun () ->
                    modify_state child_steps (fun x -> x + 1);
                    pause ();
                    modify_state child_steps (fun x -> x + 1000))
              in
              pause ();
              modify_state parent_steps (fun x -> x + 1000));
          set_state parent_continued true
        in
        let killer () = Low_level.abort_kill kill in
        parallel [ parent; killer ];
        emit output (get_state parent_steps, get_state child_steps, get_state parent_continued))
  in
  let a = run () in
  let b = run () in
  match (a, b) with
  | [ (p1, c1, k1) ], [ (p2, c2, k2) ] ->
      Printf.printf "tag=low_level;a=(%d,%d,%b);b=(%d,%d,%b);eq=%b\n" p1 c1 k1 p2 c2 k2 (a = b)
  | _ -> Printf.printf "unexpected\n"
