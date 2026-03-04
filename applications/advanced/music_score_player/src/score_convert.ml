let usage () =
  prerr_endline
    "Usage:\n  dune exec \
     ./applications/advanced/music_score_player/src/score_convert.exe -- <input.mid|input.midi|input.tscore> <output.tscore>";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | [ _; input_path; output_path ] ->
      let score =
        if Filename.check_suffix input_path ".mid"
           || Filename.check_suffix input_path ".midi"
        then Tempo_score.of_midi_file input_path
        else Tempo_score.of_text_file input_path
      in
      Tempo_score.write_text_file ~path:output_path score;
      Printf.printf "Written %s\n%!" output_path
  | _ -> usage ()
