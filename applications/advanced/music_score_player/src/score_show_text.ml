let usage () =
  prerr_endline
    "Usage:\n  dune exec \
     ./applications/advanced/music_score_player/src/score_show_text.exe -- <input.tscore|input.txt|input.mid>";
  exit 2

let load_score path =
  if Filename.check_suffix path ".mid" || Filename.check_suffix path ".midi"
  then Tempo_score.of_midi_file path
  else if Filename.check_suffix path ".txt" || Filename.check_suffix path ".tempo-score"
  then Tempo_score.of_text_file path
  else Tempo_score.of_binary_file path

let () =
  match Array.to_list Sys.argv with
  | [ _; input_path ] ->
      let score = load_score input_path in
      print_string (Tempo_score.to_text score);
      print_newline ()
  | _ -> usage ()
