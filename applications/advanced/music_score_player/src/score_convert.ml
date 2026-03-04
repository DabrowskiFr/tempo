let usage () =
  prerr_endline
    "Usage:\n  dune exec \
     ./applications/advanced/music_score_player/src/score_convert.exe -- <input.mid|input.midi|input.txt|input.tscore> <output.txt|output.tscore>";
  exit 2

let load_score path =
  if Filename.check_suffix path ".mid" || Filename.check_suffix path ".midi"
  then Tempo_score.of_midi_file path
  else if
    Filename.check_suffix path ".txt"
    || Filename.check_suffix path ".tempo-score"
  then Tempo_score.of_text_file path
  else if
    Filename.check_suffix path ".tscore"
    || Filename.check_suffix path ".tempo-scoreb"
    || Filename.check_suffix path ".tbin"
  then Tempo_score.of_binary_file path
  else Tempo_score.of_binary_file path

let save_score path score =
  if Filename.check_suffix path ".txt" || Filename.check_suffix path ".tempo-score"
  then Tempo_score.write_text_file ~path score
  else if Filename.check_suffix path ".tscore" || Filename.check_suffix path ".tempo-scoreb"
  then Tempo_score.write_binary_file ~path score
  else Tempo_score.write_binary_file ~path score

let () =
  match Array.to_list Sys.argv with
  | [ _; input_path; output_path ] ->
      let score = load_score input_path in
      save_score output_path score;
      Printf.printf "Written %s\n%!" output_path
  | _ -> usage ()
