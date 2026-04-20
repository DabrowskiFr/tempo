# Music Score Player

Advanced music demo for Tempo.

This is the showcase version of the score player:
- interactive logical timeline driven by `Tempo.run_interactive`
- notes held by Tempo processes across multiple instants
- sustain pedal (`CC64`) scheduled by Tempo from MIDI/text score events
- piano controls (`CC64`, `CC66`, `CC67`) scheduled by Tempo
- tempo map events (`tempo_change`) applied on the logical timeline
- MIDI import through `tempo-fluidsynth`
- Tempo binary score import (`.tscore`)
- SoundFont rendering through FluidSynth

Assets layout:
- `assets/mid/` stores source MIDI files
- `assets/txt/` stores editable Tempo textual scores (`.txt`)
- `assets/tscore/` stores binary Tempo scores loaded by the app (`.tscore`)
- `assets/soundfonts/` stores `.sf2` / `.sf3` banks used for playback comparison
- tracked by git: `GeneralUser-GS.sf2` only
- local optional (not tracked): `MuseScore_General.sf2` for higher-quality piano

Run it with:

```sh
dune exec ./applications/advanced/music_score_player/src/main.exe
```

Score selection UI:
- always-visible score list panel at the bottom
- click to select a piece
- mouse wheel and scrollbar supported when the list is longer than the visible area

## CLI tools

Two CLI tools are provided in `src/`:

- `score_convert.exe`: convert between MIDI, textual score and binary score
- `score_show_text.exe`: print any score as textual `tempo-score v2`
- textual format now uses musical-oriented headers/positions:
  - `tempo: q=<bpm>` (quarter note reference)
  - `subdivision: 1/N` (musical grid, no MIDI tick leak)
  - optional `tempo_change bar:<m> step:<k> q:<bpm>`
  - `note bar:<m> step:<k> dur:<u> ...`
  - `control bar:<m> step:<k> ch:<c> cc:<n> val:<v>`

### `score_convert.exe`

```sh
dune exec ./applications/advanced/music_score_player/src/score_convert.exe -- \
  <input> <output>
```

Conversion behavior is based on file extensions.

Input:
- `.mid` / `.midi` => MIDI import
- `.txt` / `.tempo-score` => textual score import
- `.tscore` (or legacy `.tbin`) => binary score import

Output:
- `.txt` / `.tempo-score` => textual score export
- `.tscore` => binary score export

Examples:

```sh
# MIDI -> text (.txt)
dune exec ./applications/advanced/music_score_player/src/score_convert.exe -- \
  applications/advanced/music_score_player/assets/mid/beethoven_op2no3_1.mid \
  applications/advanced/music_score_player/assets/txt/beethoven_op2no3_1.txt

# text -> binary (.tscore)
dune exec ./applications/advanced/music_score_player/src/score_convert.exe -- \
  applications/advanced/music_score_player/assets/txt/beethoven_op2no3_1.txt \
  applications/advanced/music_score_player/assets/tscore/beethoven_op2no3_1.tscore

# binary -> text
dune exec ./applications/advanced/music_score_player/src/score_convert.exe -- \
  applications/advanced/music_score_player/assets/tscore/beethoven_op2no3_1.tscore \
  applications/advanced/music_score_player/assets/txt/beethoven_op2no3_1.roundtrip.txt
```

### `score_show_text.exe`

```sh
dune exec ./applications/advanced/music_score_player/src/score_show_text.exe -- \
  <input.mid|input.tscore|input.txt>
```

Example:

```sh
dune exec ./applications/advanced/music_score_player/src/score_show_text.exe -- \
  applications/advanced/music_score_player/assets/tscore/piano_sonata_13_1_(c)oguri.tscore
```

SoundFont comparison in-app:
- drop several `.sf2`/`.sf3` files in `assets/soundfonts/`
- run the app on the same score
- switch SoundFont live with `F5` / `F6` (same timeline, same notes)
- default selection order: `GeneralUser-GS.sf2` (startup stability), then `MuseScore_General.sf2`
