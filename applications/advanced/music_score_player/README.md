# Music Score Player

Advanced music demo for Tempo.

This is the showcase version of the score player:
- interactive logical timeline driven by `Tempo.run_interactive`
- notes held by Tempo processes across multiple instants
- MIDI import through `tempo-fluidsynth`
- Tempo textual score import (`.tscore`, `.tempo-score`)
- SoundFont rendering through FluidSynth

Assets layout:
- `assets/mid/` stores source MIDI files
- `assets/tscore/` stores Tempo textual scores loaded by the app

Run it with:

```sh
dune exec ./applications/advanced/music_score_player/src/main.exe
```

Convert MIDI to Tempo textual score:

```sh
dune exec ./applications/advanced/music_score_player/src/score_convert.exe -- \
  applications/advanced/music_score_player/assets/mid/beethoven_op2no3_1.mid \
  applications/advanced/music_score_player/assets/tscore/beethoven_op2no3_1.tscore
```
