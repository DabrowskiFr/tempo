# Score Player Raylib

This demo is a Tempo core showcase:

- one logical instant corresponds to the smallest rhythmic unit
- notes are held across multiple instants by dedicated Tempo processes
- `run_interactive` drives the playback with a host-side metronome
- FluidSynth imports MIDI and renders a SoundFont-backed sound

Run it with:

```sh
dune exec ./applications/simple-demos/score-player-raylib/src/main.exe
```

You may optionally pass a MIDI file:

```sh
dune exec ./applications/simple-demos/score-player-raylib/src/main.exe -- /path/to/file.mid
```
