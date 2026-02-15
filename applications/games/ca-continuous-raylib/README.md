# Continuous CA Raylib (Tempo)

Port de `examples/ca_continuous_tsdl.ml` en Raylib.

Features Tempo utilisees:
- `Game.every_n` pour injecter periodiquement des perturbations
- API `state` pour la grille

Build:
```bash
cd /Users/fredericdabrowski/Repos/tempo/applications/games/ca-continuous-raylib
dune build
dune exec ./main.exe
```
