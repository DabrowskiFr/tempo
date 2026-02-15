# Solar System Raylib (Tempo)

Port de `examples/solar_system_tsdl.ml` en Raylib.

Features Tempo utilisees:
- API `state` pour integrer les vitesses angulaires
- signaux evenementiels simples pour le toggle pause/reprise

Controles:
- `SPACE` ou `P`: pause/reprise
- `ESC`: quitter

Build:
```bash
cd /Users/fredericdabrowski/Repos/tempo/applications/games/solar-system-raylib
dune build
dune exec ./main.exe
```
