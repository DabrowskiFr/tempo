# ca-continuous-raylib

## Overview

Port Raylib de `examples/ca_continuous_tsdl.ml`.
Automate cellulaire continu de type réaction-diffusion (Gray-Scott-like) avec:

- un comportement Tempo par cellule,
- communication via signaux,
- rendu colorimétrique dynamique.

## Controls / Inputs

- `Q` ou `ESC`: quitter

## Launch Commands

Depuis la racine du dépôt:

```bash
sh applications/simple-demos/ca-continuous-raylib/run
```

Direct:

```bash
cd applications/simple-demos/ca-continuous-raylib
dune exec ./src/main.exe --
```

## Dependencies

- OCaml 5.x, dune
- `tempo`, `tempo-raylib`, `raylib`
