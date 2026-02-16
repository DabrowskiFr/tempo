# tempo-core-studio

## Overview

Application avancée de type Scratch orientée exclusivement sur le core Tempo. Elle permet de construire un programme hiérarchique par blocs (`emit`, `await`, `await_immediate`, `pause`, `when`, `watch`, `parallel`) avec des sous-corps (`body1`, `body2`) pour les blocs conteneurs, d'injecter des signaux d'entrée par instant, puis d'observer la timeline résultante de manière déterministe.

## Controls / Inputs

- Mouse:
- cliquer un bloc dans la palette pour l'ajouter au programme;
- sélectionner une ligne dans `Program Tree` pour définir la cible contextuelle d'insertion;
- le nœud implicite `main` est sélectionnable pour revenir au niveau le plus haut;
- l'arbre affiche une pastille couleur (`red`/`blue`) sur les blocs qui portent un signal;
- un clic palette insère dans le bloc sélectionné: dans `body1/body2` pour les conteneurs, sinon juste après le bloc sélectionné;
- sélectionner un bloc (hors `main`) puis éditer son `kind` et son signal dans `Selected block editor`;
- cliquer `X` sur une ligne pour supprimer un bloc;
- cliquer les sélecteurs `red` et `blue` de chaque instant (les deux peuvent être actifs simultanément).
- Buttons:
- `Run Simulation`: exécute le programme sur les instants configurés;
- `Clear Program`: vide le script;
- `Clear Inputs`: remet les entrées à vide;
- `Load Sample Program`: charge un scénario exemple.
- Keyboard:
- `Esc`: fermer la fenêtre.

## Launch Commands

Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/run -- tempo-core-studio
```

Lancement direct:

```bash
dune exec ./applications/advanced/tempo-core-studio/run
```

## CLI Options

- `--headless`: exécute un scénario déterministe sans UI et affiche la timeline.
- `--instants <n>`: nombre d'instants logiques (défaut: `16`, minimum: `4`).

Exemple:

```bash
dune exec ./applications/run -- tempo-core-studio -- --headless --instants 12
```

## Dependencies

- OCaml 5.x, dune
- raylib (mode UI)
- tempo

## Headless / Reproducible Mode

- Mode headless supporté via `--headless`.
- Le mode headless est déterministe (pas d'aléatoire).
- Utile pour valider rapidement la sémantique du programme core sans interface graphique.

## Troubleshooting

- Si la fenêtre ne s'affiche pas: vérifier l'installation de `raylib`.
- Si la simulation semble bloquée: vérifier que le script contient au moins un bloc qui peut progresser sans signal externe (ex: `emit` ou `pause`).

## Release Status

- Category: `advanced`
- Quality status: `ready`
- Last validation date: 2026-02-16
- Validated commands:
- `dune exec ./applications/run -- tempo-core-studio`
- `dune exec ./applications/run -- tempo-core-studio -- --headless --instants 12`
