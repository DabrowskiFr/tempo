# tempo-core-studio

## Overview

Application avancée de type Scratch orientée exclusivement sur le core Tempo. Elle permet de construire un programme hiérarchique par blocs (`emit`, `await`, `await_immediate`, `pause`, `when`, `watch`, `parallel`) avec des sous-corps (`body1`, `body2`) pour les blocs conteneurs, d'injecter des signaux d'entrée par instant, puis d'observer la timeline résultante de manière déterministe.

## Controls / Inputs

- Mouse:
- cliquer un bloc dans la palette pour l'ajouter au programme;
- choisir la cible d'insertion (`Main`, `Body1`, `Body2`) dans le panneau `Actions`;
- sélectionner un bloc dans l'arbre puis éditer son `kind` et son signal dans `Selected block editor`;
- cliquer `X` sur une ligne pour supprimer un bloc;
- cliquer une cellule de timeline pour cycler l'entrée `- -> A -> B`.
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
