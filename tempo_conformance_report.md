# Tempo Conformance Report

Date: 2026-02-15

Référence: `/Users/fredericdabrowski/Repos/tempo/guideline.md`

Légende statut:
- `OK`: conforme aux principes Tempo-first sur les points essentiels.
- `PARTIEL`: base correcte, mais migration recommandée.
- `A MIGRER`: écart notable par rapport au modèle cible.

## Applications game

| Application | Statut | Constat | Actions restantes |
|---|---|---|---|
| `game-univ` | OK | Logique coeur en Tempo, audio piloté par commandes émises côté Tempo, adapter Raylib majoritairement I/O. | Continuer à extraire les derniers patterns UI communs dans `tempo-raylib` si duplication future. |
| `snake-raylib` | PARTIEL | Logique en `Tempo.App` correcte, intégration `tempo-raylib` commencée (HUD/UI/audio). | Optionnel: passer à un pipeline Tempo explicite (`input/sim/render`) pour homogénéité avec les autres jeux. |
| `time-echo-jumper` | PARTIEL | Refactor single-owner fait (plus de partage mutable inter-processus). | Migrer davantage de primitives HUD/FX/audio vers `tempo-raylib`. |
| `temporal-arena` | PARTIEL | Refactor single-owner fait; plus de pattern multi-processus mutable. | Migrer UI/HUD/FX/audio vers `tempo-raylib`; structurer les événements internes en signaux dédiés. |

## Applications simulation

| Application | Statut | Constat | Actions restantes |
|---|---|---|---|
| `boids-raylib` | PARTIEL | Refactor owner unique effectué; intégration HUD/audio `tempo-raylib` ajoutée. | Étendre encore `tempo-raylib` pour FX visuels dédiés si nécessaire. |
| `ca-continuous-raylib` | A MIGRER | Architecture parallèle + état mutable partagé probable via `new_state`. | Passer en owner clair par état ou reducer unique; intégrer briques `tempo-raylib`. |
| `solar-system-raylib` | A MIGRER | Modèle parallèle fonctionnel, mais intégration `tempo-raylib` absente. | Aligner UI/HUD avec `tempo-raylib`; clarifier ownership des états temporels. |
| `temporal-physics-sandbox` | A MIGRER | Processus parallèles avec état global; Raylib direct. | Refactor vers ownership explicite + adoption `tempo-raylib` pour composants partagés. |
| `emergent-city-lab` | A MIGRER | Event bus présent, mais architecture encore orientée états partagés/parallel. | Consolider en reducer propriétaire + flux d’événements Tempo stricts; UI via `tempo-raylib`. |

## Applications logic

| Application | Statut | Constat | Actions restantes |
|---|---|---|---|
| `logicgroove` | A MIGRER | Usage parallel + `new_state`; architecture valide fonctionnellement mais peu alignée avec cible stricte ownership/signaux. | Clarifier ownership de l’état et réduire coordination implicite entre processus. |
| `logicgroove-evolution` | A MIGRER | Même pattern: parallel + états partagés. | Refactor progressif vers single-owner/reducer et signaux explicites. |
| `temporalsim` | A MIGRER | Pipeline parallèle avec état central mutable. | Migrer vers modèle owner clair et échanges signalés explicites. |

## Priorités de migration recommandées

1. Simulation: `boids-raylib`, `ca-continuous-raylib`, `temporal-physics-sandbox`
2. Logic: `logicgroove`, `logicgroove-evolution`, `temporalsim`
3. Harmonisation `tempo-raylib` transversale sur apps restantes

## Critères de sortie (Done)

- Pas de communication inter-processus via références partagées.
- Ownership explicite des états (single-owner ou découpage strict).
- Logique temporelle métier côté Tempo.
- `tempo-raylib` utilisé pour les briques techniques communes.
- Build ciblé + `dune runtest` passent.
