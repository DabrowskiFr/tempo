# Tempo

## 1. Vue d'ensemble
Tempo est une librairie OCaml de programmation réactive synchrone basée sur les effets algébriques (OCaml 5). Elle exécute des processus concurrents sur une suite d'**instants logiques**.

Idée centrale:
- un programme progresse instant par instant;
- les signaux émis pendant un instant sont observables selon les primitives (`await`, `await_immediate`, `when_`, `watch`);
- l'absence d'un signal est décidée à la fin de l'instant, ce qui favorise un comportement déterministe.

## 2. Modèle d'exécution
### 2.1 Instants
Un instant représente une "frame logique" de calcul synchrone.

### 2.2 Signaux
Deux familles de signaux:
- `new_signal ()` : signal événementiel (au plus une émission par instant)
- `new_signal_agg ~initial ~combine` : signal agrégé (plusieurs émissions possibles, agrégées via `combine`)

### 2.3 Primitives haut niveau
- `emit s v`
- `await s`
- `await_immediate s`
- `pause ()`
- `parallel [ ... ]`
- `when_ g body`
- `watch s body`
- `present_then_else s t e`

### 2.4 Exécution
- `execute ?instants ?input ?output main`
- `execute_trace ~inputs main` : capture des sorties
- `execute_timeline ~inputs main` : trace par instant (input/output)
- `execute_inspect ~on_instant ...` : inspection runtime (tasks/signaux)

## 3. Modules API principaux
### 3.1 Noyau
- `Tempo` : primitives synchrones, signaux, exécution
- `Tempo.Low_level` : fork/join/kill (usage avancé)

### 3.2 Outils état/processus
- `new_state/get_state/set_state/modify_state/await_state`
- `Tempo.Dynamic` : `spawn/stop/join/spawn_many`

### 3.3 Helpers gameplay
- `Tempo.Game.after_n`
- `Tempo.Game.every_n`
- `Tempo.Game.timeout`
- `Tempo.Game.cooldown`

### 3.4 Modules game-dev ajoutés
- `Tempo.Scene` : transitions de scènes
- `Tempo.Resource` : cycle de vie ressources
- `Tempo.Input_map` : mapping actions
- `Tempo.Event_bus` : bus événementiel typé
- `Tempo.Fixed_step` : boucle fixe / interpolation
- `Tempo.Rng` : RNG déterministe
- `Tempo.Netcode` : snapshots frame-tagged + rollback sur `state`
- `Tempo.Profiler` : mesure temps d'exécution
- `Tempo.Tick_tags` : tags d'instants
- `Tempo.Runtime_snapshot` : capture/restore d'état runtime
- `Tempo.Error_bus` : exécution sécurisée et remontée d'erreurs
- `Tempo.Timeline_json` : export JSON de timeline
- `Tempo.Dev_hud` : rendu texte d'un snapshot d'inspection
- `Tempo.Entity_set` : spawn/despawn/broadcast d'entités dynamiques

## 4. Package `tempo.game`
En plus de `tempo`, le repo expose la librairie `tempo.game` (module `Tempo_game`) qui ré-exporte les modules orientés jeu:
- `Scene`, `Resource`, `Input_map`, `Event_bus`, `Fixed_step`, `Rng`, `Netcode`, `Profiler`, `Tick_tags`, `Runtime_snapshot`, `Entity_set`, `Dev_hud`, `Error_bus`, `Timeline_json`.

## 5. Dépendances
### 5.1 Dépendances de la librairie Tempo
- OCaml >= 5.3.0
- dune >= 3.19
- logs >= 0.10.0
- mtime >= 2.1.0

### 5.2 Dépendances optionnelles pour un jeu graphique (exemple)
Tempo n'impose pas de moteur graphique. Pour un jeu 2D type `game-univ`, vous pouvez ajouter:
- `raylib` (binding OCaml)
- éventuellement audio/assets selon votre stack

## 6. Installation
### 6.1 Depuis ce dépôt (pin local)
```bash
opam switch create 5.4.0   # ou 5.3.x+
eval $(opam env)
opam install dune logs mtime
opam pin add tempo /Users/fredericdabrowski/Repos/tempo
```

### 6.2 Depuis GitHub
```bash
opam pin add tempo https://github.com/DabrowskiFr/tempo.git
```

## 7. Build, tests, bench, docs
### 7.1 Build
```bash
cd /Users/fredericdabrowski/Repos/tempo
dune build
```

### 7.2 Tests
```bash
dune runtest
```

### 7.3 Benchmarks
```bash
dune exec bench/basic_bench.exe
```

### 7.4 Documentation du repo
- Quickstart game: `docs/GAME_QUICKSTART.md`
- README global: `README.md`

## 8. Utilisation dans un projet Dune
Exemple `dune`:
```lisp
(executable
 (name main)
 (libraries tempo))
```

Si vous utilisez les modules orientés jeu:
```lisp
(executable
 (name main)
 (libraries tempo tempo.game))
```

Dans votre code:
```ocaml
open Tempo
```

## 9. Exemple minimal
```ocaml
open Tempo

let () =
  execute (fun input output ->
    let rec loop tick =
      when_ input (fun () ->
        let v = await_immediate input in
        emit output (tick + v));
      pause ();
      loop (tick + 1)
    in
    loop 0)
```

## 10. Pattern recommandé pour un jeu
Architecture type:
- un adaptateur plateforme (raylib/headless/etc.)
- `execute ~input ~output`
- processus Tempo parallèles:
  - input mapping
  - gameplay/entités
  - UI/HUD
  - audio/events

Bonnes pratiques:
- garder la logique gameplay dans Tempo, pas dans l'adaptateur rendu;
- utiliser `Event_bus` pour découpler gameplay/UI/audio;
- utiliser `execute_timeline` + `Timeline_json` pour débogage déterministe;
- utiliser `Dynamic`/`Entity_set` pour spawn/despawn runtime.

## 11. Gestion des erreurs
- Encapsuler du code non fiable avec `Error_bus.safe`.
- En mode global, utiliser `Error_bus.execute_safe` pour publier les erreurs sur un signal dédié au lieu de faire tomber tout le runtime.

## 12. Inspection et debug runtime
- `execute_inspect` fournit à chaque instant:
  - `instant`
  - `current_tasks`
  - `blocked_tasks`
  - `next_tasks`
  - `signal_count`
- `Dev_hud.to_string` transforme ce snapshot en texte affichable in-game.

## 13. Version/API
- `Tempo.version_string`
- `Tempo.api_level`
- `Tempo.require_api_level n`

## 14. Limites actuelles (v1)
- Les snapshots runtime s'appuient sur les `state` enregistrés (pas une sérialisation complète du scheduler).
- `tempo.game` est une couche utilitaire orientée pratique; certaines API pourront évoluer.

## 15. Références dans ce repo
- API publique: `lib/tempo.mli`
- Implémentation: `lib/tempo.ml`
- Module game re-export: `lib/tempo_game.mli`, `lib/tempo_game.ml`
- Tests: `tests/ok`
- Exemple jeu utilisant Tempo: `applications/games/game-univ/`

## 16. Lancer les jeux depuis la racine (sans `cd`, sans `.exe`)
Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/games/boids-raylib/run
dune exec ./applications/games/ca-continuous-raylib/run
dune exec ./applications/games/snake-raylib/run
dune exec ./applications/games/solar-system-raylib/run
dune exec ./applications/games/game-univ/run
```
