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

## 3. Architecture API: Core vs Layer2
Le contrat de la librairie est maintenant explicite:
- **Core** = sémantique synchrone minimale.
- **Layer2** = abstractions construites sur Core.

### 3.1 Tableau de séparation
| Couche | Modules / primitives | Rôle |
|---|---|---|
| `Tempo.Core` | `execute`, `new_signal`, `new_signal_agg`, `emit`, `await`, `await_immediate`, `pause`, `when_`, `watch`, `parallel` | Noyau sémantique synchrone |
| `Tempo` (top-level) | mêmes primitives aussi exposées au niveau racine | Compatibilité + ergonomie |
| `Tempo.Layer2` | `Game`, `App`, `Reactive`, `Loop`, `Scene`, `Resource`, `Event_bus`, etc. | Outils d'ingénierie basés sur Core |

### 3.2 API Core (référence minimale)
- `Tempo.Core.execute`
- `Tempo.Core.new_signal`
- `Tempo.Core.new_signal_agg`
- `Tempo.Core.emit`
- `Tempo.Core.await`
- `Tempo.Core.await_immediate`
- `Tempo.Core.pause`
- `Tempo.Core.when_`
- `Tempo.Core.watch`
- `Tempo.Core.parallel`

### 3.3 API Layer2 (exemples importants)
- `Tempo.App.every_n` : tick périodique en architecture model/update.
- `Tempo.App.tick_every` : alias intentionnel pour un tick périodique.
- `Tempo.App.tick_if` : tick périodique conditionnel.
- `Tempo.App.command_if` : activer/désactiver une commande sans boilerplate.
- `Tempo.App.command_when` : choisir entre deux commandes selon une condition.
- `Tempo.App.boot_once_input` : injecter un message de boot unique.
- `Tempo.App.with_boot_and_tick` : setup standard boot + tick périodique.
- `Tempo.App.input_union` : fusion de plusieurs sources d'input.
- `Tempo.Reactive.rising_edge` : détecter un front montant.
- `Tempo.Reactive.falling_edge` : détecter un front descendant.
- `Tempo.Reactive.edge_by` : détecter une transition custom.
- `Tempo.Reactive.hold_last` : mémoriser la dernière valeur d'un signal.
- `Tempo.Reactive.sample_on` : échantillonner un état sur trigger.
- `Tempo.Reactive.toggle_on` : état booléen qui bascule à chaque événement.
- `Tempo.Reactive.pulse_n` : signal périodique de pulse.
- `Tempo.Reactive.supervise_until` : supervision structurée jusqu'à un signal d'arrêt.
- `Tempo.Game.after_n/every_n/timeout/cooldown` : temporalité gameplay.
- `Tempo.State.create/get/set/modify/await/update_and_get` : état synchrone encapsulé (couche 2).
- `Tempo.Timeline_json.of_timeline_with` : export JSON avec paramètres nommés et serializers explicites.

### 3.4 Exemple minimal Core
```ocaml
open Tempo

let () =
  Core.execute (fun input output ->
    let rec loop () =
      Core.when_ input (fun () ->
        let v = Core.await_immediate input in
        Core.emit output (v + 1));
      Core.pause ();
      loop ()
    in
    loop ())
```

### 3.5 Exemple minimal Layer2 (`App`)
```ocaml
open Tempo

type msg = Boot | Tick
type model = { ticks : int }

let update m = function
  | Boot -> (m, App.every_n 1 Tick)
  | Tick -> ({ ticks = m.ticks + 1 }, App.none)

let () =
  let input = App.boot_once_input ~boot:Boot (fun () -> None) in
  App.run ~instants:10 ~input { App.init = { ticks = 0 }; update }
```

### 3.6 Exemple Layer2: fusion input + edges + sampling
```ocaml
open Tempo

let key_input () = None
let mouse_input () = None

let merged = App.input_union [ key_input; mouse_input ]

let main down_level _output =
  let down_edge = Core.new_signal () in
  let released_edge = Core.new_signal () in
  let is_down = Reactive.hold_last false down_level in
  let sampled = Reactive.sample_on is_down down_edge in
  let _ = sampled in
  Reactive.rising_edge (fun b -> b) down_level down_edge;
  Reactive.falling_edge (fun b -> b) down_level released_edge
```

### 3.7 Exemple Layer2: `with_boot_and_tick` + `command_if`
```ocaml
open Tempo

type msg = Boot | Tick | Pause
type model = { running : bool; ticks : int }

let boot_input, boot_tick_cmd =
  App.with_boot_and_tick ~boot:Boot ~tick:Tick ~tick_every:1
    ~input:(fun () -> None)

let update m = function
  | Boot -> (m, App.command_if m.running boot_tick_cmd)
  | Tick -> ({ m with ticks = m.ticks + 1 }, App.none)
  | Pause ->
      let m' = { m with running = not m.running } in
      (m', App.command_if m'.running (App.tick_every 1 ~tick:Tick))

let () =
  let program = { App.init = { running = true; ticks = 0 }; update } in
  App.run ~instants:10 ~input:boot_input program
```

### 3.8 Exemple Layer2: `State` + timeline JSON typée
```ocaml
open Tempo

let program input output =
  let counter = State.create 0 in
  let rec loop () =
    when_ input (fun () ->
      let v = await_immediate input in
      let n = State.update_and_get counter (fun x -> x + v) in
      emit output n);
    pause ();
    loop ()
  in
  loop ()

let () =
  let timeline = execute_timeline ~inputs:[ Some 1; None; Some 2 ] program in
  let _json =
    Timeline_json.of_timeline_with
      ~input_to_string:string_of_int
      ~output_to_string:string_of_int
      timeline
  in
  ()
```

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
dune exec ./bench/basic_bench.exe
./tools/bench/run_core_bench.sh --out-dir bench/results/core
./tools/bench/run_core_bench.sh --sweep 0.5,1,2 --log-y --out-dir bench/results/core-sweep
./tools/bench/run_core_bench.sh --out-dir bench/results/core-candidate \
  --baseline-csv bench/results/core/core_bench.csv \
  --fail-on-regression-pct 5
```

Le bench core parametrique genere automatiquement:
- CSV des mesures
- graphes SVG par primitive
- graphe SVG combine (cout normalise)
- rapport Markdown

Details: `bench/README_CORE_BENCH.md`

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

## 9. Exemple minimal (Core)
```ocaml
open Tempo

let () =
  Core.execute (fun input output ->
    let rec loop tick =
      Core.when_ input (fun () ->
        let v = Core.await_immediate input in
        Core.emit output (tick + v));
      Core.pause ();
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
- utiliser `execute_timeline` + `Timeline_json.of_timeline_with` pour débogage déterministe;
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
- Exemple jeu utilisant Tempo: `applications/advanced/game-univ/`
- Architecture applications: `applications/ARCHITECTURE.md`
- Matrice smoke release: `applications/SMOKE_MATRIX.md`
- Politique de compatibilité: `docs/COMPATIBILITY_POLICY.md`
- Notes de release/migration: `RELEASE_NOTES.md`

## 16. Lancer les applications depuis la racine (sans `cd`, sans `.exe`)
Depuis `/Users/fredericdabrowski/Repos/tempo`.

Méthode recommandée (lanceur unifié):

```bash
dune exec ./applications/run -- help
dune exec ./applications/run -- <app>
```

Identifiants `<app>` disponibles:

```text
game-univ
boids-raylib
ca-continuous-raylib
lenia-raylib
snake-raylib
solar-system-raylib
logicgroove
temporalsim
refactor
```

Exemples:

```bash
dune exec ./applications/run -- game-univ
dune exec ./applications/run -- temporalsim
dune exec ./applications/run -- refactor
```

Alternative (lancement direct d'un jeu):

```bash
dune exec ./applications/advanced/game-univ/run
dune exec ./applications/simple-demos/boids-raylib/run
dune exec ./applications/simple-demos/ca-continuous-raylib/run
dune exec ./applications/simple-demos/lenia-raylib/run
dune exec ./applications/simple-demos/snake-raylib/run
dune exec ./applications/simple-demos/solar-system-raylib/run
dune exec ./applications/simple-demos/logicgroove/run
dune exec ./applications/simple-demos/temporalsim/run
dune exec ./applications/advanced/refactor/run
```

## 17. Validation pre-release des applications
Depuis `/Users/fredericdabrowski/Repos/tempo`.

Commande officielle de gate release:

```bash
./tools/release/validate_apps.sh
```

Le script verifie:
- routeurs de lancement;
- presence/structure des README applications;
- build des executables applications;
- contrat `run --help` de chaque application;
- smoke test headless de `refactor`.

## 18. Enregistrer des videos de demo (macOS)
Depuis `/Users/fredericdabrowski/Repos/tempo`.

Prerequis:

```bash
brew install ffmpeg
```

Lister les peripheriques AVFoundation (index video/audio):

```bash
./tools/demo/list_avfoundation_devices_macos.sh
```

Si aucun device video n'apparait, autoriser d'abord l'app terminal dans:
`System Settings -> Privacy & Security -> Screen Recording`, puis relancer le terminal.

Enregistrer automatiquement une session (lance le jeu + capture):

```bash
./tools/demo/run_and_record_macos.sh game-univ 30 demos/game-univ-demo.mp4 "1:none" 30 8
```

Si la video est noire:
- verifier que l'index device est correct (`list_avfoundation_devices_macos.sh`);
- augmenter `startup_delay_sec` (ex: `12` ou `15`);
- lancer une premiere fois le jeu pour chauffer le build Dune.

Compresser une video:

```bash
./tools/demo/compress_mp4.sh demos/game-univ-demo.mp4 demos/game-univ-demo-small.mp4
```
