# Tempo

## 1. Vue d'ensemble

Tempo est une librairie OCaml de programmation réactive synchrone basée sur des instants logiques.

Le coeur du modèle est simple :
- les processus communiquent par signaux ;
- un instant regroupe une réaction synchrone complète ;
- l'absence d'un signal n'est décidée qu'à la fin de l'instant ;
- la concurrence reste déterministe tant qu'on reste dans ce cadre.

## 2. Surface principale

### 2.1 Noyau recommandé

La surface recommandée est :
- `Tempo.Core` pour les primitives synchrones ;
- `Tempo.Constructs` pour les opérateurs dérivés ;
- `Tempo.Observe` pour les outils d'observation ;
- `Tempo.Meta` pour la version et le niveau d'API.

Les primitives sont aussi disponibles au niveau racine de `Tempo` pour compatibilité.

### 2.2 `Tempo.Core`

`Tempo.Core` contient la surface canonique de programmation :
- `new_signal`
- `new_signal_agg`
- `emit`
- `await`
- `await_immediate`
- `pause`
- `loop`
- `when_`
- `watch`
- `parallel`
- `execute`
- `run_interactive`

### 2.3 `Tempo.Constructs`

`Tempo.Constructs` regroupe les opérateurs réactifs dérivés les plus utiles :
- `present_then_else`
- `after_n`
- `every_n`
- `timeout`
- `cooldown`
- `rising_edge`
- `falling_edge`
- `edge_by`
- `pulse_n`
- `supervise_until`

### 2.4 `Tempo.Low_level`

`Tempo.Low_level` est réservé aux auteurs d'extensions et aux cas avancés :
- `with_guard`
- `new_kill`
- `with_kill`
- `abort_kill`
- `abort_kill_next`
- `fork`
- `join`
- `peek`
- `is_present`

### 2.5 `Tempo.Observe`

`Tempo.Observe` contient les aides d'inspection et de test :
- `execute_trace`
- `execute_timeline`
- `execute_inspect`

### 2.6 `Tempo.Meta`

`Tempo.Meta` contient :
- `version_string`
- `api_level`
- `require_api_level`

## 3. Exécution

Tempo expose deux styles d'exécution :

- `execute` pour les scénarios batch et scriptés ;
- `run_interactive` pour les applications vivantes.

Le mode interactif utilise un modèle hybride :
- `push` pour réveiller le runtime ;
- `poll` pour importer les données externes à la frontière entre instants.

Ce point est important pour les entrées hôte et `tempo-jobs` :
- les producteurs externes ne modifient pas directement l'état du runtime ;
- ils réveillent le runtime ;
- les données sont intégrées lors d'un instant contrôlé.

## 4. Packages compagnons

### 4.1 `tempo-app`

`tempo-app` fournit des structures applicatives légères :
- `Tempo_app.App`
- `Tempo_app.Loop`
- `Tempo_app.Scene`

### 4.2 `tempo-jobs`

`tempo-jobs` lance des jobs externes sur des domaines OCaml et réinjecte leur progression/fin dans Tempo.

Le package est utile quand il faut :
- du vrai parallélisme ;
- un bridge propre vers le runtime synchrone ;
- une annulation coopérative.

### 4.3 `tempo-raylib`

`tempo-raylib` fournit des helpers Raylib pour les démos et applications graphiques.

### 4.4 `tempo-fluidsynth`

`tempo-fluidsynth` fournit un backend FluidSynth pour :
- un meilleur rendu sonore ;
- l'import MIDI ;
- les démos musicales.

### 4.5 `tempo-frp`

`tempo-frp` reste disponible comme encodage FRP/SF au-dessus de Tempo. Ce n'est pas la surface principale recommandée pour apprendre la librairie.

## 5. Exemple minimal

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

## 6. Applications de démonstration

Applications avancées importantes dans ce dépôt :
- `applications/advanced/game-univ`
- `applications/advanced/music_score_player`
- `applications/advanced/refactor`
- `applications/advanced/tempo-core-studio`

Démos simples :
- `applications/simple-demos/score-player-raylib`
- `applications/simple-demos/snake-raylib`
- `applications/simple-demos/boids-raylib`

## 7. Build et tests

```bash
cd /Users/fredericdabrowski/Repos/tempo/tempo-dev/tempo
dune build
dune runtest
dune build @doc
```

## 8. Références utiles

- API publique : `lib/tempo.mli`
- Implémentation principale : `lib/tempo.ml`
- Tests : `tests/ok`
- Documentation odoc : `doc/`
