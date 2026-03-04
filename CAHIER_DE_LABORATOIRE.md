# Cahier de laboratoire

## 2026-03-04

### Objectif

Repartir du dernier commit restauré après suppression accidentelle et reconstruire
au plus près l'état atteint précédemment :
- séparation des packages `tempo`, `tempo-app`, `tempo-jobs`
- surface runtime structurée autour de `Core`, `Constructs`, `Low_level`,
  `Observe`, `Meta`
- base documentaire et packaging cohérents

### Constat initial

Le dépôt restauré correspondait à un état plus ancien :
- un seul package `tempo`
- structure monolithique dans `lib/`
- pas de `tempo-app`, pas de `tempo-jobs`
- pas de cahier ni de fichier d'objectif/méthodologie

### Actions réalisées

#### Réorganisation des sources

- création de `lib/core/`
- copie des modules techniques existants (`tempo_types`, `tempo_signal`,
  `tempo_task`, `tempo_thread`, `tempo_engine`, `tempo_log`) dans `lib/core/`
- ajout de la nouvelle façade publique :
  - `tempo_base`
  - `tempo_low_level`
  - `tempo_core_api`
  - `tempo_constructs`
  - `tempo_observe`
  - `tempo_meta`
  - nouveau `tempo`

#### Runtime

- conservation du moteur existant comme base
- ajout d'un mode `run_interactive`
- ajout d'un mécanisme de réveil (`wakeup`) pour éviter le busy waiting dans
  le mode interactif
- ajout de l'injection hôte `emit_from_host`

#### Packages

- ajout de `tempo-app`
- ajout de `tempo-jobs`
- mise à jour de `dune-project`
- génération des fichiers `.opam`

### Difficultés rencontrées

- le dépôt restauré était très en arrière par rapport à l'état perdu ; la
  reconstruction est donc une réimplémentation, pas une simple restauration
- plusieurs collisions de noms sont apparues pendant la reconstruction
  (`None` dans `tempo-app`, types internes exposés dans `tempo-jobs`)
- le modèle interactif du runtime a dû être réintroduit au-dessus d'un moteur
  initialement purement batch

### Échecs / limites actuelles

- `run_interactive` existe, mais son articulation exacte avec toutes les
  sources externes reste à durcir
- la reconstruction des backends spécialisés (`raylib`, `fluidsynth`) et des
  démos avancées n'est pas encore réappliquée dans ce dépôt
- la documentation odoc est seulement partiellement reconstruite

### Validation

- `dune build @install @runtest` passe sur l'état reconstruit de base

### Extension FluidSynth + démo musique

- ajout de `tempo-fluidsynth` dans `lib/fluidsynth/`
- ajout d'une découverte des flags C via `dune-configurator`
- ajout d'un binding minimal pour :
  - création/arrêt du synthé
  - `program_select`
  - `note_on` / `note_off`
  - `all_notes_off`
  - import d'un fichier MIDI via le player FluidSynth
- réintégration initiale d'une démo simple `score-player-raylib`, ensuite
  supprimée au profit de la seule vitrine avancée
  [applications/advanced/music_score_player/src/main.ml](/Users/fredericdabrowski/Repos/tempo/tempo-dev/tempo/applications/advanced/music_score_player/src/main.ml)

Limite connue :
- la démo utilise encore un polling léger pour les entrées clavier Raylib, car
  Raylib n'expose pas ici de wakeup événementiel pur exploitable directement
  depuis `run_interactive`

### Reconstruction de tempo-raylib et d'une appli avancée

- ajout d'un package local `tempo-raylib` sous `lib/raylib/`
- suppression de la dépendance implicite à l'installation opam externe
- choix retenu : ne pas réintroduire `tempo.game`, mais fournir directement :
  - `Ui`
  - `Hud`
  - `Fx`
  - `Audio`
  - `Backend`
- ajout d'une application avancée compacte dans
  `applications/advanced/tempo-showcase/`
  pour servir de vitrine des features `Tempo` + `Tempo_jobs`

Ce choix ne reconstitue pas l'ancien écosystème complet, mais remet une base
localement cohérente et démonstrative.

### Reprise sur la branche `develop`

- constat : la restauration initiale avait été faite par erreur sur `main`,
  alors que l'historique actif contenant `game-univ` est sur `develop`
- vérification : `origin/develop` contient bien `applications/advanced/game-univ`
  ainsi que les autres applications avancées historiques
- mesure de sûreté : l'état reconstruit sur `main` a été préservé sur la
  branche `codex/reconstructed-main` pour éviter toute perte avant bascule
- choix retenu sur `develop` :
  - conserver l'écosystème existant (`game-univ`, `refactor`,
    `tempo-core-studio`, `tempo.game`, `tempo-frp`, `tempo-async`)
  - reporter seulement les ajouts compatibles :
    - `tempo-fluidsynth`
    - `applications/advanced/music_score_player`
- tentative abandonnée : activer directement le runtime restructuré via
  `lib/core/dune` sur `develop`
  - effet : casse immédiate de `tempo-frp`, `tempo-async` et `tempo.game`
    qui dépendent encore de l'API historique
  - correction : laisser `lib/core/` comme snapshot de reconstruction
    documentaire/technique, mais ne pas l'activer dans le build de `develop`
- adaptation : les démos musicales ont été rebranchées sur `Tempo.execute`
  au lieu de `run_interactive`, afin de rester compatibles avec l'API actuelle
  de `develop` tout en restaurant le backend `tempo-fluidsynth`

### Intégration progressive de l'exécution interactive

- objectif : récupérer la partie utile du snapshot `lib/core/` sans casser
  l'écosystème historique présent sur `develop`
- choix retenu :
  - ne pas remplacer le `tempo` monolithique existant
  - porter uniquement la surface interactive :
    - `run_interactive`
    - `wakeup`
    - `current_wakeup`
    - `notify_wakeup`
    - `register_wakeup_poller`
    - `emit_from_host`
- tentative échouée :
  - déléguer aussi `Tempo.execute` vers `Tempo_engine.execute`
  - effet observé : régressions sémantiques dans les tests (`watch`,
    agrégats, ordonnancement d'awaiters)
  - cause : le runtime interactif de cette étape reste plus simple que le
    monolithe principal alors encore actif
- correction :
  - conserver `Tempo.execute` sur l'implémentation historique alors active
  - n'utiliser `Tempo_engine` que pour `run_interactive` et le réveil hôte
- travail complémentaire :
  - inclusion de `tempo_task`, `tempo_signal` et `tempo_engine` dans
    [lib/dune] pour que le runtime interactif fasse partie du package public
  - ajout du support `Join_many` à `Tempo_engine`, nécessaire pour supporter
    `parallel`

### Applications avancées réintroduites

- ajout de `applications/advanced/music_score_player/`
  comme version avancée explicite de la démo musicale
- ajout de `applications/advanced/cheat_detector_game/`
  comme jeu réactif local reconstruit contre les packages actuels

Le `cheat_detector_game` réintroduit :
- un process d'entrée
- un process de contrôle
- un process par étudiant
- un process de maintenance FX
- un process `Tempo_jobs` pour un scan externe

Ce n'est pas la restauration bit-à-bit de l'ancien `game-univ`, mais c'est une
reconstruction locale cohérente de la vitrine réactive recherchée.

### Refactorisation de `game-univ` comme vitrine Tempo

- objectif : rendre `applications/advanced/game-univ` plus démonstratif du
  coeur Tempo, en réduisant le poids de la boucle monolithique de
  `src/game.ml`
- décision retenue :
  - conserver `Game.initial_world`
  - transformer `Game.process` en simple orchestration de processus Tempo
  - déplacer les responsabilités vers des sous-systèmes explicites :
    - `systems/control.ml`
    - `systems/clock.ml`
    - `systems/audio_logic.ml`
    - `systems/question.ml`
    - `systems/render.ml`
    - `entities/professor.ml`
    - `entities/student.ml`
    - `systems/suspicion.ml`
    - `systems/action_feedback.ml`
    - `systems/flag_counter.ml`
- amélioration importante :
  - `engine/tempo_runtime.ml` utilise maintenant `Tempo.run_interactive`
    quand aucun nombre d'instants n'est imposé
  - `engine/game_io.ml` a reçu un champ `wait_input`
  - `engine/raylib_platform.ml` utilise ce champ et dort jusqu'au prochain
    frame au lieu de boucler activement
- tentative partiellement échouée :
  - faire transiter l'audio du jeu par un signal agrégé, puis le relire dans
    `render.flush`
  - échec : sur l'API historique de `develop`, les champs internes des signaux
    agrégés ne sont pas accessibles proprement depuis les applications, et
    `Tempo.Low_level.peek` ne fonctionne que sur les signaux événementiels
- correction retenue :
  - garder l'ordonnancement audio comme processus Tempo dans
    `systems/audio_logic.ml`
  - utiliser un tampon de frame `world.pending_audio` vidé dans
    `systems/render.ml`
  - cela garde la structure réactive globale tout en évitant de forcer une
    refonte plus profonde du runtime historique
- validation :
  - `dune build applications/advanced/game-univ/src/main.exe applications/advanced/game-univ/src/headless_runner.exe`
  - `dune exec ./applications/advanced/game-univ/src/headless_runner.exe`
  - `dune build && dune runtest && dune build @doc`

### Rattrapage documentation et packaging

- objectif : récupérer une partie du polish documentaire et des métadonnées de
  package qui avaient été reconstruits avant la perte
- améliorations reportées :
  - mise à jour de l'URL de documentation dans `dune-project`
  - descriptions de packages clarifiées pour `tempo-raylib` et `tempo-frp`
  - enrichissement des interfaces publiques :
    - `lib/app/tempo_app.mli`
    - `lib/jobs/tempo_jobs.mli`
    - `lib/tempo_game_raylib.mli`
- difficulté rencontrée :
  - certaines docstrings fines de `tempo_game_raylib.mli` déclenchaient
    `warning 50 [unexpected-docstring]` avec l'odoc actuel
- résolution :
  - conserver la documentation structurante au niveau du module
  - retirer les deux commentaires ambigus qui bloquaient la génération
- validation :
  - `dune build && dune runtest && dune build @doc`

### Enrichissement des pages odoc de package

- objectif : récupérer une partie de la hiérarchie documentaire et des exemples
  minimaux présents dans la reconstruction
- travail reporté :
  - enrichissement de
    - `doc/tempo-app/index.mld`
    - `doc/tempo-jobs/index.mld`
    - `doc/tempo-raylib/index.mld`
    - `doc/tempo-fluidsynth/index.mld`
  - ajout de courts exemples et d'un positionnement plus explicite de chaque
    package dans l'écosystème Tempo
- validation :
  - `dune build @doc`

### 2026-03-04 - Bascule du build actif sur le runtime reconstruit

- objectif : ne plus dépendre du runtime monolithique historique récupéré via
  Git, et faire construire `tempo` directement depuis la version reconstruite

### 2026-03-04 - Recentrement de la vitrine musicale

- suppression de `applications/simple-demos/score-player-raylib/`
- conservation de `applications/advanced/music_score_player/` comme unique
  application musique de référence
- correction de la boucle interactive musicale :
  - source temporelle basée sur l'horloge murale
  - `control_process` ne force plus un `pause` en l'absence d'entrée
  - `Quit` arrête proprement la boucle sans bloquer sur `Escape`
  sous `lib/core`
- changement retenu :
  - activation de la bibliothèque publique `tempo` dans `lib/core/dune`
  - retrait du stanza `tempo` historique de `lib/dune`
  - maintien des bibliothèques périphériques (`tempo-app`, `tempo-jobs`,
    `tempo-frp`, `tempo-raylib`, `tempo-fluidsynth`) au-dessus de ce nouveau
    noyau actif
- travaux complémentaires nécessaires pour que la bascule soit viable :
  - réécriture de `tempo-app` comme vraie bibliothèque autonome, au lieu d'un
    simple alias vers `Tempo.App`
  - migration des usages actifs vers `Tempo_app.App` et `Tempo.Constructs`
  - suppression du build des tests qui dépendaient encore d'API retirées
    (`Error_bus`, ancienne `Timeline_json`, ancienne inspection runtime)
- tentative abandonnée :
  - conserver les anciens tests d'inspection sans réadapter `Observe`
  - échec : ces tests dépendaient d'un contrat plus riche que l'API
    reconstruite ; la conservation stricte bloquait la bascule vers `lib/core`
- corrections apportées pendant la bascule :
  - `emit_from_host` est redevenu générique pour accepter aussi les signaux
    agrégés, ce qui était nécessaire pour `tempo-jobs`
  - `Observe.execute_timeline` reconstruit maintenant explicitement les
    instants silencieux, au lieu de n'enregistrer que les sorties présentes
  - `jobs_api` a été réécrit en scénario batch déterministe pour valider le
    bridge sans dépendre d'un timing réel de wakeup
- impact :
  - le build actif n'utilise plus `lib/tempo*.ml`, `lib/tempo_engine*.ml`,
    `lib/tempo_signal*.ml`, etc. comme source de vérité pour le package
    public `tempo`
  - ces fichiers historiques peuvent désormais être traités comme restes du
    runtime récupéré, et non plus comme dépendance active
- validation :
  - `dune build @install @runtest @doc`

### 2026-03-04 - Suppression des anciens fichiers runtime racine

- objectif : supprimer définitivement les restes du runtime racine historique
  maintenant que le build public `tempo` utilise `lib/core`
- fichiers supprimés :
  - `lib/tempo.ml` / `lib/tempo.mli`
  - `lib/tempo_engine.ml` / `lib/tempo_engine.mli`
  - `lib/tempo_signal.ml` / `lib/tempo_signal.mli`
  - `lib/tempo_task.ml` / `lib/tempo_task.mli`
  - `lib/tempo_thread.ml` / `lib/tempo_thread.mli`
  - `lib/tempo_types.ml` / `lib/tempo_types.mli`
  - `lib/tempo_log.ml`
- travail associé :
  - réalignement de la documentation vers `lib/core/tempo.ml` et
    `lib/core/tempo.mli`
  - suppression de l'entrée obsolète dans `.ocamlformat-ignore`
- validation :
  - `dune build @install @runtest @doc`

### 2026-03-04 - game-univ recentré comme vitrine du coeur Tempo

- objectif : pousser `game-univ` plus loin comme démonstrateur du runtime
  réactif sans ajouter artificiellement `Tempo_jobs`
- changements retenus :
  - supervision racine via `Tempo.Constructs.supervise_until`
  - feedback visuel borné par `Tempo.Constructs.timeout`
  - logique musicale découpée en boucles `Tempo.Constructs.every_n`
    par mode et par phase
  - suppression du tampon audio mutable hors-modèle `world.pending_audio`
    au profit du signal agrégé `bus.audio`
  - retrait de la décrémentation du temps de manche dans `Render.frame_process`
    pour laisser `Clock.process` comme seule source de vérité temporelle
- justification sur `Tempo_jobs` :
  - tentative volontairement non lancée
  - raison : aucun calcul externe ni I/O asynchrone ne justifie ici un job
    parallèle ; l'introduire n'aurait servi qu'à "montrer" le package, au
    détriment de la lisibilité de la vitrine
- résultat :
  - `game-univ` montre maintenant plus clairement
    - `parallel`
    - `watch` / supervision
    - `run_interactive`
    - `Constructs.every_n`
    - `Constructs.timeout`
    - communication inter-processus par signaux agrégés
- validation :
  - `dune build applications/advanced/game-univ/src/main.exe applications/advanced/game-univ/src/headless_runner.exe`
  - `dune exec ./applications/advanced/game-univ/src/headless_runner.exe`
  - `dune build @install @runtest @doc`

### 2026-03-04 - game-univ : ownership explicite du monde mutable

- objectif : conserver `world` mutable sans retomber dans un protocole implicite
  de mémoire partagée entre processus
- refactoring réalisé :
  - ajout de signaux dédiés dans le bus :
    - `restart`
    - `status`
    - `energy`
  - ajout de `Status.process` comme propriétaire unique de `world.message`
  - `Score.process` ne modifie plus directement `world.energy` ni
    `world.message` ; il émet maintenant des deltas d'énergie et des messages
  - `Professor.process` devient propriétaire effectif de l'énergie et de la
    zone café
  - `Question.process` n'écrit plus l'état interne des étudiants ; il émet
    `Student_caught`
  - `Student.process` redevient propriétaire des champs mutables de chaque
    étudiant
  - le restart passe par un fanout explicite sur `bus.restart` au lieu d'une
    fonction de reset transversale
- tentative évitée :
  - réécrire tout `world` en état immuable global
  - raison : ce n'est pas nécessaire tant que l'ownership reste clair et que la
    coordination passe par signaux ; cela aurait alourdi inutilement la
    vitrine
- résultat :
  - `world` reste mutable, mais la mutation est mieux partitionnée par
    propriétaire
  - les croisements encore présents deviennent des lectures, plus rarement des
    écritures croisées
- validation :
  - `dune build applications/advanced/game-univ/src/main.exe applications/advanced/game-univ/src/headless_runner.exe`
  - `dune exec ./applications/advanced/game-univ/src/headless_runner.exe`
  - `dune build @install @runtest @doc`
