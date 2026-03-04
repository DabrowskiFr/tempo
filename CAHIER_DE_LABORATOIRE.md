# Cahier de laboratoire

## 2026-03-04

### Format musical Tempo (`tempo-score`)

- objectif du lot: sortir du binaire `music_score_player` la logique de
  représentation/chargement de partition pour obtenir un format musical
  réutilisable, orienté instants Tempo
- réalisation:
  - ajout du package `tempo-score` dans `lib/score/`
  - ajout de `Tempo_score.t` (voix, notes en unités logiques, métrique,
    tempo initial)
  - extraction de la conversion MIDI -> score logique dans
    `Tempo_score.of_midi_file`
  - conservation d'une partition de repli (`Tempo_score.default`)
  - recâblage de `applications/advanced/music_score_player/src/main.ml`
    pour consommer `Tempo_score` au lieu du parser local
- documentation:
  - ajout de `doc/tempo-score/index.mld`
  - ajout des références à `tempo-score` dans la vue d'ensemble odoc
- tentative échouée:
  - commande `dune build --only-packages tempo-score @install`
  - échec attendu car `tempo-score` dépend de `tempo-fluidsynth`, exclu par
    `--only-packages`
  - correction: validation par build réel de l'application dépendante
    (`music_score_player`)
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe`
    passe avec la nouvelle extraction

### Représentation textuelle `tempo-score v1`

- objectif du lot: permettre d'éditer/charger une partition Tempo sans passer
  par MIDI, et fournir une conversion MIDI -> texte
- réalisation:
  - ajout dans `Tempo_score`:
    - `of_text` / `of_text_file`
    - `to_text` / `write_text_file`
    - exception `Parse_error`
  - format retenu (compact + explicite):
    - en-tête `tempo-score v1`
    - métadonnées `title`, `meter`, `unit`, `units_per_bar`, `bpm`
    - sections `voice` puis lignes `note start:.. dur:.. midi:.. vel:..`
  - ajout d'un convertisseur CLI:
    [score_convert.ml](/Users/fredericdabrowski/Repos/tempo/tempo-dev/tempo/applications/advanced/music_score_player/src/score_convert.ml)
  - adaptation du player pour charger aussi `.tscore` / `.tempo-score`
- tentative/constat:
  - écriture vers chemin relatif via `dune exec` observée dans le contexte
    d'exécution dune, pas forcément dans l'arborescence attendue
  - correction: utiliser un chemin absolu pour la conversion
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe`
  - conversion testée sur `bach_prelude_c.mid` vers le format texte

### Réorganisation des assets musique (MIDI vs tscore)

- objectif du lot: éviter les doublons dans l'UI en séparant clairement
  sources MIDI et partitions Tempo textuelles
- réalisation:
  - création de:
    - `applications/advanced/music_score_player/assets/mid/`
    - `applications/advanced/music_score_player/assets/tscore/`
  - déplacement de tous les `.mid` existants vers `assets/mid/`
  - conversion de tous ces MIDI en `.tscore` vers `assets/tscore/`
  - adaptation du player: la liste déroulante charge uniquement
    `assets/tscore/` (plus de mix midi/tscore dans l'UI)
  - mise à jour README de l'application
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe`

### Comparaison SoundFonts dans l'application

- objectif du lot: comparer plusieurs banques `.sf2/.sf3` sur un même morceau
  sans redémarrer l'application
- réalisation:
  - ajout d'un répertoire dédié
    `applications/advanced/music_score_player/assets/soundfonts/`
  - déplacement de `GeneralUser-GS.sf2` vers ce répertoire
  - chargement dynamique de la liste des SoundFonts dans l'UI
  - contrôle runtime:
    - `F5` banque précédente
    - `F6` banque suivante
  - changement de banque appliqué par redémarrage propre du synthé en
    conservant le même flux de partition Tempo
  - mise à jour du fallback hardcodé dans `tempo_fluidsynth` vers le nouveau
    chemin `assets/soundfonts/GeneralUser-GS.sf2`
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe`

### Politique SoundFonts (poids dépôt vs qualité locale)

- objectif du lot: garder une base git légère tout en permettant un rendu piano
  de meilleure qualité en local
- réalisation:
  - conservation de deux banques dans `assets/soundfonts`:
    - `GeneralUser-GS.sf2` (versionnée)
    - `MuseScore_General.sf2` (locale, non versionnée)
  - suppression locale des banques non retenues (`Vintage...`, `MuseScore...sf3`)
  - ajout de `.gitignore` dans `assets/soundfonts` pour ne versionner que
    `GeneralUser-GS.sf2`
  - fallback explicite dans le player:
    - défaut `MuseScore_General.sf2` si présent
    - sinon `GeneralUser-GS.sf2`
  - alignement du fallback dans `tempo_fluidsynth` sur le même ordre

### Support pédale piano (MIDI CC64) dans le modèle Tempo

- objectif du lot: intégrer la pédale sustain du piano dans le modèle logique
  Tempo, sans déléguer la timeline à FluidSynth
- réalisation:
  - extension du modèle `tempo-score` avec `pedals : pedal array`
    (`start_unit`, `channel`, `value`)
  - import MIDI: récupération des `Control_change` CC64 dans
    `Tempo_score.of_midi_file`
  - format texte: ajout du directive
    `pedal start:<u> ch:<c> val:<v>` (ou `value:<v>`)
  - sérialisation texte: export des événements pédale
  - exécution app:
    - ajout d'un processus Tempo dédié `pedal_process`
    - émission synchrone d'événements `Pedal_cc`
    - envoi côté audio via `Synth.control_change ... ~control:64`
  - binding FluidSynth:
    - ajout de `control_change` en OCaml et stub C (`fluid_synth_cc`)
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe`

### Format binaire `tempo-score` + CLI texte

- objectif du lot: accélérer le chargement des grosses partitions tout en
  gardant une vue texte éditable/inspectable
- réalisation:
  - ajout API binaire dans `Tempo_score`:
    - `to_binary` / `of_binary`
    - `write_binary_file` / `of_binary_file`
    - magic/version binaire explicite
  - extension de `score_convert.exe`:
    - conversion `mid|tscore|tbin` -> `tscore|tbin` selon extension de sortie
  - ajout d'un utilitaire dédié:
    - `score_show_text.exe <input.tbin|input.tscore|input.mid>`
    - affiche la représentation texte sur stdout
  - application `music_score_player`:
    - charge désormais `assets/scorebin/*.tbin`
    - `assets/tscore` reste la source texte éditable
  - conversion de tous les scores existants vers `assets/scorebin/*.tbin`
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe ./applications/advanced/music_score_player/src/score_show_text.exe`

### Notation texte plus musicale + synchronisation des assets

- objectif du lot: rendre le `.txt` plus lisible musicalement tout en gardant
  le runtime binaire `.tscore`
- réalisation:
  - format texte mis à jour en `tempo-score v2`:
    - `tempo: q=<bpm>` (référence noire)
    - positions `note/pedal` en `bar:<m> step:<k>`
    - compatibilité lecture conservée avec le v1 (`start:`/`bpm:`/`unit:`)
  - convention assets finalisée:
    - `assets/mid/*.mid` (sources)
    - `assets/txt/*.txt` (édition)
    - `assets/tscore/*.tscore` (runtime binaire)
  - resynchronisation complète depuis les MIDI présents:
    - régénération de tous les `.txt`
    - régénération de tous les `.tscore`
    - suppression des sorties orphelines non liées à un `.mid` source

### Binaire `tempo-score` v2 + compatibilité v1

- objectif du lot: verrouiller un format runtime binaire versionné v2, sans
  conversion au fil des instants pendant l'exécution de l'application
- réalisation:
  - `Tempo_score.of_binary` accepte désormais les préfixes
    `TEMPO_SCORE_BIN_V1` et `TEMPO_SCORE_BIN_V2`
  - `Tempo_score.to_binary` écrit désormais `TEMPO_SCORE_BIN_V2`
  - mise à jour doc API (`tempo_score.mli`) et odoc (`doc/tempo-score/index.mld`)
  - mise à jour README du player (panneau de sélection en bas)
  - régénération complète des assets:
    - `assets/mid/*.mid` -> `assets/txt/*.txt`
    - `assets/txt/*.txt` -> `assets/tscore/*.tscore` (binaire v2)
- tentative écartée (raison):
  - encapsuler un schéma binaire entièrement nouveau (enregistrements dédiés)
  - écarté pour ce lot car la structure sérialisée n'a pas changé et le besoin
    principal est la version explicite + rétrocompatibilité lecture
- validation:
  - build OK:
    `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe ./applications/advanced/music_score_player/src/score_show_text.exe`
  - vérification d'un payload `.tscore`: préfixe `TEMPO_SCORE_BIN_V2`

### Neutralisation de `unit_label` (pas de fuite MIDI)

- objectif du lot: retirer des formats d'échange les libellés dépendants de
  MIDI (`128 MIDI ticks`) qui perturbent la lecture métier
- réalisation:
  - valeur canonique interne: `unit_label = "step"` pour les scores importés et
    le score built-in
  - export texte v2: sortie en `unit: step` (au lieu de `grid: ...`)
  - lecture rétrocompatible conservée:
    - `unit:` et `grid:` toujours acceptés
    - normalisation vers `step` (ou `1/N` si fourni explicitement)
  - régénération des assets `txt/` et `tscore/` depuis `mid/`
- tentative écartée (raison):
  - suppression structurelle du champ `unit_label` du type `Tempo_score.t`
  - écartée pour ce lot car casserait la compatibilité Marshal binaire avec les
    partitions déjà produites
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe`
  - contrôle manuel: les `.txt` générés affichent `unit: step`

### Passage à `subdivision` (remplacement de `units_per_bar` dans le texte)

- objectif du lot: rendre la métadonnée de grille lisible musicien, en
  exposant une subdivision (`1/N`) plutôt qu'un compteur interne
  (`units_per_bar`)
- réalisation:
  - export texte `tempo-score v2` modifié:
    - `subdivision: 1/N` affiché
    - `units_per_bar` non affiché
  - parser texte étendu:
    - accepte `subdivision:` (nouveau)
    - conserve la lecture de `unit:` / `grid:` / `units_per_bar:` (anciens)
  - import MIDI:
    - calcule `units_per_bar` (interne runtime)
    - dérive `subdivision` lorsque possible et stocke le libellé `1/N`
  - régénération des assets `txt` et `tscore` depuis `mid`
- tentative écartée (raison):
  - retirer physiquement `units_per_bar` du type OCaml
  - écartée pour ce lot afin d'éviter une casse large (runtime + compatibilité
    binaire Marshal existante)
- validation:
  - build OK:
    `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe ./applications/advanced/music_score_player/src/score_show_text.exe`
  - contrôle des `.txt`: présence de `subdivision: ...`, absence de
    `units_per_bar`

### Piano: tempo map + CC66/CC67 + demi-pédale

- objectif du lot: enrichir l'exécution piano pour couvrir les contrôles
  expressifs demandés (tempo map, sostenuto/una corda, demi-pédale)
- réalisation:
  - `tempo-fluidsynth`:
    - exporte désormais la liste complète des changements de tempo et de
      métrique lus dans le MIDI (`tempo_changes_us_per_quarter`,
      `time_signature_changes`)
  - `tempo-score`:
    - ajout des événements `control` (`start_unit`, `ch`, `cc`, `val`)
    - ajout des événements `tempo_change` (`start_unit`, `bpm`)
    - import MIDI étendu:
      - contrôles CC64/66/67 conservés
      - tempo map convertie en `tempo_change`
    - format texte étendu:
      - `tempo_change bar:<m> step:<k> q:<bpm>`
      - `control bar:<m> step:<k> ch:<c> cc:<n> val:<v>`
    - compatibilité lecture conservée pour anciens fichiers (`pedal` legacy)
    - binaire runtime passé en `TEMPO_SCORE_BIN_V3`
      (lecture v1/v2/v3, décodage legacy pour anciens payloads)
  - player `music_score_player`:
    - remplacement du flux `Pedal_cc` par `Control_cc` générique
    - processus Tempo dédié pour rejouer les `control` sur la timeline logique
    - application de la tempo map pendant l'exécution:
      - mise à jour de `transport.bpm`
      - recalage de `source.unit_ms` entre instants
- tentative échouée:
  - première passe de build KO (inférence ambiguë de type sur tri de listes
    d'événements record)
  - correction: annotations explicites de type sur les comparateurs de tri
    (`pedal`, `control`, `tempo_change`)
- validation:
  - build OK:
    `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe ./applications/advanced/music_score_player/src/score_show_text.exe`
  - régénération complète des assets MIDI -> TXT -> TSCORE
  - contrôle binaire: en-tête `.tscore` = `TEMPO_SCORE_BIN_V3`

### Uniformisation des noms de partitions Beethoven (assets MIDI)

- objectif du lot: rendre les noms de fichiers ajoutés plus lisibles et
  homogènes entre `mid/`, `txt/`, `tscore/`
- réalisation:
  - renommage de:
    - `6101-2d_moonlight_sonata_27-2_1_2_(nc)smythe.*`
    - `71773a_sonata_quasi-fantasie_(moonlight)_(nc)smythe.*`
  - nouveaux noms:
    - `beethoven_moonlight_sonata_op27_no2_mov1_smythe_6101.*`
    - `beethoven_moonlight_sonata_op27_no2_mov1_smythe_71773a.*`
  - régénération des contenus `.txt` et `.tscore` depuis les `.mid` renommés
    pour aligner aussi les titres internes
- validation:
  - conversion OK via `score_convert.exe` pour les deux fichiers

### Renommage des trois `piano_sonata_13_*` (Beethoven)

- objectif du lot: corriger l'identification compositeur/oeuvre pour les trois
  fichiers ajoutés en conservant une nomenclature uniforme
- réalisation:
  - renommage:
    - `piano_sonata_13_1_(c)oguri.*` ->
      `beethoven_sonata_op13_mov1_oguri.*`
    - `piano_sonata_13_2_(c)oguri.*` ->
      `beethoven_sonata_op13_mov2_oguri.*`
    - `piano_sonata_13_3_(c)oguri.*` ->
      `beethoven_sonata_op13_mov3_oguri.*`
  - régénération `txt/tscore` depuis les `mid` renommés
- validation:
  - présence des nouveaux noms dans `assets/mid`, `assets/txt`, `assets/tscore`

### Renommage de Bach en convention lisible

- objectif du lot: homogénéiser le nom de la pièce de Bach avec la convention
  descriptive utilisée sur les autres assets
- réalisation:
  - `bach_prelude_c.*` -> `bach_prelude_in_c_major.*`
  - régénération de `txt` et `tscore` depuis le `mid` renommé
- validation:
  - présence de `bach_prelude_in_c_major` dans `assets/mid`, `assets/txt`,
    `assets/tscore`

### Correction du modèle de durée d'instant en lecture

- objectif du lot: supprimer l'écart de timing perçu entre la partition MIDI
  source et la lecture Tempo (silences/espaces artificiels signalés)
- cause racine identifiée:
  - la boucle interactive utilisait une durée d'instant fixe
    `15000/bpm` (hypothèse "double-croche"), indépendante de la grille réelle
    de la partition (`meter` + `units_per_bar`)
  - conséquence: lecture temporellement inexacte dès qu'une partition n'était
    pas exactement sur cette hypothèse, même si la conversion d'événements
    était correcte
- réalisation:
  - remplacement par un calcul musical exact:
    - `quarter_ms = 60000/bpm`
    - `bar_ms = quarter_ms * (num * 4 / den)`
    - `unit_ms = bar_ms / units_per_bar`
  - `transport` stocke désormais `unit_ms` courant
  - la tempo map met à jour à la fois `bpm` et `unit_ms`
  - régénération complète des assets `txt/tscore` depuis les `mid`
- validation:
  - build OK:
    `dune build ./applications/advanced/music_score_player/src/main.exe ./applications/advanced/music_score_player/src/score_convert.exe`
  - conversion complète relancée sur tous les `mid` présents

### Lecture MIDI exacte (ticks) pilotée par Tempo

- objectif du lot: supprimer les artefacts de quantification audibles
  (glissements/bégaiements) sur certains morceaux tout en gardant Tempo comme
  orchestrateur de la boucle interactive
- réalisation:
  - ajout d'un mode de lecture runtime `Midi_ticks` dans
    `music_score_player`:
    - si un `.mid` homonyme du `.tscore` existe dans `assets/mid/`, la lecture
      se fait sur événements MIDI exacts (`note on/off`, `control change`)
      indexés par tick
    - fallback automatique sur le mode quantifié existant si le `.mid` est
      absent ou en erreur
  - tempo map appliquée au tick exact:
    - mise à jour de `source.unit_ms` à partir de
      `tempo_us_per_quarter / division`
    - recalage du BPM affiché
  - conversion visuelle conservée:
    - l'UI reste basée sur le score (`units`) avec projection tick->unit pour
      le playhead
  - suppression du morceau built-in de la liste
- tentative/diagnostic:
  - vérification des MIDI Moonlight: pas de chevauchement pathologique dans les
    données source, ce qui a confirmé que les artefacts venaient du pipeline de
    quantification/rejeu
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe` OK

### Retour à une lecture 100% `tscore`

- objectif du lot: supprimer l'ambiguïté runtime et revenir à la source
  d'exécution unique demandée (`tscore`) pour le playback
- réalisation:
  - retrait complet de la branche `Midi_ticks` dans `main.ml`
  - suppression du fallback automatique vers les `.mid` homonymes
  - conservation du pilotage Tempo (pause/restart/boucle/tempo map `tscore`)
- validation:
  - `dune build ./applications/advanced/music_score_player/src/main.exe` OK

### Renforcement de la documentation technique (MIDI vs `tscore`)

- objectif du lot: fournir une explication plus précise et structurée des
  formats de données et de leur interprétation d'exécution
- réalisation:
  - réécriture de
    `applications/advanced/music_score_player/EXPLICATIONS_MUSIC_PLAYER.md`
    avec:
    - structure binaire MIDI (`MThd`, `MTrk`, delta-time VLQ, running status)
    - interprétation exacte des notes/tempo/signature
    - structure détaillée du format `tscore` textuel v2
    - comparaison MIDI vs `tscore` (précision, abstraction, usages)
    - rappel du pipeline d'exécution Tempo et de la couche FluidSynth
  - génération d'un PDF dédié:
    `EXPLICATIONS_MUSIC_PLAYER.pdf`
- validation:
  - génération PDF OK via `pandoc + xelatex`

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

### 2026-03-04 - music_score_player : import MIDI local et polyphonie réelle

- objectif : corriger la vitrine musique pour qu'elle démarre vite avec un
  fichier MIDI et joue réellement plusieurs notes au lieu d'un seul son
  initial
- problèmes observés :
  - l'import MIDI via le player temps réel de FluidSynth bloquait le démarrage
    de l'application
  - après remplacement de l'import, l'application ne jouait qu'une seule note
    au début, ce qui révélait un modèle de voix encore monophonique
- refactoring réalisé :
  - remplacement de l'import MIDI bloquant par un parseur SMF local dans
    `lib/fluidsynth/tempo_fluidsynth.ml`
  - ajout d'un SoundFont local de meilleure qualité :
    `applications/advanced/music_score_player/assets/GeneralUser-GS.sf2`
  - mise à jour du choix de SoundFont par défaut pour privilégier ce fichier
  - refactoring de `music_score_player` pour que chaque voix garde une liste de
    notes actives au lieu d'une seule note courante
  - arrêt des notes échues par filtrage des notes actives à chaque pulse
- tentative échouée :
  - réutiliser directement le player MIDI de FluidSynth comme importeur
  - raison : la lecture restait couplée au temps réel et gelait l'application
    au démarrage
- résultat :
  - le build de l'application musique repasse
  - le moteur de lecture supporte maintenant la polyphonie au sein d'une voix
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : robustesse du runtime interactif et du backend FluidSynth

- objectif : stabiliser la lecture de fichiers MIDI réels dans la vitrine
  musique
- problèmes observés :
  - `Emit : multiple emission` lorsque plusieurs pulses étaient injectés dans
    le même instant
  - `fluid_synth_noteoff failed` sur certains fichiers importés
- corrections réalisées :
  - la source interactive de `music_score_player` ne livre plus qu'un seul
    `Pulse` par instant ; les pulses en retard sont mis en file
  - la boucle interactive du runtime n'injecte plus d'entrée hors d'un instant
    actif
  - `note_off` et `all_notes_off` sont rendus tolérants aux relâchements
    redondants côté FluidSynth
- résultat :
  - disparition des exceptions liées aux doubles émissions
  - le backend audio n'échoue plus sur un relâchement logique déjà consommé
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : changement de morceau à chaud

- objectif : pouvoir tester plusieurs fichiers MIDI disponibles dans les assets
  sans redémarrer l'application
- refactoring réalisé :
  - ajout d'une liste de choix construite à partir des `.mid` et `.midi` du
    dossier `applications/advanced/music_score_player/assets`
  - ajout d'un sélecteur déroulant dans l'interface graphique
  - arrêt propre du programme Tempo courant sur demande de changement de
    morceau, puis relance d'un nouveau programme dans la même fenêtre et le
    même process
- choix d'implémentation :
  - ne pas reconfigurer dynamiquement les processus de voix en place
  - relancer proprement le programme Tempo avec un nouveau score, ce qui est
    plus simple et plus robuste tout en restant transparent pour l'utilisateur
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : rendre les mesures plus lisibles

- objectif : faire ressortir plus nettement les mesures dans la partition
  affichée
- constat :
  - les séparations de mesure existaient après l'ajout de la métrique, mais
    restaient trop discrètes visuellement
- corrections réalisées :
  - alternance de fonds de mesure plus contrastés
  - bandeau supérieur léger par mesure pour renforcer la lecture du découpage
  - traits de mesure plus lumineux et plus épais
  - numéros de mesure plus visibles
  - correction de l'ordre des paramètres affichés dans l'en-tête (`Meter` /
    `View`)
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : lecture plus fidèle au modèle Tempo

- objectif : faire utiliser à l'application musicale le plus possible le
  modèle Tempo lui-même, au lieu d'une logique impérative locale pour la durée
  des notes
- refactoring réalisé :
  - remplacement de l'état mutable de notes actives par un signal agrégé
    d'événements musicaux `note_events`
  - une note est maintenant incarnée par un petit processus Tempo qui :
    - attend `start_unit` pulses
    - émet `Note_on`
    - attend `duration_units` pulses
    - émet `Note_off`
  - un processus audio dédié consomme `note_events` et appelle FluidSynth
  - le rendu visuel des notes actives est recalculé à partir de `current_unit`
    et de la partition, plutôt que relu depuis un état partagé
- intérêt :
  - la durée des notes est désormais portée directement par les instants Tempo
  - la lecture illustre explicitement `parallel`, `watch`, `await`, `emit` et
    les signaux agrégés
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : commandes audio appliquées côté hôte

- objectif : éviter que les processus Tempo appellent directement FluidSynth,
  afin que la communication avec le backend audio se fasse à la frontière de
  sortie du runtime
- refactoring réalisé :
  - remplacement du processus audio interne par une sortie `host_output`
    contenant :
    - la `frame` à afficher
    - la liste des `audio_commands` à appliquer pour l'instant courant
  - `render_process` agrège maintenant les commandes audio présentes pendant
    l'instant et les transmet au callback de sortie
  - le callback hôte applique ensuite `note_on`, `note_off` et `all_notes_off`
    à FluidSynth avant de dessiner la frame
- intérêt :
  - Tempo continue à décider des durées et des instants de début/fin
  - FluidSynth n'est plus appelé depuis la logique synchrone elle-même
  - la frontière entre coeur Tempo et effet externe audio est plus nette
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : séquencer le rendu hôte après les émissions de notes

- problème observé :
  - après séparation entre logique Tempo et backend audio, l'application
    tournait mais aucun son n'était audible
- analyse :
  - le rendu hôte pouvait être déclenché trop tôt dans l'instant, avant que les
    processus réveillés par `pulse` aient eu le temps d'émettre leurs
    `Note_on` / `Note_off`
- correction :
  - les demandes de rendu sont maintenant planifiées via `Low_level.fork`, ce
    qui décale l'émission de `render` après les émissions du même instant déjà
    déclenchées par le contrôle
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : ajout de morceaux MIDI plus orientés rock

- objectif : disposer de morceaux plus proches d'un jeu "groupe de rock" pour
  tester la vitrine musicale avec un matériau plus énergique et plus dense
- morceaux ajoutés dans `applications/advanced/music_score_player/assets` :
  - `acdc_for_those_about_to_rock.mid`
  - `acdc_highway_to_hell.mid`
  - `acdc_let_there_be_rock.mid`
  - `acdc_back_in_black.mid`
- source :
  - téléchargements depuis BitMidi, choisis pour avoir un caractère plus
    clairement rock que les exemples classiques déjà présents
- remarque :
  - certains fichiers se sont révélés trop lourds ou musicalement mal encodés
    pour la vitrine (nombre de notes très élevé, métrique incohérente comme
    `1/4`) ; le sélecteur les filtre désormais automatiquement au chargement

### 2026-03-04 - music_score_player : nettoyage du modèle vitrine

- objectif : rapprocher encore l'application du modèle Tempo en supprimant les
  points trop ad hoc relevés pendant l'audit
- corrections réalisées :
  - le sélecteur de morceaux ne parse plus tous les fichiers MIDI au démarrage
    ; le chargement est maintenant lazy au moment de la sélection
  - le bridge audio n'utilise plus de concaténation de listes coûteuse ; il
    accumule les commandes en ordre inverse puis les draine côté hôte
  - l'ordonnancement rendu/audio n'utilise plus `Low_level.fork` ; il passe
    maintenant par un vrai signal agrégé `render_request` suivi d'un `pause`
    explicite dans `render_process`
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : rejet explicite des MIDI trop lourds

- problème observé :
  - certains morceaux orientés rock restaient problématiques même après le
    nettoyage du sélecteur
  - en particulier, des partitions très denses ou trop longues pouvaient
    dégrader fortement l'expérience de la vitrine
- correction :
  - ajout d'un garde-fou dans `load_score`
  - si une partition dépasse un seuil raisonnable pour la vitrine
    (`voices`, `notes`, `total_units`, `units_per_bar`), elle est rejetée
    proprement et remplacée par la partition intégrée
- intérêt :
  - l'application ne tente plus d'exécuter des fichiers qui sortent du domaine
    cible de la démonstration
  - le comportement devient explicite et stable, y compris quand on force un
    fichier sur la ligne de commande
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : retrait complet des MIDI rock problématiques

- décision :
  - supprimer complètement les fichiers ACDC encore présents dans les assets
    plutôt que de garder des cas connus comme peu adaptés à la vitrine
- fichiers retirés :
  - `acdc_back_in_black.mid`
  - `acdc_highway_to_hell.mid`
- raison :
  - même avec les garde-fous de chargement, ils restaient source de confusion
    pour l'utilisateur et n'apportaient pas une démonstration stable du modèle
    Tempo

### 2026-03-04 - music_score_player : suppression du contrôle tempo +/- 

- demande :
  - retirer le changement de rythme au clavier (`+` / `-`) dans la vitrine
    musicale
- modifications :
  - suppression des événements `Tempo_up` / `Tempo_down`
  - suppression de la capture clavier `Key.Equal` / `Key.Minus`
  - suppression du traitement associé dans `control_process`
  - mise à jour de l'aide affichée à l'écran
- validation :
  - `dune build applications/advanced/music_score_player/src/main.exe`

### 2026-03-04 - music_score_player : renforcement du document technique PDF

- objectif du lot:
  - produire un document autonome, détaillé et exploitable pour comprendre
    tout le pipeline (MIDI -> format Tempo -> ordonnancement Tempo ->
    commandes audio), hors aspects UI
- actions menées :
  - extension substantielle de
    `applications/advanced/music_score_player/EXPLICATIONS_MUSIC_PLAYER.md`
    avec :
    - pipeline complet de transformation des données
    - modèle de temps unifié et relation tick/barre/step
    - traces précises note et contrôle
    - ordonnancement et invariants d'exécution
    - limites connues et cartographie code -> rôle
  - régénération du PDF :
    - `applications/advanced/music_score_player/EXPLICATIONS_MUSIC_PLAYER.pdf`
- tentative non retenue :
  - conserver une version plus concise orientée "vue d'ensemble"
  - raison de rejet : insuffisante pour expliquer précisément les conventions
    de timeline et le protocole exact Tempo <-> bridge audio
- résultat :
  - document orienté "spécification technique exécutable", plus précis pour
    l'audit et la maintenance
