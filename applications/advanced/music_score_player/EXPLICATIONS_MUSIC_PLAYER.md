# Tempo Music Player

## 1) Structure d'un fichier MIDI (niveau fichier)

Un fichier MIDI standard (`.mid`) est un conteneur binaire organisé en *chunks*.

### 1.0 Qu'est-ce qu'un tick MIDI?

Un **tick** est l'unité de temps discrète interne d'un fichier MIDI.

Ce n'est pas une milliseconde fixe.  
Sa durée réelle dépend de deux choses:

1. `division` (ticks par noire),
2. `tempo` courant (`us_per_quarter`).

Formules exactes:

- `tick_us = us_per_quarter / division`
- `tick_ms = tick_us / 1000`
- `BPM = 60_000_000 / us_per_quarter`

Exemple concret:

- `division = 480`
- `tempo = 500000 us` (120 BPM)
- `tick_us = 500000 / 480 = 1041.666... us`
- `tick_ms ~ 1.0417 ms`

Si le tempo change en cours de morceau, la durée d'un tick change aussi.

Conséquence importante:

- un événement à `tick=10000` n'a pas un temps réel absolu fixe sans intégrer
  toute la tempo map entre `0` et `10000`.

### 1.1 Chunk d'en-tête `MThd`

Le chunk `MThd` contient les paramètres globaux:

- `format` (`0` ou `1` dans notre importeur),
- `track_count`,
- `division` (résolution temporelle).

`division` = nombre de ticks par noire (PPQN).  
Exemple: `division = 480` -> 1 noire = 480 ticks.

### 1.2 Chunks de piste `MTrk`

Chaque piste contient une suite d'événements:

- chaque événement est précédé d'un `delta-time` en VLQ
  (*variable-length quantity*),
- le temps absolu est reconstruit en cumulant les deltas.

Pseudo-structure:

```text
MTrk:
  delta_0, event_0
  delta_1, event_1
  ...
```

`delta-time` est stocké en VLQ (1 à 4 octets dans la pratique).  
Le temps absolu se reconstruit:

```text
abs_tick_0 = delta_0
abs_tick_1 = abs_tick_0 + delta_1
...
```

Dans le code d'import, c'est ce cumul qui produit la liste
`(tick_absolu, midi_event)`.

### 1.3 Types d'événements utiles pour la lecture

Événements canal:

- `NoteOn(ch, key, vel)`
- `NoteOff(ch, key)` (ou `NoteOn vel=0`)
- `ControlChange(ch, cc, value)`
- `ProgramChange(ch, program)`

Événements méta:

- `SetTempo` (`0x51`): microsecondes par noire (`us_per_quarter`)
- `TimeSignature` (`0x58`)

### 1.4 Détails d'interprétation importants

- **Running status**: un status byte peut être omis si identique au précédent;
  le parser doit le reconstruire.
- **Durée d'une note**: appariement `NoteOn` -> `NoteOff` de même
  `(channel, key)`.
- **Tempo réel**: obtenu via `SetTempo`, pas via une hypothèse fixe.
- **MIDI type 0/1**:
  - type 0: une seule piste globale,
  - type 1: plusieurs pistes synchronisées sur la même timeline.
- **Canal vs piste**:
  - la piste organise les événements dans le fichier,
  - le canal (`0..15`) indique l'instrument logique de sortie.
- **NoteOn velocity=0**:
  - doit être traité comme `NoteOff`.

Formules:

- `BPM = 60_000_000 / us_per_quarter`
- `tick_ms = us_per_quarter / division / 1000`

Donc MIDI est précis: la précision dépend de `division` et de la tempo map.

### 1.5 Ce que MIDI ne dit pas directement

MIDI encode l'exécution (événements), pas une partition éditoriale complète.

Typiquement absent/partiel:

- notation enharmonique explicite (Do# vs Réb),
- phrasé éditorial détaillé,
- mise en page partition.

Donc MIDI est excellent pour jouer, pas toujours suffisant pour afficher une
partition "graveur" sans interprétation supplémentaire.

---

## 2) Comment on interprète MIDI pour jouer effectivement les notes

Dans `tempo-fluidsynth`, l'import construit:

- `division`,
- tempo initial + liste des changements de tempo,
- signature rythmique + changements,
- liste d'événements `(tick_absolu, midi_event)`.

Ensuite, dans `tempo-score`:

1. regroupement des notes par voix/instrument,
2. conversion des ticks en unités logiques (`start_unit`, `duration_units`),
3. extraction des contrôles (CC64/66/67),
4. conversion de la tempo map vers `tempo_change`.

Détail exact pour les notes:

1. sur `NoteOn(vel>0)`, on pousse un état actif `(start_tick, vel, bank, prog)`.
2. sur `NoteOff` (ou `NoteOn vel=0`), on retrouve l'état actif correspondant.
3. on crée une note avec:
   - `start_tick`,
   - `duration_tick = off_tick - start_tick`,
   - `key`, `velocity`.

Puis conversion en unités Tempo:

- `start_unit` dérivé du tick de début,
- `end_unit` dérivé du tick de fin,
- `duration_units = end_unit - start_unit`.

Ce calcul évite de tronquer artificiellement les durées.

Détail exact pour le tempo:

- chaque `SetTempo` devient un événement `tempo_change`,
- le transport met à jour `unit_ms` selon le BPM courant.

Ce sont ces données qui sont ensuite sérialisées en `.txt` et `.tscore`.

---

## 3) Structure du format `tscore` textuel (v2)

Le format texte (`.txt`) est auto-descriptif et orienté lecture humaine.

### 3.1 En-tête

```text
tempo-score v2
title: ...
meter: N/D
subdivision: 1/S
tempo: q=BPM
```

Sémantique:

- `meter N/D`: signature rythmique de référence.
- `subdivision 1/S`: granularité logique d'un pas.
- `units_per_bar` implicite: `N * (S / D)` (quand divisible).

### 3.2 Événements de tempo

```text
tempo_change bar:<m> step:<k> q:<bpm>
```

### 3.3 Déclaration de voix

```text
voice: <name> | ch:<c> bank:<b> prog:<p>
```

### 3.4 Notes

```text
note bar:<m> step:<k> dur:<u> midi:<key> vel:<v>
```

- `bar` / `step`: position dans la grille logique,
- `dur`: durée en unités logiques,
- `midi`: pitch (0..127),
- `vel`: vélocité (1..127).

Convention de position:

- `bar` commence à `1`.
- `step` commence à `1`.
- conversion interne:
  - `start_unit = (bar-1)*units_per_bar + (step-1)`.

### 3.5 Contrôles

```text
control bar:<m> step:<k> ch:<c> cc:<n> val:<v>
```

Utilisé notamment pour piano:

- `cc=64` sustain,
- `cc=66` sostenuto,
- `cc=67` una corda.

### 3.6 Lecture effective

Le runtime charge le binaire `.tscore` (même modèle de données), pas le texte.
Le texte sert d'inspection/édition.

Le binaire et le texte représentent la même structure métier:

- texte: lisible/éditable.
- binaire: chargement rapide et stable pour runtime.

---

## 4) Comparaison MIDI vs `tscore` texte

### 4.1 Niveau d'abstraction

- MIDI:
  - format source bas niveau, horodatage en ticks, encodage compact binaire.
- `tscore` texte:
  - format métier Tempo, positions en `bar/step`, lisible et éditable.

### 4.2 Précision temporelle

- MIDI:
  - précision native en ticks + tempo map exacte.
- `tscore`:
  - précision dépend de la grille choisie (`subdivision`, `units_per_bar`).
  - plus simple à lire, mais potentiellement moins fidèle si quantification
    trop grossière.

Lecture pratique:

- MIDI = format source le plus précis temporellement.
- `tscore` = format d'exécution Tempo (lisible + stable) avec une projection
  discrète adaptée au moteur synchrone.

### 4.3 Sémantique musicale

- MIDI:
  - évènements bruts (canal, CC, program, note on/off).
- `tscore`:
  - structure explicite par voix + notes + contrôles + tempo changes.

### 4.4 Usage dans ce projet

- conversion/ingestion: MIDI -> `tempo-score`.
- exécution player: `.tscore` (binaire).
- inspection: `.txt`.

Choix d'architecture actuel:

- une seule source runtime: `.tscore`,
- MIDI utilisé comme format d'import/outillage.

---

## 5) Comment Tempo joue la musique (précision d'exécution)

### 5.1 Chargement

Au changement de morceau:

1. lecture du `.tscore`,
2. création des signaux Tempo,
3. configuration synthé (program/bank),
4. initialisation transport (`playing`, `bpm`, `unit_ms`, `current_unit`).

### 5.1.b Rôle des signaux Tempo

Les signaux sont le mécanisme de communication synchrone entre processus.

Signaux principaux utilisés par le player:

- `pulse`:
  - représente l'avancée d'un instant logique de partition.
  - chaque émission de `pulse` fait progresser les processus musicaux d'un pas.
- `restart`:
  - force les processus musicaux à repartir du début du morceau.
  - utilisé lors d'un `Restart` manuel ou d'un retour fin de morceau.
- `note_events` (agrégateur):
  - collecte les commandes audio produites pendant l'instant
    (`Note_on`, `Note_off`, `Control_cc`, `Panic`).
- `render_request` (agrégateur booléen):
  - indique qu'une nouvelle frame doit être rendue.
- `input`:
  - porte les événements hôte/UI (`Toggle_play`, `Pulse`, `Select_score`, etc.).

Pourquoi des signaux:

- ils imposent une propagation déterministe par instant,
- ils découpent proprement logique synchrone (calcul) et effets (audio/rendu),
- ils permettent d'exprimer les dépendances sans état partagé complexe.

### 5.2 Horloge d'instants

La source interactive génère des `Pulse` selon `unit_ms`.  
`unit_ms` est calculé à partir de:

- `meter`,
- `units_per_bar`,
- BPM courant.

### 5.3 Processus Tempo parallèles

- **control_process**:
  - gère UI (`SPACE`, `R`, etc.),
  - avance l'unité courante à chaque pulse,
  - applique les `tempo_change`,
  - déclenche `panic/restart` au wrap.
- **voice_process** (par voix):
  - attend la bonne unité,
  - émet `Note_on`,
  - planifie `Note_off`.
- **control_process_timeline**:
  - rejoue les événements `control` au bon instant.
- **audio_bridge_process**:
  - agrège les commandes audio.
- **render_process**:
  - produit les frames UI.

### 5.3.b Comment la partition est respectée

Le respect de la partition vient du couplage:

- position des événements dans le score (`start_unit`, `duration_units`),
- avancement discret par `pulse`,
- attente explicite (`await pulse`) dans les processus.

Mécanisme:

1. chaque `voice_process` lit une liste triée de notes.
2. il attend le nombre exact de pulses jusqu'au `start_unit` de la note.
3. il émet `Note_on` exactement à cet instant.
4. un sous-processus attend `duration_units` pulses.
5. ce sous-processus émet `Note_off`.

Donc:

- démarrage note = quand l'instant courant atteint son `start_unit`,
- arrêt note = `duration_units` instants plus tard.

La logique est identique pour `control_process_timeline`:

- un `Control_cc` est émis à l'instant exact indiqué dans le score.

### 5.3.d Transformation exacte des durées en `note_on` / `note_off`

Cette section décrit le mécanisme exact utilisé par l'application.

#### Données d'entrée (dans le score)

Chaque note porte:

- `start_unit`
- `duration_units`
- `midi` (hauteur)
- `volume` (vélocité normalisée)

#### Étape 1: attente jusqu'au démarrage

Dans `voice_process`, pour la note courante:

1. on calcule `delta = start_unit - current_unit`,
2. on attend `delta` pulses via `wait_pulses`,
3. à l'instant d'arrivée, on émet `Note_on`.

#### Étape 2: planification de l'arrêt

Au moment du `Note_on`, `trigger_note` lance un sous-processus
`note_release_process` qui:

1. attend `duration_units` pulses,
2. émet ensuite `Note_off`.

Donc la durée de la note est exactement "nombre de pulses à attendre" entre
le début et la fin.

#### Étape 3: recouvrements et notes simultanées

Si plusieurs notes ont le même `start_unit`, elles sont toutes émises dans le
même instant (`launch_same_start`).

Les `note_off` peuvent aussi tomber dans le même instant que des `note_on`.
L'application applique un ordre stable côté bridge:

1. `Panic`
2. `Note_off`
3. `Control_cc`
4. `Note_on`

Ce choix évite les artefacts de réattaques coupées.

#### Étape 4: sécurité sur mêmes touches

Le bridge maintient un compteur actif par `(channel,key)`:

- `Note_on` -> compteur +1
- `Note_off` -> compteur -1
- l'appel réel `Synth.note_off` est envoyé uniquement quand le compteur tombe à
  zéro

Cela évite qu'un `Note_off` ancien coupe une note plus récente de même pitch.

### 5.3.e Transformation exacte des contrôles (`control_change`)

Les contrôles (`score.controls`) sont traités par un processus dédié:

1. lecture triée des événements `control`,
2. attente du `start_unit` via `wait_pulses`,
3. émission d'un `Control_cc(channel, control, value)`.

Contrairement aux notes:

- un contrôle n'a pas de `duration_units`,
- c'est un événement ponctuel,
- son effet est porté par l'état interne du synthé jusqu'à un autre contrôle.

Exemples:

- `CC64` sustain:
  - `value` élevé -> sustain actif,
  - `value` bas -> sustain relâché.
- `CC66` sostenuto
- `CC67` una corda

### 5.3.f Du signal logique à l'appel audio

Chaîne complète:

1. les processus Tempo émettent des événements logiques (`Note_on`, `Note_off`,
   `Control_cc`) dans `note_events` (signal agrégateur),
2. `audio_bridge_process` collecte ces événements,
3. `apply_audio_commands` les ordonne et les traduit en appels FluidSynth:
   - `Synth.note_on`
   - `Synth.note_off`
   - `Synth.control_change`
   - `Synth.all_notes_off`

Cette séparation garantit:

- logique temporelle déterministe côté Tempo,
- effets audio appliqués entre instants,
- comportement stable même avec événements simultanés.

### 5.3.c Démarrer, mettre en pause, reprendre

Commandes UI:

- `SPACE` (`Toggle_play`):
  - bascule `playing`.
  - si passage en pause: émission `Panic` (arrêt immédiat des notes externes).
- reprise:
  - les pulses recommencent, les processus repartent depuis l'état courant.
- `R` (`Restart`):
  - reset transport (`current_unit <- -1`),
  - `Panic`,
  - émission `restart` pour relancer les processus depuis le début.

### 5.4 Discipline instant/inter-instant

- instant: calcul synchrone pur (signaux/process).
- inter-instant: effets externes (audio/rendu).

Tempo reste le planificateur logique.

---

## 6) Conventions d'affichage de la timeline

### 6.1 Axes

- horizontal: unités logiques (`step`).
- vertical: voix/instruments.

### 6.1.b Ce que représentent les "petits carrés"

Dans la grille, les petits rectangles/carrés correspondent à des segments
de notes projetés sur les unités visibles:

- un rectangle horizontal = une note (ou portion de note) sur la voix,
- sa position X = `start_unit`,
- sa largeur = `duration_units`,
- sa ligne Y = voix/instrument.

Quand une note est plus longue que la fenêtre visible, seule la partie visible
est dessinée.

### 6.2 Mesures

- alternance de fond par mesure,
- traits verticaux renforcés aux débuts de mesure,
- numérotation des mesures.

`units_per_bar` définit la largeur d'une mesure.

Conventions visuelles des mesures:

- fond alterné par barre (deux teintes alternées),
- trait vertical fort au début de chaque mesure,
- numéro de mesure affiché au-dessus de la grille.

### 6.3 Playhead

Le curseur vertical suit `current_unit`.

Convention:

- couleur chaude (jaune clair) pour distinguer clairement l'instant courant
  des autres repères.

### 6.4 Adaptation d'affichage

- si beaucoup de voix: réduction automatique de hauteur de ligne.
- compression horizontale de la grille.
- liste de morceaux mono-colonne scrollable.

### 6.5 Couleurs et états des notes

Chaque voix reçoit une couleur de palette fixe (index de voix):

- la couleur sert au nom de la voix,
- au contour des notes de cette voix,
- et aux notes actives (version plus opaque).

Conventions de rendu:

- note "normale" (historique/future): rectangle semi-transparent avec contour.
- note active (instant courant dans l'intervalle de la note):
  - surcouche plus visible,
  - message texte "`N notes`" si plusieurs notes de la voix sonnent
    simultanément.

### 6.6 Messages affichés dans l'en-tête

Sous le titre, plusieurs lignes d'information:

- nom de partition (`Score`),
- SoundFont active, statut (`PLAYING`/`PAUSED`), BPM, durée d'unité,
- métrique (`Meter`), fenêtre visible (`View`), nombre de morceaux chargés.

Ces lignes sont des conventions de monitoring runtime: elles donnent l'état
musical et l'état de transport à l'instant courant.

### 6.7 Convention de sélection des morceaux

Dans le panneau de morceaux:

- ligne sur fond plus clair = morceau sélectionné,
- clic gauche = sélection,
- molette = scroll vertical,
- scrollbar verticale = position dans la liste.

Cette zone ne représente pas la musique; elle représente l'état de navigation
des partitions.

---

## 7) Lecture des notes et communication avec FluidSynth

### 7.1 API audio utilisée

Le player envoie:

- `program_select`
- `note_on`
- `note_off`
- `control_change`
- `all_notes_off`

### 7.2 Ordonnancement des commandes par instant

Ordre stable appliqué:

1. `Panic`
2. `Note_off`
3. `Control_cc`
4. `Note_on`

Cela limite les artefacts lors d'événements simultanés.

### 7.3 Protection contre les coupures parasites

Compteur de notes actives par `(channel, key)`:

- `Note_on` incrémente,
- `Note_off` décrémente,
- le vrai `note_off` synthé est envoyé seulement à zéro.

Cela évite qu'un `note_off` tardif coupe une note plus récente de même pitch.

### 7.4 Où la note est réellement démarrée/arrêtée

Important:

- la décision logique `start/stop` est prise dans les processus Tempo
  (instant synchrone),
- l'appel effectif à FluidSynth est fait par le bridge audio
  (inter-instant).

Donc Tempo décide *quand* une note doit vivre; FluidSynth exécute la commande
de synthèse.

---

## 8) Synthèse

- MIDI est un format binaire très précis (ticks + tempo map).
- `tscore` texte est un format métier plus lisible, structuré pour Tempo.
- Le player exécute la musique depuis `.tscore`, avec Tempo comme orchestrateur
  des processus synchrones.
- FluidSynth est la couche de synthèse sonore; la timeline logique reste pilotée
  par Tempo.

---

## 9) Pipeline complet (hors UI)

Vue d'ensemble de la chaîne de production sonore:

1. **Source fichier**
   - soit `MIDI (.mid)` pour import,
   - soit `tscore binaire (.tscore)` pour exécution.
2. **Parsing / chargement**
   - MIDI: extraction des événements horodatés en ticks,
   - tscore: chargement du modèle `Tempo_score.t`.
3. **Normalisation métier**
   - notes, contrôles, tempo map, métrique.
4. **Planification Tempo**
   - génération d'instants logiques (`Pulse`),
   - émission d'événements logiques (`Note_on`, `Note_off`, `Control_cc`).
5. **Bridge audio**
   - ordonnancement stable des commandes dans l'instant,
   - protections anti-coupure.
6. **Synthèse**
   - appels `tempo-fluidsynth` (`note_on`, `note_off`, `control_change`),
   - rendu sonore final.

---

## 10) Modèle de temps unifié

Trois échelles de temps coexistent:

1. **Tick MIDI**
   - unité native du fichier MIDI.
2. **Unit Tempo**
   - pas logique de la partition pour le scheduler synchrone.
3. **Temps réel**
   - millisecondes utilisées par l'horloge interactive.

Relations:

- `tick_us = us_per_quarter / division`
- `BPM = 60_000_000 / us_per_quarter`
- `unit_ms` dérivé de la métrique du score:
  - `quarter_ms = 60000 / BPM`
  - `bar_ms = quarter_ms * (num * 4 / den)`
  - `unit_ms = bar_ms / units_per_bar`

Exemple numérique:

- `meter = 4/4`, `BPM = 120`, `units_per_bar = 16`
- `quarter_ms = 500`
- `bar_ms = 2000`
- `unit_ms = 125`

Chaque `Pulse` arrive donc toutes les `125 ms`.

---

## 11) Trace complète d'une note (de la donnée au son)

Exemple conceptuel:

1. Le score contient:
   - `note bar:3 step:5 dur:4 midi:67 vel:90`
2. Conversion en unité absolue:
   - `start_unit = (3-1)*units_per_bar + (5-1)`
3. `voice_process`:
   - attend `start_unit - current_unit` pulses,
   - émet `Note_on`.
4. `note_release_process`:
   - attend `4` pulses,
   - émet `Note_off`.
5. `audio_bridge_process` récupère les deux événements.
6. `apply_audio_commands` traduit en:
   - `Synth.note_on channel key velocity`
   - `Synth.note_off channel key`
7. FluidSynth produit l'attaque puis l'extinction.

Ce parcours illustre le principe fondamental:

- la **durée** de note = nombre de pulses entre `on` et `off`.

---

## 12) Trace complète d'un contrôle (CC64)

Exemple conceptuel:

1. Le score contient:
   - `control bar:8 step:2 ch:0 cc:64 val:127`
2. `control_process_timeline` attend l'instant correspondant.
3. Il émet `Control_cc(0,64,127)`.
4. Le bridge applique:
   - `Synth.control_change ~channel:0 ~control:64 ~value:127`
5. Le sustain devient actif et reste actif
   jusqu'à un autre `CC64` (ex: `val:0`).

Différence clé avec les notes:

- note: événement + durée explicite.
- contrôle: événement ponctuel, effet persistant côté synthé.

---

## 13) Ordonnancement exact dans un instant

L'application impose un ordre stable:

1. `Panic`
2. `Note_off`
3. `Control_cc`
4. `Note_on`

Objectif:

- éviter les conflits sur événements simultanés,
- empêcher les coupures involontaires à la réattaque,
- garantir un comportement reproductible.

Puis un garde-fou supplémentaire:

- compteur actif par `(channel,key)`,
- `note_off` réel envoyé seulement quand compteur atteint `0`.

---

## 14) Invariants d'exécution

Invariants logiques:

- les processus n'avancent que sur `Pulse`,
- `restart` remet les processus musicaux au début,
- la tempo map met à jour `unit_ms` dynamiquement,
- la boucle reste déterministe à données égales.

Invariants audio:

- `Panic` coupe toutes les notes (`all_notes_off`),
- pas de `note_off` destructeur si une note homonyme plus récente est active.

Invariants de format:

- runtime lit un seul format de playback: `.tscore`,
- MIDI est utilisé comme format d'import/outillage.

---

## 15) Limites connues et implications

1. La partition runtime est discrétisée en unités (`units`).
2. La fidélité dépend de la qualité de conversion MIDI -> tscore.
3. Les contrôles pris en charge sont ciblés (piano: CC64/66/67).
4. Le moteur n'est pas une gravure de partition complète:
   - priorité à l'exécution synchrone contrôlée par Tempo.

Implication pratique:

- pour des cas extrêmes (ornements très fins), il faut augmenter la granularité
  du score ou adapter la conversion.

---

## 16) Cartographie code -> rôle (hors UI)

Fichier principal:

- `applications/advanced/music_score_player/src/main.ml`

Fonctions clés:

- `control_process`
  - pilotage transport/tempo/restart/pulse.
- `voice_process`
  - lecture des notes d'une voix.
- `note_release_process`
  - émission différée des `Note_off`.
- `control_process_timeline`
  - lecture des événements de contrôle.
- `audio_bridge_process`
  - transfert signal logique -> commandes audio.
- `apply_audio_commands`
  - ordonnancement final + appels FluidSynth.

Formats:

- `lib/score/tempo_score.ml`
  - structure score + conversion + sérialisation.
- `lib/fluidsynth/tempo_fluidsynth.ml`
  - interface de synthèse/audio + import MIDI.
