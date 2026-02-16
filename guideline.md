# Guideline Projet Tempo

Ce document est la référence des recommandations à appliquer pour les prochaines itérations du projet.

## 1) Priorités de travail

- Préserver la stabilité du runtime Tempo.
- Favoriser les améliorations par couches au-dessus de Tempo (pas de réécriture du runtime).
- Maintenir une bonne expérience de jeu (lisibilité, feedback, fluidité).
- Garder une séparation claire entre logique métier et rendu.

## 2) Règles Tempo

- Principe Tempo-first:
  toute application doit utiliser au maximum les primitives Tempo pour la logique
  (processus, signaux, contrôle temporel, orchestration).
- Pas de communication entre processus via références partagées.
- Un processus peut muter son état interne, mais les échanges inter-processus se font par signaux.
- Les nouvelles abstractions doivent rester construites au-dessus de Tempo.
- Éviter de simuler/copier un autre runtime FRP.
- FRP (`tempo-frp`) n’est pas la base de conception des applications Tempo:
  c’est une couche optionnelle pour démontrer l’expressivité de Tempo.
- Les applications principales doivent rester écrites en primitives Tempo
  (signaux/processus/contrôle temporel) sans dépendre de FRP.

## 3) Règles Raylib / tempo-raylib

- Principe "facilitateur, pas substitut":
  `tempo-raylib` doit faciliter le rendu/input/audio, sans déplacer la logique métier
  hors du style de programmation Tempo.
- Réutiliser `tempo-raylib` pour les primitives communes (UI, HUD, FX, audio) dès que possible.
- Garder le code spécifique jeu dans l’adaptateur local du jeu.
- Éviter la duplication de composants graphiques/audios génériques.
- Conserver un mode headless quand c’est pertinent.

## 3bis) Règles FRP / tempo-frp

- `tempo-frp` est un package indépendant, optionnel, au même titre que `tempo-raylib`.
- `tempo-frp` regroupe les abstractions FRP/SF et ne doit pas être fusionné dans `tempo`.
- L’usage de `tempo-frp` est réservé aux démos d’expressivité, prototypes FRP,
  ou modules explicitement marqués FRP.
- Une application "Tempo-first" ne doit pas exiger `tempo-frp` pour sa logique coeur.

Critères de revue pour ces deux principes:
- La logique de jeu reste implémentée dans des processus Tempo (pas dans une boucle Raylib impérative).
- Les communications inter-composants passent par signaux/événements Tempo.
- `tempo-raylib` sert aux briques techniques communes et non à encapsuler la logique de gameplay.
- Les comportements temporels (pause, tick, transitions d’état) restent exprimés côté Tempo.

## 4) Règles Gameplay (game-univ)

- Les mécaniques doivent être visuellement explicites (états, portée, feedback).
- Les faux positifs et succès doivent avoir des feedbacks audio/visuels distincts.
- Les réglages utilisateur doivent être simples (boutons clairs, interactions souris).
- L’équilibrage (combo, énergie, difficulté) doit rester paramétrable.

## 5) Règles UI/UX

- Lisibilité avant complexité (hiérarchie visuelle claire).
- Éviter les chevauchements de texte.
- Boutons cohérents (taille, espacement, état actif).
- Palette assumée, style visuel cohérent, rendu desktop et mobile/canvas correct.

## 6) Qualité et validation

- Compiler la cible impactée après chaque changement significatif.
- Exécuter `dune runtest` après modifications fonctionnelles.
- Ne pas introduire de régression sur les commandes de lancement.
- Garder la documentation de lancement à jour.
- Important: ne pas lancer plusieurs commandes `dune` en parallèle dans la même session/projet (`dune build`, `dune exec`, `dune runtest`), car le verrou global `_build/.lock` provoque des blocages/erreurs. Exécuter ces commandes en séquentiel.

## 7) Commandes de référence

- Lancer un jeu: `dune exec ./applications/run -- <nom-du-jeu>`
- Exemple: `dune exec ./applications/run -- game-univ`
- Tests: `dune runtest`

## 8) Règle de mise à jour de ce fichier

- Toute nouvelle préférence ou contrainte exprimée doit être ajoutée ici.
- En cas de conflit, la consigne explicite la plus récente de l’utilisateur prime.

## 9) Ce que signifie suivre le modèle Tempo

Cette section précise les principes concrets à respecter pour dire qu’une application
"suit le modèle Tempo".

### 9.1 Principe d’architecture

- Le coeur métier est exprimé en processus Tempo.
- Le temps logique (ticks, pauses, transitions, délais) est piloté par Tempo.
- Le rendu (Raylib) reste une couche d’adaptation I/O, pas le moteur de la logique.

### 9.2 Principe de communication

- Les composants logiques communiquent par signaux/événements Tempo.
- La communication entre processus par références partagées est proscrite.
- La mutation interne à un processus est autorisée si elle ne sert pas de canal
  inter-processus.

### 9.3 Principe de propriété d’état

- Soit un état est possédé par un seul processus (single-owner), soit il est découpé
  en états locaux indépendants.
- Si plusieurs processus doivent réagir à une même information, elle doit être publiée
  via signaux/événements.

### 9.4 Principe d’intégration tempo-raylib

- `tempo-raylib` fournit des briques techniques de rendu/input/audio réutilisables.
- `tempo-raylib` ne doit pas encapsuler la logique métier ni remplacer les primitives
  de contrôle Tempo.
- Les helpers communs (UI/HUD/FX/audio) doivent être centralisés dans `tempo-raylib`
  quand cela évite de la duplication.
- Tout ajout à `tempo-raylib` doit être non spécifique à un jeu particulier.
- Tout ajout à `tempo-raylib` doit apporter une plus-value liée au modèle synchrone
  Tempo (pilotage par flux/signaux, déterminisme par instant, commandes explicites).

Critères d’acceptation pour une API `tempo-raylib`:
- Réutilisable dans plusieurs jeux sans dépendre d’un univers de jeu.
- Exprimable en termes d’entrées/sorties synchrones compatibles Tempo.
- N’introduit pas de logique de gameplay spécifique.

### 9.5 Critères de conformité (checklist)

- La boucle de progression métier n’est pas une boucle Raylib impérative.
- Les transitions de gameplay dépendent d’événements/synchronisation Tempo.
- Les modules de rendu n’implémentent pas de logique métier critique.
- Les règles de jeu restent testables en headless (sans fenêtre graphique).
- Les flux I/O sont adaptateurs en bordure (input -> Tempo, Tempo -> output).
- L’application reste fonctionnelle sans `tempo-frp`, sauf cas explicite de démonstration FRP.
