# Manuel de jeu: Cheat Detector (Tempo x Raylib)

## 1. Concept

Dans **Cheat Detector**, vous incarnez un professeur dans une salle d'examen.
Votre objectif est de surprendre les etudiants qui trichent, en evitant les faux positifs, pour maximiser le score sur 3 manches.

## 2. Objectif et fin de partie

- La partie est decoupee en **3 manches**.
- Chaque manche dure **45 secondes** (a 60 FPS, soit 2700 frames).
- La partie se termine a la fin de la 3e manche.
- Un ecran de resultats affiche:
  - score final
  - flagrants delits
  - faux positifs
  - verifications dans le vide
  - meilleur combo

## 3. Lancer le jeu

Depuis la racine du depot:

```bash
dune exec ./applications/game/game-univ/src/main.exe
```

Ou via le script du jeu:

```bash
./applications/game/game-univ/run
```

## 4. Commandes

## 4.1 Clavier

- `Entree` ou `Espace`: demarrer (si la partie n'a pas commence)
- `P`: pause / reprise
- `R`: recommencer la partie
- `Fleches`: deplacement du professeur
- `E`: interroger l'etudiant le plus proche
- `C` (maintenir): boire un cafe (si a portee, et sans bouger)
- `-` / `=`: diminuer / augmenter le facteur de triche
- `Echap`: quitter

## 4.2 Souris

Vous pouvez cliquer sur les boutons a droite:

- `Start`
- `Pause` / `Reprise`
- `Recommencer`
- `-` et `+` pour regler le facteur de triche

## 5. Lecture de l'interface (HUD)

En haut:

- `Score`: votre score courant
- `Signalements`: nombre d'etudiants montes a 100 de suspicion
- `Combo`: combo en cours (augmente les points en serie)
- `Best`: meilleur combo realise
- `Precision`: catches / total interrogations
- `Energie`: reserve du professeur (0 a 100%)
- `Triche X.Xx`: facteur de frequence/duree de triche
- `Fenetre combo`: temps restant pour continuer le combo
- `Manche` / `Temps`: progression temporelle de la partie

En bas:

- barre message contextuelle (succes, erreur, info systeme)

Dans la salle:

- cercle autour du prof: rayon de detection actif
- etudiants:
  - teinte neutre s'ils sont hors rayon
  - teinte plus vive et jauges s'ils sont dans le rayon
  - animations/fx lors des actions

## 6. Mecanique coeur: detection et interrogation

1. Deplacez le professeur pour vous positionner.
2. Approchez-vous d'un etudiant (distance d'interaction proche).
3. Appuyez sur `E` pour l'interroger.
4. Le resultat depend de l'etat reel de l'etudiant a cet instant.

Cas possibles:

- **Flagrant delit**: etudiant effectivement en train de tricher.
- **Faux positif**: etudiant innocent interroge.
- **Verification vide**: aucun etudiant assez proche.

## 7. Score, penalties et combo

## 7.1 Gains

En flagrant delit:

- points de base: `+8`
- bonus profil:
  - Prudent: `+4`
  - Opportuniste: `+2`
  - Chaotique: `+1`
- bonus combo: augmente avec la serie (plafonne)

## 7.2 Pertes

- faux positif: malus significatif, casse le combo
- verification vide: petit malus, casse le combo

## 7.3 Combo

- une capture reussie prolonge la fenetre combo (5 secondes)
- si vous enchainez avant expiration, le combo monte
- si la fenetre expire ou si vous faites une erreur, combo remis a zero

## 8. Energie, focus, rayon de detection

## 8.1 Energie

- l'energie baisse avec le temps
- elle baisse plus vite en mouvement
- elle baisse aussi lors des interrogations

## 8.2 Rayon de detection

- le rayon depend de l'energie:
  - energie haute -> rayon large
  - energie basse -> rayon reduit
- le rayon est borne entre une valeur min et max

## 8.3 Focus

Apres un cafe complet:

- gros regain d'energie
- activation d'un **focus temporaire**
- focus = rayon legerement amplifie pendant quelques secondes

## 9. Systeme de cafe

- un cafe apparait a un emplacement parmi plusieurs points fixes
- il possede une duree de vie
- pour le consommer:
  - approchez-vous
  - restez immobile
  - maintenez `C` jusqu'au remplissage complet
- s'il expire, il disparait puis reapparait plus tard ailleurs

## 10. Profils etudiants

Chaque etudiant suit un profil comportemental:

- **Prudent**: triche plutot hors rayon, plus discret
- **Opportuniste**: alterne selon contexte et fenetres
- **Chaotique**: comportement plus fluctuant

Le profil influence:

- probabilite de triche
- valeur en points lorsqu'il est capture
- cout energie de l'interrogation reussie

## 11. Indices visuels et sonores

Indices visuels:

- variation de couleurs des anneaux
- jauges de suspicion
- feedback flottant lors des actions (`+points`, `faux positif`, etc.)
- pulse ecran sur evenements importants

Indices audio:

- son de succes
- son d'erreur (faux positif)
- son de verification vide
- alerte de fin de manche
- musique d'ambiance adaptative (patrouille, tension, fin de partie)

## 12. Conseils de jeu

- visez la regularite plutot que la vitesse brute
- evitez `E` en rafale: les faux positifs detruisent score et combo
- surveillez l'energie avant de vous eloigner
- utilisez le cafe de facon proactive (pas seulement a 0%)
- exploitez les phases de tension (triche probable dans le rayon)

## 13. Depannage rapide

- Fenetre noire au premier lancement: attendre 1-2 secondes apres compilation initiale.
- Pas de son:
  - verifier la sortie audio systeme
  - verifier que l'application n'est pas mutee
- Le jeu ne se ferme pas: utiliser `Echap`.

## 14. Notes techniques (lecture code)

- Logique jeu: `/Users/fredericdabrowski/Repos/tempo/applications/game/game-univ/src/game.ml`
- Types/evenements/frame: `/Users/fredericdabrowski/Repos/tempo/applications/game/game-univ/src/types.ml`
- Rendu et input Raylib: `/Users/fredericdabrowski/Repos/tempo/applications/game/game-univ/src/engine/raylib_adapter.ml`

Le jeu suit une boucle synchrone Tempo: une entree d'input, un tick logique, puis emission d'une frame (draw + audio).
