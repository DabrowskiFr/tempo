# Lenia Raylib (Tempo)

Automate cellulaire continu de type Lenia:
- convolution locale lisse (kernel radial)
- fonction de croissance continue
- dynamique organique stable visuellement
- anti-stagnation autonome (drift parametrique + reseed adaptatif)

Lancer:
```bash
dune exec ./applications/run -- lenia-raylib
```

Controles:
- `SPACE`: pause/reprise
- `R`: reset
- `Q/E`: preset precedent/suivant
- `TAB` (ou `M`): basculer moteur `matriciel` <-> `processus/cellule`
- `ESC`: quitter

Le HUD affiche:
- le mode courant
- le temps moyen de step en millisecondes pour chaque moteur
- le ratio `process/matrix` pour visualiser l'impact des processus sur les performances
- un niveau de resolution automatique en mode `processus` (le jeu ajuste le nombre de cellules pour viser ~30 FPS)
