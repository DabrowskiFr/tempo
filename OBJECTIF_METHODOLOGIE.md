# Objectif et méthodologie

## Objectif courant

Ramener le dépôt restauré à un état proche de la version précédemment obtenue,
en priorité sur :

1. la structure des packages
2. la clarté de l'API publique
3. la documentation et la traçabilité des changements

## Architecture cible

### Packages principaux

- `tempo` : runtime synchrone et constructions réactives
- `tempo-app` : helpers applicatifs (`App`, `Loop`, `Scene`)
- `tempo-jobs` : jobs externes parallèles réinjectés dans Tempo
- `tempo-raylib` : backend et modèles de présentation spécialisés
- `tempo-fluidsynth` : rendu SoundFont et import MIDI pour les démos musicales

### Niveaux de l'API `tempo`

- `Tempo.Core`
- `Tempo.Constructs`
- `Tempo.Low_level`
- `Tempo.Observe`
- `Tempo.Meta`

## Méthodologie

1. repartir du dépôt restauré sans supposer que les changements perdus sont
   encore disponibles localement
2. reconstruire d'abord une base compilable et testable
3. documenter immédiatement les décisions importantes
4. procéder par couches :
   - runtime
   - packages
   - doc
   - démos / backends
5. valider fréquemment avec Dune
6. pour les backends externes, privilégier une reconstruction minimale mais
   directement testable avant d'affiner l'API
7. pour les applications avancées, préférer des vitrines locales cohérentes
   avec l'API courante plutôt qu'une dépendance implicite à l'ancien écosystème
8. lorsqu'une divergence de branche est découverte, préserver d'abord le
   travail local reconstruit sur une branche dédiée, puis reporter
   sélectivement sur la branche cible sans régression pour les applications
   déjà présentes
9. lors d'un port progressif d'API runtime, ne pas déléguer les primitives
   centrales (`execute`) à une implémentation plus simple tant que l'équivalence
   sémantique n'est pas prouvée par les tests
10. pour les applications vitrines, préférer une orchestration explicite de
    plusieurs processus Tempo plutôt qu'une grosse boucle unique mutable, même
    si une petite quantité d'état partagé reste nécessaire pour composer la
    frame finale
11. pour les interfaces publiques et l'odoc, viser des signatures directement
    lisibles sans supposer que l'utilisateur ouvre l'implémentation ; en cas de
    conflit avec les contraintes d'odoc, préférer une documentation simple qui
    génère proprement plutôt qu'un commentaire fin qui casse la build doc
12. lorsqu'un runtime reconstruit coexiste avec un runtime historique, traiter
    la migration comme un vrai basculement de source de vérité : activer le
    nouveau build, adapter les dépendants actifs, puis seulement conserver ou
    supprimer l'ancien code comme archive
13. pour une application vitrine, n'introduire `Tempo_jobs` que si un besoin
    métier réel existe (calcul externe, import ou I/O parallélisable) ; ne pas
    l'utiliser comme simple démonstration décorative
14. dans une application Tempo, `world` peut rester mutable si chaque sous-état
    a un propriétaire explicite ; éviter les mutations croisées et préférer des
    signaux dédiés (`restart`, statut, deltas) pour les transferts entre
    processus

## Points d'attention

- la correction prévaut sur la vitesse
- éviter les API publiques ambiguës
- réserver `Low_level` aux auteurs d'extensions et aux cas avancés
- garder une séparation nette entre primitives réactives et helpers
  applicatifs
