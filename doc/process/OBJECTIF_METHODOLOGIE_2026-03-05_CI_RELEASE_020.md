# Objectif et Methodologie - CI release/0.2.0

Date: 2026-03-05

## Objectif
Diagnostiquer et corriger un echec CI GitHub sur la branche `release/0.2.0` avec correction minimale, explicable et verifiable.

## Methodologie
1. Identifier un run en echec recent via `gh run list`.
2. Lire les logs du step en echec via `gh run view --log-failed`.
3. Isoler la cause racine (pas seulement le symptome workflow).
4. Corriger a la source de verite (`dune-project`) et synchroniser l'opam impacte.
5. Valider localement avec checks rapides et deterministes (`opam lint`, `dune build`).
6. Commit/push cible pour relancer la CI distante.
7. Re-boucler tant que la CI expose une nouvelle cause racine.
8. Distinguer dependances opam et dependances systeme (C headers/libs) pour les bindings natifs.

## Procedure pratique
1. `gh run list --limit 8`
2. `gh run view <run_id> --log-failed`
3. Modifier `dune-project` et `<pkg>.opam`
4. `opam lint <pkg>.opam`
5. `dune build @all`
6. `git add ... && git commit && git push`

## Criteres de succes
- Plus d'erreur solveur opam sur `deps-of-tempo-jobs -> threads`.
- Plus d'erreur solveur opam sur `deps-of-tempo-fluidsynth -> unix`.
- Plus d'erreur compilation C sur `fluidsynth.h` manquant.
- Jobs CI `ubuntu-latest` et `macos-latest` passent le step `opam install . --deps-only --with-doc --with-test`.
