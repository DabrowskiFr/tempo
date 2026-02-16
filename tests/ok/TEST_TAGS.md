# Test Tags

Ce repertoire distingue deux familles:

- `tag=codee`: tests de semantique observable deterministe (API de haut niveau).
- `tag=low_level`: tests des primitives internes bas niveau (ex: `Low_level.with_kill`), non destinees a l'usage applicatif direct.

Regle:

- les tests `codee` verifient des traces observables equivalentes sans dependre de l'ordre d'execution intra-instant;
- les tests `low_level` verifient des proprietes minimales de surete/comportement sans pretendre a une equivalence par permutation des taches.
