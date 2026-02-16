# Tempo Core Optimisation Plan

Ce document fixe le plan d'optimisation du runtime synchrone de Tempo a partir des benchs core.

## 1. Etat de reference

Mesures de reference (ns/op) observees dans `bench/results/core/core_bench.csv`:

- `await_immediate`: ~9426 a 9830 ns/op (hotspot principal)
- `when_`: ~4710 a 4826 ns/op (hotspot secondaire)
- `emit_await`: ~2022 a 2115 ns/op
- `pause_loop`: ~1883 a 1931 ns/op
- `combined_core`: ~1871 a 1910 ns/op
- `watch_preempt`: ~1272 a 1360 ns/op
- `parallel_fanout`: ~1231 a 2081 ns/op (derive forte avec le fanout)

Constats:

- la plupart des constructions sont lineaires et stables;
- `parallel_fanout` degrade avec le nombre de branches;
- les zones les plus couteuses sont `await_immediate` puis `when_`.

## 2. Priorites d'optimisation

Ordre de priorite:

1. `await_immediate`
2. `when_` / gestion des guards
3. `parallel` a fort fanout

Objectifs cibles:

- `await_immediate`: -25% a -40%
- `when_`: -15% a -30%
- `parallel_fanout`: reduire la derive du cout unitaire entre faible et fort fanout

## 3. Pistes techniques

### 3.1 `await_immediate`

- fast-path "signal deja present" avec moins d'allocations;
- reduction des allocations de task/closure sur reprise immediate;
- eviter les wrappers intermediaires quand le resultat peut etre pousse directement.

### 3.2 `when_`

- optimiser le cas mono-guard (cas majoritaire);
- reduire la construction de structures generiques de guards;
- limiter les recreations de tasks/wrappers lors des passages guardes.

### 3.3 `parallel`

- ajouter une orchestration interne fork-join "group" pour eviter la surcharge liste (`map` + `iter`);
- optimiser le chemin des join waiters quand fanout elevé;
- valider explicitement les seuils 32/64/96/128 branches.

## 4. Protocole de validation

Chaque optimisation doit etre validee par:

1. `dune runtest` (execution sequentielle; ne pas lancer plusieurs commandes `dune` en parallele)
2. benchmark core standard
3. benchmark core compare baseline

Commandes:

```bash
./tools/bench/run_core_bench.sh --out-dir bench/results/core-baseline
./tools/bench/run_core_bench.sh --out-dir bench/results/core-candidate \
  --baseline-csv bench/results/core-baseline/core_bench.csv \
  --fail-on-regression-pct 5
```

Sweep de charge:

```bash
./tools/bench/run_core_bench.sh --sweep 0.5,1,2 --log-y --out-dir bench/results/core-sweep
```

## 5. Criteres de non-regression

- aucune regression semantique sur `tests/ok`;
- aucune regression perf > 5% sur points compares (sinon echec CI);
- toute regression volontaire doit etre documentee avec justification.

## 6. Tests de protection ajoutes

Tests dedies (focus zones critiques):

- `await_immediate_parallel_order`
- `parallel_fanout_barrier`
- `watch_when_parallel_preempt`

Ces tests doivent rester verts avant/apres toute optimisation runtime.

## 7. Journal d'execution (branche `optimisation`)

Baseline de comparaison:

- `/Users/fredericdabrowski/Repos/tempo/bench/results/baseline-20260216-163831/core/core_bench.csv`

Etapes executees:

1. Etape 1: `parallel` optimise avec `join_many` interne.
2. Etape 2: optimisation du chemin guards (`kills_alive`, `guard_ok`, enregistrement des guards manquants en une passe).
3. Etape 3: optimisation complementaire guards (eviter la collecte detaillee des guards manquants quand inutile pour les logs).
4. Etape 4: stabilisation des mesures (`--stat median`) + optimisation du hot path `await_immediate`.
5. Etape 5: cache `thread_state` dans `task` (suppression des lookups `Hashtbl` chauds).
6. Etape 6: optimisation `when_guard` (pas de re-inscription quand deja bloque + fast path mono-guard).
7. Etape 7: allègement des chemins chauds `emit/await` (suppression de logs coûteux).
8. Etape 8: optimisation `parallel_fanout` (join_many fast paths + cache dense `thread_state` dans le scheduler).
9. Etape 9: reduction du coût de creation de tâches via `mk_task_with_state` (continuations intra-thread).
10. Etape 10: suppression du coût caché de logging scheduler (gardes explicites Debug/Info autour des snapshots/formatage).
11. Etape 11: désactivation des métriques runtime par défaut (activation explicite via `RML_METRICS`).
12. Etape 12: migration des waiters de signaux vers `Stack.t` mutable (drain LIFO sans reconstruction de listes).
13. Etape 13: fast paths `kills/guards` via métadonnées précalculées dans `task`.
14. Etape 14: suppression du coût d'appel de logs sur `enqueue_now`/`enqueue_next` (garde explicite Debug).
15. Etape 15: `log_ctx` lazy sur `handle_task` + consolidation des checks logs sur hot paths.
16. Etape 16: fast paths API `parallel` (petits fanouts + fork loop sans `List.map`).
17. Etape 17: fast paths `Join_many` dans l'effet handler pour petits fanouts (0/1/2).
18. Etape 18: fast path contigu `new_thread_id` / `thread_dense`.

Verification a chaque etape:

- `dune runtest` (OK)
- campagne bench complete avec comparaison baseline (`COMPARE.csv` / `COMPARE.md`)

Dossiers de resultats:

- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step1-20260216-164557`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step2-20260216-164719`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step3-20260216-164822`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-final-20260216-165015`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step12-20260216-173502`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step3-cache-threadstate-20260216-174216`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step6-when-guard-20260216-174736`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step7-await-logging-rerun-20260216-175122`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step8b-thread-dense-20260216-175455`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step9-mk-task-with-state-20260216-175743`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step10-log-guards-rerun-20260216-180044`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step11-metrics-gate-20260216-180313`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step12-waiters-stack-20260216-180942`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step13-task-flags-20260216-181334`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step14-enqueue-log-guards-rerun-20260216-181921`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step15-lazy-logctx-20260216-183139`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step16-parallel-api-fastpaths-20260216-184420`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step17-joinmany-small-fastpaths-20260216-184721`
- `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step18-thread-id-fastpath-20260216-184958`

Bilan synthetique (tendance moyenne observee sur les runs stables):

- `await_immediate`: amelioration marquee (jusqu'a ~`-14%` sur runs stables recents)
- `parallel_fanout`: amelioration moderee (jusqu'a ~`-5%`)
- `combined_core`: amelioration moderee a forte (jusqu'a ~`-7.5%`)
- la passe "log guard" montre un gain massif supplementaire (ordre de grandeur ~`-75%` a `-80%`) en supprimant des calculs de debug executes inutilement en mode non-debug.
- la passe "metrics gate" ajoute un gain massif complémentaire en retirant le coût `Mtime` + agrégation métrique du chemin nominal.
- la passe "waiters stack" consolide ces gains en évitant les reconstructions de listes de waiters tout en conservant l'ordre LIFO historique.
- la passe "task flags" retire le coût résiduel de parcours de listes sur les checks `kills/guards` du chemin chaud.
- la passe "enqueue log guards" retire un coût d'appel/logging encore présent sur les primitives les plus fréquentes.
- la passe "lazy log_ctx" retire le coût résiduel de construction de contexte de log sur `handle_task` quand aucun log n'est émis.
- la passe "parallel api fastpaths" réduit encore le coût de composition côté API sans changer la sémantique.
- la passe "join_many small fastpaths" consolide les gains sur les petits fanouts fréquents côté handler.
- la passe "thread id fastpath" réduit le coût d'allocation de threads logiques sur les rafales de forks.
- pas de regression semantique (tests OK)

Note de mesure:

- quelques points presentent des outliers ponctuels selon la charge machine;
- pour les comparaisons finales, privilegier plusieurs runs et la lecture des tendances (moyennes par benchmark) plutot qu'un unique point.
