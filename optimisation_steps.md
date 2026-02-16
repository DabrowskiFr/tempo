# Synthèse des étapes d'optimisation (branche `optimisation`)

Ce document résume, étape par étape, les optimisations réalisées sur Tempo et les gains observés via les benchs core.

## Référence de baseline

- Baseline principale:
  `/Users/fredericdabrowski/Repos/tempo/bench/results/baseline-20260216-163831/core/core_bench.csv`

## Méthode de validation utilisée

À chaque étape:

1. `dune runtest` (non-régression sémantique)
2. campagne bench core comparée à la baseline (`COMPARE.csv` / `COMPARE.md`)
3. lecture des deltas moyens par benchmark + pire régression ponctuelle

---

## Étape 1 — Optimisation `parallel` (join group)

### Changement

- Introduction d'un `Join_many` interne pour réduire la surcharge de synchronisation dans `parallel`.
- Fichiers:
  - `lib/tempo.ml`
  - `lib/tempo_types.ml`
  - `lib/tempo_types.mli`

### Résultats

- Run: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step1-20260216-164557`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-8.125%`
- `combined_core`: `-1.925%`
- `emit_await`: `-0.322%`
- `parallel_fanout`: `+0.155%` (quasi neutre)
- `pause_loop`: `-0.357%`
- `watch_preempt`: `+2.634%`
- `when_guard`: `+1.107%`

Pire régression ponctuelle:

- `watch_preempt` (complexité 300): `+15.624%`

Remarque:

- Gain net déjà visible sur `await_immediate`, mais bruit notable sur les petits cas (`watch_preempt`).

---

## Étape 2 — Optimisation chemin guards (`when_`)

### Changement

- Fast paths:
  - `kills_alive` (cas `[]` / singleton)
  - `guard_ok` (cas `[]` / singleton)
- Enregistrement des guards manquants en une passe.
- Fichier: `lib/tempo.ml`

### Résultats

- Run: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step2-20260216-164719`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-8.407%`
- `combined_core`: `-2.862%`
- `emit_await`: `-1.071%`
- `parallel_fanout`: `-1.468%`
- `pause_loop`: `-0.163%`
- `watch_preempt`: `-0.250%`
- `when_guard`: `+0.947%`

Pire régression ponctuelle:

- `watch_preempt` (complexité 3000): `+2.477%`

Remarque:

- Étape globalement plus stable que l’étape 1.

---

## Étape 3 — Optimisation guards additionnelle (logs)

### Changement

- Évite la collecte détaillée des guards manquants quand elle n’est pas utile.
- Fichier: `lib/tempo.ml`

### Résultats

- Run: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step3-20260216-164822`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-7.843%`
- `combined_core`: `-0.826%`
- `emit_await`: `+1.533%`
- `parallel_fanout`: `-1.535%`
- `pause_loop`: `-0.176%`
- `watch_preempt`: `-1.324%`
- `when_guard`: `+0.872%`

Pire régression ponctuelle:

- `when_guard` (complexité 500): `+2.887%`

Remarque:

- Gain sur `await_immediate` maintenu, `when_guard` reste à surveiller.

---

## Étape intermédiaire rejetée

Une tentative d’optimisation de `With_guard` (enchaîner `continue k ()` sans tâche intermédiaire) a modifié l’ordre observable dans `when_parallel` et a été annulée.

Conclusion:

- optimisation refusée pour préserver la sémantique testée.

---

## Étape 4 — Stabilisation de la mesure + optimisation `await_immediate`

### Changement A (mesure)

- Ajout de `--stat mean|median` dans le bench (`median` par défaut) pour réduire les outliers.
- Fichier: `bench/core_bench.ml`
- Doc: `bench/README_CORE_BENCH.md`

### Changement B (runtime `await_immediate`)

- Allègement du chemin chaud `Await_immediate` (retrait de coût non essentiel sur ce chemin).
- Fichier: `lib/tempo.ml`

### Résultats (run stable)

- Run: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step12-20260216-173502`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-9.807%`
- `combined_core`: `-3.978%`
- `emit_await`: `-1.027%`
- `parallel_fanout`: `-2.701%`
- `pause_loop`: `-1.385%`
- `watch_preempt`: `-3.950%`
- `when_guard`: `+0.222%` (quasi neutre)

Pire régression ponctuelle:

- `pause_loop` (complexité 20000): `+2.054%`

Meilleure amélioration ponctuelle:

- `await_immediate` (complexité 200): `-12.126%`

---

## Bilan global actuel

- Pas de régression sémantique (`dune runtest` vert à chaque étape retenue).
- Gain robuste sur le hotspot principal:
  - `await_immediate` autour de `-8%` à `-10%` (jusqu’à `-12%` selon points).
- Gain mesurable sur:
  - `parallel_fanout` (léger à modéré),
  - `combined_core`.
- `when_guard` reste proche du neutre (légères fluctuations selon run).

## Prochaine recommandation

- Conserver `--stat median` pour toute comparaison.
- Continuer sur optimisations ciblées `await_immediate` (réduction allocations), puis `parallel` gros fanout, avec seuil de régression strict (ex: `5%`) sur campagnes stables.

---

## Étape 5 — Cache `thread_state` dans `task` (suppression des lookups chauds)

### Changement

- Ajout d’un champ `thread_state` dans `task` pour éviter les `Hashtbl.find` répétés sur le cycle de vie des tâches.
- Le runtime met désormais à jour/consomme directement `t.thread_state` sur les chemins chauds:
  - création de tâche (`mk_task`)
  - fin de tâche (`finish_task_state`)
  - élimination des tâches tuées dans `current` et `next_instant`
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_types.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_types.mli`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_task.ml`

### Résultats

- Run: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step3-cache-threadstate-20260216-174216`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas observés vs baseline (tendance):

- `await_immediate`: amélioration forte, jusqu’à `-13.374%` (meilleur point), autour de `-9%` à `-11%` sur la famille.
- `parallel_fanout`: amélioration stable (`~ -2.3%` à `-4.3%`).
- `combined_core`: amélioration stable (`~ -2.3%` à `-5.6%`).
- `emit_await`: amélioration légère à modérée (`~ -1%` à `-5.7%`).
- `watch_preempt`: amélioration modérée (`~ -2.1%` à `-6.3%`).
- `when_guard`: quasi neutre (léger +/- bruit, proche 0).

Pire régression ponctuelle:

- `pause_loop` (complexité 1000): `+0.475%` (négligeable).

---

## Étape 6 — Optimisation ciblée `when_guard` (désinscription implicite + fast path)

### Changement

- `block_on_guards` n’enregistre plus les guards manquants si la tâche est déjà `blocked` dans le même instant (évite les doublons de waiters).
- Ajout d’un fast path mono-guard sans parcours de liste.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step6-when-guard-20260216-174736`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-9.343%`
- `combined_core`: `-3.366%`
- `emit_await`: `-1.265%`
- `parallel_fanout`: `-3.099%`
- `pause_loop`: `-1.566%`
- `watch_preempt`: `-2.731%`
- `when_guard`: `-0.214%`

Pire régression ponctuelle:

- `pause_loop` (complexité 5000): `+2.304%`

---

## Étape 7 — Allègement hot path `emit/await` (suppression logs coûteux)

### Changement

- Suppression de logs coûteux sur chemins très chauds (`emit_event_from_host`, `Emit` event/agg, `Await`), afin de réduire le coût CPU sans impact sémantique.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step7-await-logging-rerun-20260216-175122`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-11.226%`
- `combined_core`: `-3.815%`
- `emit_await`: `-3.591%`
- `parallel_fanout`: `-2.478%`
- `pause_loop`: `-0.742%`
- `watch_preempt`: `-2.006%`
- `when_guard`: `-1.252%`

Pire régression ponctuelle:

- `pause_loop` (complexité 5000): `+2.052%`

---

## Étape 8 — Optimisation `parallel_fanout` (`join_many` + cache dense des threads)

### Changement

- `Join_many`:
  - fast paths `[]` et singleton,
  - suppression du double parcours (`exists` + `iter`) en parcours unique.
- Scheduler:
  - ajout d’un cache dense `thread_dense` (tableau indexé par `thread id`) pour éviter les lookups hashtable sur `find_thread_state`.
  - `ensure_thread_state` gère désormais l’allocation/croissance du cache dense.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step8b-thread-dense-20260216-175455`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-13.700%`
- `combined_core`: `-7.490%`
- `emit_await`: `-7.560%`
- `parallel_fanout`: `-4.969%`
- `pause_loop`: `-3.553%`
- `watch_preempt`: `-5.279%`
- `when_guard`: `-4.157%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-0.326%`).

---

## Étape 9 — Réduction du coût de création de tâches (réutilisation `thread_state`)

### Changement

- Ajout de `mk_task_with_state` pour éviter la recherche de l’état de thread lors de la création de continuations dans le même thread.
- Remplacement des créations de tâches de continuation dans `handle_task` par la variante avec `thread_state` déjà connu.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step9-mk-task-with-state-20260216-175743`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-13.157%`
- `combined_core`: `-6.257%`
- `emit_await`: `-5.858%`
- `parallel_fanout`: `-3.627%`
- `pause_loop`: `-3.553%`
- `watch_preempt`: `-3.603%`
- `when_guard`: `-3.249%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-1.630%`).

---

## Étape 10 — Garde stricte du coût de logging dans le scheduler

### Changement

- Dans `step` et `run_instant`, ajout de gardes explicites par niveau de log (`Info`/`Debug`) pour ne plus calculer:
  - snapshots de files (`snapshot_queue`),
  - rendu détaillé d’états de signaux/files,
  - formatage associé,
  lorsque le niveau ne consomme pas ces informations.
- Remplacement de constructions de messages intermédiaires par des appels formatés directs.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step10-log-guards-rerun-20260216-180044`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-77.785%`
- `combined_core`: `-76.643%`
- `emit_await`: `-75.702%`
- `parallel_fanout`: `-81.337%`
- `pause_loop`: `-76.912%`
- `watch_preempt`: `-75.850%`
- `when_guard`: `-76.390%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-72.245%`).

---

## Étape 11 — Désactivation du coût métrique par défaut (`RML_METRICS`)

### Changement

- Ajout d’un flag runtime `metrics_enabled` dans `Tempo_log`, activé uniquement via `RML_METRICS=1|true|yes|on`.
- `record_duration` et `log_duration_summary` deviennent no-op quand les métriques sont désactivées.
- Dans `step`/`run_instant`, les compteurs `Mtime_clock.counter/count` ne sont créés/évalués que si les métriques sont actives.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_log.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step11-metrics-gate-20260216-180313`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-84.877%`
- `combined_core`: `-85.231%`
- `emit_await`: `-83.701%`
- `parallel_fanout`: `-87.349%`
- `pause_loop`: `-86.271%`
- `watch_preempt`: `-85.784%`
- `when_guard`: `-85.065%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-83.054%`).

---

## Étape 12 — Waiters des signaux en structure mutable LIFO (`Stack.t`)

### Changement

- `awaiters` et `guard_waiters` des signaux passent de listes à `Stack.t` mutables.
- Drain explicite via `Stack.pop`/`Stack.is_empty`, sans reconstruction de liste.
- L’ordre observable historique (LIFO) est conservé pour les tests qui impriment.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_types.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_types.mli`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_signal.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_task.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_engine.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_log.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step12-waiters-stack-20260216-180942`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-84.756%`
- `combined_core`: `-85.569%`
- `emit_await`: `-85.029%`
- `parallel_fanout`: `-87.397%`
- `pause_loop`: `-86.282%`
- `watch_preempt`: `-85.492%`
- `when_guard`: `-85.224%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-84.048%`).

---

## Étape 13 — Fast paths `kills/guards` via métadonnées de tâche

### Changement

- Ajout dans `task` de métadonnées pré-calculées:
  - `has_kills`
  - `guard_single`
- Utilisation de `task_kills_alive` / `task_guard_ok` sur les chemins chauds, pour éviter des parcours de listes répétitifs.
- Application dans le runtime principal et les modules auxiliaires (`tempo_task`).
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_types.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_types.mli`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo_task.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step13-task-flags-20260216-181334`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-85.020%`
- `combined_core`: `-85.284%`
- `emit_await`: `-84.995%`
- `parallel_fanout`: `-87.521%`
- `pause_loop`: `-86.339%`
- `watch_preempt`: `-85.561%`
- `when_guard`: `-85.223%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-83.983%`).

---

## Étape 14 — Suppression du coût d’appel de logs dans `enqueue_*`

### Changement

- Dans `enqueue_now` et `enqueue_next`, les logs chauds sont maintenant entièrement court-circuités via un garde explicite `Tempo_log.level_enabled Logs.Debug`.
- Cela évite l’appel à `Tempo_log.log` et le coût de formatage associé quand les logs Debug ne sont pas actifs.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step14-enqueue-log-guards-rerun-20260216-181921`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-87.084%`
- `combined_core`: `-89.216%`
- `emit_await`: `-88.077%`
- `parallel_fanout`: `-90.894%`
- `pause_loop`: `-88.567%`
- `watch_preempt`: `-89.509%`
- `when_guard`: `-88.326%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.198%`).

---

## Étape 15 — `log_ctx` lazy sur les hot paths de `handle_task`

### Changement

- Dans `handle_task`, introduction de wrappers de log (`debug_log`, `guard_log`) avec:
  - garde par niveau (`Debug`) / tracing guards,
  - construction lazy de `log_ctx st` uniquement si un log est effectivement émis.
- Ajustement complémentaire:
  - `enqueue_next` évite de recalculer plusieurs fois `level_enabled Debug` et `log_ctx`.
  - `block_on_guards` construit un `ctx` unique en branche détaillée.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step15-lazy-logctx-20260216-183139`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-86.805%`
- `combined_core`: `-89.293%`
- `emit_await`: `-88.338%`
- `parallel_fanout`: `-91.305%`
- `pause_loop`: `-88.950%`
- `watch_preempt`: `-89.705%`
- `when_guard`: `-88.502%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-85.991%`).

---

## Étape 16 — Fast paths API `parallel` (petits fanouts + boucle sans `List.map`)

### Changement

- `parallel`:
  - fast paths explicites pour 2/3/4 branches,
  - chemin général via boucle récursive `fork_all_rev` (suppression de `List.map fork`).
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step16-parallel-api-fastpaths-20260216-184420`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-87.202%`
- `combined_core`: `-89.330%`
- `emit_await`: `-88.146%`
- `parallel_fanout`: `-91.335%`
- `pause_loop`: `-88.935%`
- `watch_preempt`: `-89.786%`
- `when_guard`: `-88.468%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.227%`).

---

## Étape 17 — Fast paths `Join_many` dans l’effet handler (0/1/2 threads)

### Changement

- Dans `effect (Join_many ...)`:
  - spécialisation explicite des cas `[]`, singleton et paire de threads,
  - réduction du coût du chemin générique (`remaining/resumed`) sur petits fanouts.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step17-joinmany-small-fastpaths-20260216-184721`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-87.359%`
- `combined_core`: `-89.240%`
- `emit_await`: `-88.202%`
- `parallel_fanout`: `-91.216%`
- `pause_loop`: `-89.010%`
- `watch_preempt`: `-89.877%`
- `when_guard`: `-88.665%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.367%`).

---

## Étape 18 — Fast path contigu pour `new_thread_id` / `thread_dense`

### Changement

- Ajout d’un helper `ensure_thread_dense_capacity`.
- `new_thread_id` utilise un fast path contigu:
  - allocation/capacité dense directe,
  - création immédiate du `thread_state` dans `thread_dense` + indexation table.
- `ensure_thread_state` est simplifié autour de cette capacité.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/opt-step18-thread-id-fastpath-20260216-184958`
- Paramètres: `--stat median --repeats 7 --warmup 2`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-87.000%`
- `combined_core`: `-89.258%`
- `emit_await`: `-88.280%`
- `parallel_fanout`: `-91.343%`
- `pause_loop`: `-89.062%`
- `watch_preempt`: `-89.774%`
- `when_guard`: `-88.546%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-85.810%`).

---

## Étape 19 — Refactoring maintenabilité: module `Instant_rollover`

### Changement

- Extraction de la logique de fin d’instant dans `Instant_rollover`:
  - `schedule_unblocked_for_next`: transfert explicite des tâches bloquées vers l’instant suivant.
  - `drain_survivors_for_next_instant`: filtrage des tâches tuées et préparation de la file `current`.
- Simplification de `Scheduler_step.run_instant`:
  - orchestration plus lisible du cycle `finalize -> rollover -> recurse`.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_internals.md`
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_source_of_truth.md`

### Résultats

- Run initial (bruité): `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase5-instant-rollover-20260216-190951/core`
- Run validé (rerun): `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase5-instant-rollover-rerun-20260216-191013/core`
- `dune runtest`: OK

Deltas moyens vs baseline (run validé):

- `await_immediate`: `-86.914%`
- `combined_core`: `-89.280%`
- `emit_await`: `-88.185%`
- `parallel_fanout`: `-91.036%`
- `pause_loop`: `-89.217%`
- `watch_preempt`: `-89.719%`
- `when_guard`: `-88.414%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.213%`).

---

## Étape 20 — Refactoring maintenabilité: module `Runtime_bootstrap`

### Changement

- Extraction de la création d’état runtime + branchement I/O hôte:
  - `create_state`
  - `create_io_hooks`
  - `enqueue_root`
- Déduplication de `execute` et `execute_inspect` autour de ce bootstrap partagé.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_internals.md`
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_source_of_truth.md`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase6-runtime-bootstrap-20260216-191235/core`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-87.007%`
- `combined_core`: `-89.226%`
- `emit_await`: `-88.064%`
- `parallel_fanout`: `-91.195%`
- `pause_loop`: `-89.016%`
- `watch_preempt`: `-89.614%`
- `when_guard`: `-88.509%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.111%`).

---

## Étape 21 — Refactoring maintenabilité: helpers d’observation (`execute_trace` / `execute_timeline`)

### Changement

- Ajout de `Execution_helpers` pour la couche API:
  - `resolve_instants` (politique unique de résolution des instants),
  - `make_input_cursor` (consommation séquentielle des entrées).
- Déduplication des chemins `execute_trace` et `execute_timeline`.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase7-api-observe-helpers-20260216-191435/core`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-87.086%`
- `combined_core`: `-89.359%`
- `emit_await`: `-88.296%`
- `parallel_fanout`: `-91.322%`
- `pause_loop`: `-89.020%`
- `watch_preempt`: `-89.750%`
- `when_guard`: `-88.391%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.139%`).

---

## Étape 22 — Maintenabilité: tests d’invariants runtime observables

### Changement

- Ajout de tests dédiés aux invariants de fin d’instant via `execute_inspect`:
  - `runtime_instant_observable_order`
  - `runtime_instant_current_empty`
- Les tests vérifient uniquement la sémantique observable (snapshots / sorties), sans dépendre de l’ordre interne des tâches.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/runtime_instant_observable_order.ml`
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/runtime_instant_current_empty.ml`
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/dune`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase8-runtime-invariant-tests-20260216-191657/core`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-86.852%`
- `combined_core`: `-89.123%`
- `emit_await`: `-87.970%`
- `parallel_fanout`: `-91.245%`
- `pause_loop`: `-89.271%`
- `watch_preempt`: `-89.705%`
- `when_guard`: `-88.389%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.260%`).

---

## Étape 23 — Maintenabilité: documentation invariants + anti-patterns runtime

### Changement

- Renforcement de la documentation contributeur runtime:
  - mapping explicite invariants -> tests observables,
  - section anti-patterns runtime,
  - checklist anti-drift avant merge.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_internals.md`
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_source_of_truth.md`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase9-runtime-doc-invariants-20260216-191828/core`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-87.031%`
- `combined_core`: `-89.335%`
- `emit_await`: `-88.305%`
- `parallel_fanout`: `-91.241%`
- `pause_loop`: `-89.012%`
- `watch_preempt`: `-89.682%`
- `when_guard`: `-88.642%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.052%`).

---

## Étape 24 — Refactoring maintenabilité: module `Task_builder`

### Changement

- Extraction de la construction des tâches dans `Task_builder`:
  - `guard_single`
  - `mk_task_with_state`
  - `mk_task`
- Contrat explicite: l’enregistrement de possession thread (`register_task_state`) est centralisé dans le constructeur.
- Mise à jour de la cartographie runtime dans la documentation.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_internals.md`
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_source_of_truth.md`

### Résultats

- Run initial (bruité): `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase10-task-builder-20260216-192020/core`
- Run validé (rerun): `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase10-task-builder-rerun-20260216-192035/core`
- `dune runtest`: OK

Deltas moyens vs baseline (run validé):

- `await_immediate`: `-86.911%`
- `combined_core`: `-89.360%`
- `emit_await`: `-88.151%`
- `parallel_fanout`: `-91.274%`
- `pause_loop`: `-88.976%`
- `watch_preempt`: `-89.629%`
- `when_guard`: `-88.438%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-85.908%`).

---

## Étape 25 — Ergonomie Layer 2: namespace `State` + factorisation `Timeline_json`

### Changement

- Ajout d’un namespace `State` (couche 2), pour éviter le flat API:
  - `State.create/get/set/modify/await/update_and_get`
- Factorisation de `Timeline_json`:
  - helper commun `json_string_or_null`,
  - nouvelle API nommée `of_timeline_with`,
  - compatibilité conservée via `of_timeline` (alias).
- Export Layer2 enrichi:
  - `Layer2.State`
- Tests API adaptés pour couvrir les nouveaux chemins:
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/state_api.ml`
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/timeline_json_api.ml`
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.mli`
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/state_api.ml`
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/timeline_json_api.ml`

### Résultats

- Run initial: `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase11-layer2-state-timeline-helpers-20260216-192325/core`
- Run validé (rerun2): `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase11-layer2-state-timeline-helpers-rerun2-20260216-192354/core`
- `dune runtest`: OK

Deltas moyens vs baseline (run validé):

- `await_immediate`: `-86.828%`
- `combined_core`: `-89.154%`
- `emit_await`: `-88.129%`
- `parallel_fanout`: `-91.193%`
- `pause_loop`: `-88.720%`
- `watch_preempt`: `-89.346%`
- `when_guard`: `-88.431%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-85.522%`).

---

## Étape 26 — Documentation Layer 2: `State` et `Timeline_json.of_timeline_with`

### Changement

- Mise à jour des guides API pour refléter la couche 2 enrichie:
  - ajout de `State.*` dans la documentation principale,
  - ajout de `Timeline_json.of_timeline_with` (arguments nommés),
  - exemple dédié `State` + timeline JSON.
- Fichiers:
  - `/Users/fredericdabrowski/Repos/tempo/Tempo.md`
  - `/Users/fredericdabrowski/Repos/tempo/Tempo_EN.md`
  - `/Users/fredericdabrowski/Repos/tempo/docs/GAME_QUICKSTART.md`

### Résultats

- Run validé: `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase12-doc-layer2-state-timeline-20260216-192553/core`
- `dune runtest`: OK

Deltas moyens vs baseline:

- `await_immediate`: `-86.948%`
- `combined_core`: `-89.177%`
- `emit_await`: `-88.127%`
- `parallel_fanout`: `-91.131%`
- `pause_loop`: `-88.892%`
- `watch_preempt`: `-89.768%`
- `when_guard`: `-88.360%`

Pire régression ponctuelle:

- Aucune régression ponctuelle (worst point à `-86.110%`).

---

## Étape 27 — Simplification ciblée: suppression de fast paths `parallel`/`Join_many` peu rentables

### Changement

- Simplification de `Join_many.wait_for_threads`:
  - suppression du cas spécialisé paire (`[tid1; tid2]`),
  - conservation des cas triviaux (`[]`, singleton) + chemin générique lisible.
- Simplification de `parallel`:
  - suppression des branches spécialisées 2/3/4,
  - conservation de `[]`, singleton, puis chemin générique unique.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run initial: `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase13-simplify-parallel-joinmany-20260216-192856/core`
- Run validé (rerun): `/Users/fredericdabrowski/Repos/tempo/bench/results/refactor-phase13-simplify-parallel-joinmany-rerun-20260216-192913/core`
- `dune runtest`: OK

Deltas moyens vs baseline (run validé):

- `await_immediate`: `-86.816%`
- `combined_core`: `-89.255%`
- `emit_await`: `-88.220%`
- `parallel_fanout`: `-91.288%`
- `pause_loop`: `-88.974%`
- `watch_preempt`: `-89.341%`
- `when_guard`: `-88.457%`

Interprétation ratio gain/perte:

- Impact perf global: quasi neutre (variations dans le bruit de mesure).
- Impact maintenabilité: favorable (moins de branches spécialisées, lecture plus simple).
- Décision: conserver la simplification.

---

## Étape 28 — A/B ciblé “steps 1–3”: simplification des fast paths guards/logging

### Changement

- Variante de simplification appliquée à la logique héritée des étapes 1–3:
  - `Task_fastpath.kills_alive` simplifié (suppression cas singleton dédié),
  - `Task_fastpath.guard_ok` simplifié (suppression cas singleton dédié),
  - `Guard_waiters.block_on_guards` unifié: enregistrement des guards manquants via un seul chemin,
    suppression des helpers `*_no_collect`.
- Fichier:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`

### Résultats

- Run A/B 1: `/Users/fredericdabrowski/Repos/tempo/bench/results/ab-step1-3-simplified-20260216-193150/core`
- Run A/B 2 (validation): `/Users/fredericdabrowski/Repos/tempo/bench/results/ab-step1-3-simplified-rerun-20260216-193217/core`
- `dune runtest`: OK

Deltas moyens vs baseline (run A/B 1):

- `await_immediate`: `-86.800%`
- `combined_core`: `-89.129%`
- `emit_await`: `-88.247%`
- `parallel_fanout`: `-91.328%`
- `pause_loop`: `-88.922%`
- `watch_preempt`: `-89.564%`
- `when_guard`: `-88.453%`

Deltas moyens vs baseline (run A/B 2):

- `await_immediate`: `-86.454%`
- `combined_core`: `-89.156%`
- `emit_await`: `-87.949%`
- `parallel_fanout`: `-91.085%`
- `pause_loop`: `-88.992%`
- `watch_preempt`: `-89.630%`
- `when_guard`: `-88.402%`

Interprétation ratio gain/perte:

- Perfs: globalement dans le bruit de mesure par rapport à l’état précédent (légère baisse potentielle sur `await_immediate`).
- Maintenabilité: meilleure (moins de branches et de chemins spéciaux, code plus lisible).
- Décision: conserver la simplification, avec surveillance de `await_immediate` lors des prochaines passes d’optimisation.

---

## Somme cumulée des gains (par point de benchmark)

Convention de calcul:

- `gain_incrémental(étape n) = delta_vs_baseline(étape n) - delta_vs_baseline(étape n-1)`.
- `gain_cumulé = somme des gains incrémentaux` (de l'étape 1 à l'étape courante).

Avec cette convention, le gain cumulé à l'étape 28 est exactement égal au delta mesuré vs baseline à l'étape 28.

| benchmark | gain cumulé (%) |
|---|---:|
| `await_immediate` | `-86.454` |
| `combined_core` | `-89.156` |
| `emit_await` | `-87.949` |
| `parallel_fanout` | `-91.085` |
| `pause_loop` | `-88.992` |
| `watch_preempt` | `-89.630` |
| `when_guard` | `-88.402` |

Note:
- valeur négative: amélioration cumulée (coût en baisse);
- valeur positive: dégradation cumulée.
