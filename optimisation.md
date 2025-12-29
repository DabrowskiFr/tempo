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

1. `dune runtest`
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
