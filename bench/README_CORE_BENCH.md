# Tempo Core Bench

Suite de benchmarks parametriques pour le noyau synchrone de Tempo.

Objectifs:
- mesurer les primitives du core individuellement;
- mesurer une composition de primitives;
- faire varier automatiquement la complexite;
- exporter des graphes de performances.

## Benchmarks inclus

- `pause_loop`: cout de `pause ()`.
- `emit_await`: cout de `emit` + `await` sur signal evenementiel.
- `await_immediate`: handshake intra-instant avec `await_immediate`.
- `when_guard`: cout d'un guard `when_`.
- `watch_preempt`: cout d'une preemption `watch`.
- `parallel_fanout`: impact du nombre de branches `parallel`.
- `combined_core`: scenario combine (`emit`, `await`, `when_`, `watch`, `parallel`, `pause`).

## Lancement

Depuis la racine du projet:

```bash
dune exec ./bench/core_bench.exe -- --out-dir bench/results/core
```

Ou via script:

```bash
./tools/bench/run_core_bench.sh --out-dir bench/results/core
```

Le script convertit aussi automatiquement tous les graphes `.svg` en `.pdf`.
Convertisseur utilise (ordre): `rsvg-convert`, `inkscape`, puis `cairosvg`.

## Parametres

- `--out-dir <dir>`: repertoire de sortie.
- `--repeats <n>`: repetitions par point (defaut `5`).
- `--warmup <n>`: warmups par point (defaut `1`).
- `--stat <mean|median>`: statistique sur les repetitions (defaut `median`).
- `--scale <f>`: facteur global de complexite (defaut `1.0`).
- `--only <id>`: un seul benchmark.
- `--log-y`: ajoute des graphes SVG en echelle logarithmique sur l'axe Y.
- `--sweep "s1,s2,..."`: lance plusieurs campagnes de complexite (ex: `0.5,1,2`).
- `--baseline-csv <path>`: compare les mesures courantes a un `core_bench.csv` de reference.
- `--fail-on-regression-pct <p>`: echec si une regression depasse `p` pourcents.

Exemple rapide:

```bash
./tools/bench/run_core_bench.sh --scale 0.3 --repeats 2 --warmup 0
```

Exemple sweep:

```bash
./tools/bench/run_core_bench.sh --sweep 0.5,1,2 --repeats 3 --log-y --out-dir bench/results/core-sweep
```

Exemple comparaison baseline:

```bash
./tools/bench/run_core_bench.sh --out-dir bench/results/core-baseline
./tools/bench/run_core_bench.sh --out-dir bench/results/core-candidate \
  --baseline-csv bench/results/core-baseline/core_bench.csv \
  --fail-on-regression-pct 5
```

## Sorties generees

- `core_bench.csv`: mesures brutes.
- `<bench>_total_ms.svg`: graphe `temps total` vs complexite.
- `core_per_op_ns.svg`: graphe compare `ns/op` normalise.
- `*_log.svg`: versions log Y si `--log-y`.
- versions `.pdf` de tous les graphes SVG (generation automatique par le script).
- `REPORT.md`: resume automatique.
- `SWEEP.md`: index des campagnes si `--sweep`.
- `COMPARE.csv` et `COMPARE.md`: deltas vs baseline si `--baseline-csv`.
