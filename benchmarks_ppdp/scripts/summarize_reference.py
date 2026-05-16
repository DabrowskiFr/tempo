#!/usr/bin/env python3
"""Summarize a frozen Tempo/ReactiveML benchmark pair for paper tables.

This script is intentionally explicit: it summarizes one chosen pair of CSV files
instead of auto-picking the latest run, so reported numbers remain stable.
"""

from __future__ import annotations

import argparse
import csv
import random
import statistics
from collections import defaultdict
from pathlib import Path
from typing import Dict, List, Sequence, Tuple

BENCH_ORDER = [
    "propagation_chains",
    "broadcast_expansion",
    "fork_explosion",
    "guarded_cascades",
    "nested_preemption",
]


def load_rows(path: Path) -> List[dict]:
    rows: List[dict] = []
    with path.open() as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            rows.append(
                {
                    "impl": row["impl"],
                    "benchmark": row["benchmark"],
                    "size": int(row["size"]),
                    "run": int(row["run"]),
                    "time_ms": float(row["time_ms"]),
                    "peak_mb": float(row["peak_mb"]),
                }
            )
    return rows


def percentile(values: Sequence[float], p: float) -> float:
    xs = sorted(values)
    if not xs:
        raise ValueError("percentile() requires non-empty values")
    if p <= 0.0:
        return xs[0]
    if p >= 1.0:
        return xs[-1]
    k = (len(xs) - 1) * p
    lo = int(k)
    hi = min(lo + 1, len(xs) - 1)
    if lo == hi:
        return xs[lo]
    w = k - lo
    return xs[lo] * (1.0 - w) + xs[hi] * w


def iqr(values: Sequence[float]) -> float:
    q = statistics.quantiles(sorted(values), n=4, method="inclusive")
    return q[2] - q[0]


def bootstrap_ratio_ci(
    tempo_vals: Sequence[float],
    rml_vals: Sequence[float],
    samples: int,
    seed: int,
) -> Tuple[float, float]:
    rng = random.Random(seed)
    nt = len(tempo_vals)
    nr = len(rml_vals)
    draws: List[float] = []
    for _ in range(samples):
        tb = [tempo_vals[rng.randrange(nt)] for _ in range(nt)]
        rb = [rml_vals[rng.randrange(nr)] for _ in range(nr)]
        draws.append(statistics.median(rb) / statistics.median(tb))
    return percentile(draws, 0.025), percentile(draws, 0.975)


def ensure_same_grid(tempo_rows: List[dict], rml_rows: List[dict]) -> List[int]:
    tempo_grid = {(r["benchmark"], r["size"]) for r in tempo_rows}
    rml_grid = {(r["benchmark"], r["size"]) for r in rml_rows}
    if tempo_grid != rml_grid:
        missing_in_rml = sorted(tempo_grid - rml_grid)
        missing_in_tempo = sorted(rml_grid - tempo_grid)
        raise ValueError(
            "Tempo/RML grids differ. "
            f"Missing in RML: {missing_in_rml[:5]}... "
            f"Missing in Tempo: {missing_in_tempo[:5]}..."
        )
    sizes = sorted({size for _, size in tempo_grid})
    for bench in BENCH_ORDER:
        if bench not in {b for b, _ in tempo_grid}:
            raise ValueError(f"Missing benchmark '{bench}' in input files")
    return sizes


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--tempo",
        type=Path,
        default=root / "data" / "raw" / "tempo-reference-good.csv",
    )
    parser.add_argument(
        "--rml",
        type=Path,
        default=root / "data" / "raw" / "rml-reference-good.csv",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=root / "data" / "processed",
    )
    parser.add_argument("--size", type=int, default=5000)
    parser.add_argument("--bootstrap-samples", type=int, default=20000)
    parser.add_argument("--seed", type=int, default=20260417)
    args = parser.parse_args()

    tempo_rows = load_rows(args.tempo)
    rml_rows = load_rows(args.rml)
    sizes = ensure_same_grid(tempo_rows, rml_rows)
    if args.size not in sizes:
        raise ValueError(f"Requested size {args.size} not found in dataset grid {sizes}")

    by_time: Dict[Tuple[str, str, int], List[float]] = defaultdict(list)
    by_peak: Dict[Tuple[str, str, int], List[float]] = defaultdict(list)
    for row in tempo_rows + rml_rows:
        key = (row["impl"], row["benchmark"], row["size"])
        by_time[key].append(row["time_ms"])
        by_peak[key].append(row["peak_mb"])

    args.out_dir.mkdir(parents=True, exist_ok=True)

    median_csv = args.out_dir / "reference-median-time.csv"
    memory_csv = args.out_dir / "reference-median-memory.csv"
    full_csv = args.out_dir / "reference-median-full.csv"
    summary_csv = args.out_dir / f"reference-summary-n{args.size}.csv"
    memory_summary_csv = args.out_dir / f"reference-memory-summary-n{args.size}.csv"
    disp_csv = args.out_dir / f"reference-dispersion-n{args.size}.csv"

    with median_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["impl", "benchmark", "size", "median_time_ms"])
        for impl in ["tempo", "rml"]:
            for bench in BENCH_ORDER:
                for size in sizes:
                    vals = by_time[(impl, bench, size)]
                    w.writerow([impl, bench, size, f"{statistics.median(vals):.6f}"])

    with memory_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["impl", "benchmark", "size", "median_peak_mb"])
        for impl in ["tempo", "rml"]:
            for bench in BENCH_ORDER:
                for size in sizes:
                    vals = by_peak[(impl, bench, size)]
                    w.writerow([impl, bench, size, f"{statistics.median(vals):.6f}"])

    with full_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(
            [
                "impl",
                "benchmark",
                "size",
                "median_time_ms",
                "iqr_time_ms",
                "median_peak_mb",
            ]
        )
        for impl in ["tempo", "rml"]:
            for bench in BENCH_ORDER:
                for size in sizes:
                    tvals = by_time[(impl, bench, size)]
                    pvals = by_peak[(impl, bench, size)]
                    w.writerow(
                        [
                            impl,
                            bench,
                            size,
                            f"{statistics.median(tvals):.6f}",
                            f"{iqr(tvals):.6f}",
                            f"{statistics.median(pvals):.6f}",
                        ]
                    )

    with summary_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(
            [
                "benchmark",
                "tempo_median_time_ms",
                "rml_median_time_ms",
                "tempo_over_rml_time",
                "ratio_rml_over_tempo",
                "tempo_iqr_time_ms",
                "rml_iqr_time_ms",
            ]
        )
        for bench in BENCH_ORDER:
            tvals = by_time[("tempo", bench, args.size)]
            rvals = by_time[("rml", bench, args.size)]
            tmed = statistics.median(tvals)
            rmed = statistics.median(rvals)
            w.writerow(
                [
                    bench,
                    f"{tmed:.6f}",
                    f"{rmed:.6f}",
                    f"{(tmed / rmed):.6f}",
                    f"{(rmed / tmed):.6f}",
                    f"{iqr(tvals):.6f}",
                    f"{iqr(rvals):.6f}",
                ]
            )

    with memory_summary_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(
            [
                "benchmark",
                "tempo_median_peak_mb",
                "rml_median_peak_mb",
                "tempo_over_rml_peak_mb",
                "ratio_rml_over_tempo_peak_mb",
            ]
        )
        for bench in BENCH_ORDER:
            tvals = by_peak[("tempo", bench, args.size)]
            rvals = by_peak[("rml", bench, args.size)]
            tmed = statistics.median(tvals)
            rmed = statistics.median(rvals)
            w.writerow(
                [
                    bench,
                    f"{tmed:.6f}",
                    f"{rmed:.6f}",
                    f"{(tmed / rmed):.6f}",
                    f"{(rmed / tmed):.6f}",
                ]
            )

    with disp_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(
            [
                "benchmark",
                "tempo_iqr_ms",
                "rml_iqr_ms",
                "ratio_rml_over_tempo",
                "ratio_ci95_low",
                "ratio_ci95_high",
            ]
        )
        for bench in BENCH_ORDER:
            tvals = by_time[("tempo", bench, args.size)]
            rvals = by_time[("rml", bench, args.size)]
            tmed = statistics.median(tvals)
            rmed = statistics.median(rvals)
            lo, hi = bootstrap_ratio_ci(
                tvals,
                rvals,
                samples=args.bootstrap_samples,
                seed=args.seed,
            )
            w.writerow(
                [
                    bench,
                    f"{iqr(tvals):.6f}",
                    f"{iqr(rvals):.6f}",
                    f"{(rmed / tmed):.6f}",
                    f"{lo:.6f}",
                    f"{hi:.6f}",
                ]
            )

    print(f"Tempo source: {args.tempo}")
    print(f"ReactiveML source: {args.rml}")
    print(f"Wrote {median_csv}")
    print(f"Wrote {memory_csv}")
    print(f"Wrote {full_csv}")
    print(f"Wrote {summary_csv}")
    print(f"Wrote {memory_summary_csv}")
    print(f"Wrote {disp_csv}")


if __name__ == "__main__":
    main()
