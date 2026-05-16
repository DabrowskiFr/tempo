#!/usr/bin/env python3
"""Compare Tempo performance between current and locked baseline campaigns."""

from __future__ import annotations

import argparse
import csv
import math
import statistics
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Tuple

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


def grid(rows: List[dict]) -> set[Tuple[str, int]]:
    return {(r["benchmark"], r["size"]) for r in rows}


def ensure_same_grid(*datasets: List[dict]) -> List[int]:
    base = grid(datasets[0])
    for ds in datasets[1:]:
        if grid(ds) != base:
            raise ValueError("Input datasets do not share the same benchmark/size grid")
    sizes = sorted({size for _, size in base})
    for bench in BENCH_ORDER:
        if bench not in {b for b, _ in base}:
            raise ValueError(f"Missing benchmark '{bench}' in grid")
    return sizes


def median_map(rows: List[dict], metric: str) -> Dict[Tuple[str, int], float]:
    by_key: Dict[Tuple[str, int], List[float]] = defaultdict(list)
    for row in rows:
        by_key[(row["benchmark"], row["size"])].append(row[metric])
    return {k: statistics.median(v) for k, v in by_key.items()}


def geomean(xs: List[float]) -> float:
    return math.exp(sum(math.log(x) for x in xs) / len(xs))


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument("--current-tempo", type=Path, required=True)
    parser.add_argument("--current-rml", type=Path, required=True)
    parser.add_argument("--baseline-tempo", type=Path, required=True)
    parser.add_argument("--baseline-rml", type=Path, required=True)
    parser.add_argument("--baseline-id", required=True)
    parser.add_argument("--baseline-commit", required=True)
    parser.add_argument("--size", type=int, default=5000)
    parser.add_argument("--out-dir", type=Path, default=root / "data" / "processed")
    parser.add_argument("--tag", default="")
    args = parser.parse_args()

    cur_t_rows = load_rows(args.current_tempo)
    cur_r_rows = load_rows(args.current_rml)
    base_t_rows = load_rows(args.baseline_tempo)
    base_r_rows = load_rows(args.baseline_rml)

    sizes = ensure_same_grid(cur_t_rows, cur_r_rows, base_t_rows, base_r_rows)
    if args.size not in sizes:
        raise ValueError(f"Requested size {args.size} not found in grid {sizes}")

    cur_t_time = median_map(cur_t_rows, "time_ms")
    cur_r_time = median_map(cur_r_rows, "time_ms")
    base_t_time = median_map(base_t_rows, "time_ms")
    base_r_time = median_map(base_r_rows, "time_ms")

    cur_t_mem = median_map(cur_t_rows, "peak_mb")
    cur_r_mem = median_map(cur_r_rows, "peak_mb")
    base_t_mem = median_map(base_t_rows, "peak_mb")
    base_r_mem = median_map(base_r_rows, "peak_mb")

    args.out_dir.mkdir(parents=True, exist_ok=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    tag = f"-{args.tag}" if args.tag else ""
    summary_csv = args.out_dir / f"locked-compare-n{args.size}{tag}-{stamp}.csv"
    geomean_csv = args.out_dir / f"locked-compare-geomean{tag}-{stamp}.csv"

    with summary_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(
            [
                "benchmark",
                "baseline_id",
                "baseline_commit",
                "tempo_current_ms",
                "tempo_baseline_ms",
                "tempo_baseline_over_current",
                "rml_current_ms",
                "rml_baseline_ms",
                "ratio_rml_over_tempo_current",
                "ratio_rml_over_tempo_baseline",
                "tempo_current_peak_mb",
                "tempo_baseline_peak_mb",
                "tempo_baseline_over_current_peak_mb",
                "rml_current_peak_mb",
                "rml_baseline_peak_mb",
                "ratio_rml_over_tempo_current_peak_mb",
                "ratio_rml_over_tempo_baseline_peak_mb",
            ]
        )
        for bench in BENCH_ORDER:
            t_cur = cur_t_time[(bench, args.size)]
            t_base = base_t_time[(bench, args.size)]
            r_cur = cur_r_time[(bench, args.size)]
            r_base = base_r_time[(bench, args.size)]
            m_cur = cur_t_mem[(bench, args.size)]
            m_base = base_t_mem[(bench, args.size)]
            mr_cur = cur_r_mem[(bench, args.size)]
            mr_base = base_r_mem[(bench, args.size)]
            w.writerow(
                [
                    bench,
                    args.baseline_id,
                    args.baseline_commit,
                    f"{t_cur:.6f}",
                    f"{t_base:.6f}",
                    f"{(t_base / t_cur):.6f}",
                    f"{r_cur:.6f}",
                    f"{r_base:.6f}",
                    f"{(r_cur / t_cur):.6f}",
                    f"{(r_base / t_base):.6f}",
                    f"{m_cur:.6f}",
                    f"{m_base:.6f}",
                    f"{(m_base / m_cur):.6f}",
                    f"{mr_cur:.6f}",
                    f"{mr_base:.6f}",
                    f"{(mr_cur / m_cur):.6f}",
                    f"{(mr_base / m_base):.6f}",
                ]
            )

    with geomean_csv.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(
            [
                "size",
                "baseline_id",
                "baseline_commit",
                "geomean_tempo_baseline_over_current",
                "geomean_ratio_rml_over_tempo_current",
                "geomean_ratio_rml_over_tempo_baseline",
                "geomean_tempo_baseline_over_current_peak_mb",
                "geomean_ratio_rml_over_tempo_current_peak_mb",
                "geomean_ratio_rml_over_tempo_baseline_peak_mb",
            ]
        )
        for size in sizes:
            tempo_ratio = []
            rml_cur_ratio = []
            rml_base_ratio = []
            tempo_mem_ratio = []
            rml_cur_mem_ratio = []
            rml_base_mem_ratio = []
            for bench in BENCH_ORDER:
                tempo_ratio.append(base_t_time[(bench, size)] / cur_t_time[(bench, size)])
                rml_cur_ratio.append(cur_r_time[(bench, size)] / cur_t_time[(bench, size)])
                rml_base_ratio.append(base_r_time[(bench, size)] / base_t_time[(bench, size)])
                tempo_mem_ratio.append(base_t_mem[(bench, size)] / cur_t_mem[(bench, size)])
                rml_cur_mem_ratio.append(cur_r_mem[(bench, size)] / cur_t_mem[(bench, size)])
                rml_base_mem_ratio.append(base_r_mem[(bench, size)] / base_t_mem[(bench, size)])
            w.writerow(
                [
                    size,
                    args.baseline_id,
                    args.baseline_commit,
                    f"{geomean(tempo_ratio):.6f}",
                    f"{geomean(rml_cur_ratio):.6f}",
                    f"{geomean(rml_base_ratio):.6f}",
                    f"{geomean(tempo_mem_ratio):.6f}",
                    f"{geomean(rml_cur_mem_ratio):.6f}",
                    f"{geomean(rml_base_mem_ratio):.6f}",
                ]
            )

    print(f"Wrote {summary_csv}")
    print(f"Wrote {geomean_csv}")


if __name__ == "__main__":
    main()
