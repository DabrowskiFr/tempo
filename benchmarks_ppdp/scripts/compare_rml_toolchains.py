#!/usr/bin/env python3
"""Compare ReactiveML benchmark campaigns across two toolchains."""

from __future__ import annotations

import argparse
import csv
import math
import statistics
from collections import defaultdict
from pathlib import Path

BENCH_ORDER = [
    "propagation_chains",
    "broadcast_expansion",
    "fork_explosion",
    "guarded_cascades",
    "nested_preemption",
]


def load_rows(path: Path) -> list[dict]:
    rows: list[dict] = []
    with path.open() as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            rows.append(
                {
                    "impl": row["impl"],
                    "benchmark": row["benchmark"],
                    "size": int(row["size"]),
                    "time_ms": float(row["time_ms"]),
                    "peak_mb": float(row["peak_mb"]),
                }
            )
    return rows


def median_map(rows: list[dict], metric: str) -> dict[tuple[str, int], float]:
    by_key: dict[tuple[str, int], list[float]] = defaultdict(list)
    for row in rows:
        by_key[(row["benchmark"], row["size"])].append(row[metric])
    return {key: statistics.median(values) for key, values in by_key.items()}


def geomean(values: list[float]) -> float:
    return math.exp(sum(math.log(v) for v in values) / len(values))


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--rml-legacy",
        type=Path,
        required=True,
        help="CSV from legacy ReactiveML toolchain (e.g. rml-4.14)",
    )
    parser.add_argument(
        "--rml-ocaml5",
        type=Path,
        required=True,
        help="CSV from patched ReactiveML toolchain on OCaml 5",
    )
    parser.add_argument(
        "--out-detail",
        type=Path,
        default=root / "data" / "processed" / "rml-toolchain-detail.csv",
    )
    parser.add_argument(
        "--out-geomean",
        type=Path,
        default=root / "data" / "processed" / "rml-toolchain-geomean.csv",
    )
    args = parser.parse_args()

    legacy_rows = load_rows(args.rml_legacy)
    ocaml5_rows = load_rows(args.rml_ocaml5)

    legacy_time = median_map(legacy_rows, "time_ms")
    ocaml5_time = median_map(ocaml5_rows, "time_ms")
    legacy_mem = median_map(legacy_rows, "peak_mb")
    ocaml5_mem = median_map(ocaml5_rows, "peak_mb")

    if set(legacy_time.keys()) != set(ocaml5_time.keys()):
        raise ValueError("Input grids differ between legacy and ocaml5 campaigns")

    keys = sorted(legacy_time.keys(), key=lambda k: (k[1], BENCH_ORDER.index(k[0])))
    sizes = sorted({size for _, size in keys})

    args.out_detail.parent.mkdir(parents=True, exist_ok=True)

    with args.out_detail.open("w", newline="") as fh:
        writer = csv.writer(fh)
        writer.writerow(
            [
                "benchmark",
                "size",
                "legacy_median_time_ms",
                "ocaml5_median_time_ms",
                "legacy_over_ocaml5_time",
                "legacy_median_peak_mb",
                "ocaml5_median_peak_mb",
                "legacy_over_ocaml5_peak_mb",
            ]
        )
        for bench, size in keys:
            writer.writerow(
                [
                    bench,
                    size,
                    f"{legacy_time[(bench, size)]:.6f}",
                    f"{ocaml5_time[(bench, size)]:.6f}",
                    f"{(legacy_time[(bench, size)] / ocaml5_time[(bench, size)]):.6f}",
                    f"{legacy_mem[(bench, size)]:.6f}",
                    f"{ocaml5_mem[(bench, size)]:.6f}",
                    f"{(legacy_mem[(bench, size)] / ocaml5_mem[(bench, size)]):.6f}",
                ]
            )

    with args.out_geomean.open("w", newline="") as fh:
        writer = csv.writer(fh)
        writer.writerow(
            [
                "size",
                "geomean_legacy_over_ocaml5_time",
                "geomean_legacy_over_ocaml5_peak_mb",
            ]
        )
        for size in sizes:
            ratios_t = []
            ratios_m = []
            for bench in BENCH_ORDER:
                ratios_t.append(legacy_time[(bench, size)] / ocaml5_time[(bench, size)])
                ratios_m.append(legacy_mem[(bench, size)] / ocaml5_mem[(bench, size)])
            writer.writerow([size, f"{geomean(ratios_t):.6f}", f"{geomean(ratios_m):.6f}"])

        all_ratios_t = [legacy_time[k] / ocaml5_time[k] for k in keys]
        all_ratios_m = [legacy_mem[k] / ocaml5_mem[k] for k in keys]
        writer.writerow(["global", f"{geomean(all_ratios_t):.6f}", f"{geomean(all_ratios_m):.6f}"])

    print(f"Legacy source : {args.rml_legacy}")
    print(f"OCaml5 source : {args.rml_ocaml5}")
    print(f"Wrote {args.out_detail}")
    print(f"Wrote {args.out_geomean}")


if __name__ == "__main__":
    main()
