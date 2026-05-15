#!/usr/bin/env python3
"""Plot frozen benchmark memory medians for the selected Tempo/ReactiveML pair."""

from __future__ import annotations

import argparse
import csv
import math
import random
import statistics
from collections import defaultdict
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.ticker import NullLocator

BENCH_ORDER = [
    "propagation_chains",
    "broadcast_expansion",
    "fork_explosion",
    "guarded_cascades",
    "nested_preemption",
]

TITLES = {
    "propagation_chains": "B1 Propagation chains",
    "broadcast_expansion": "B2 Broadcast expansion",
    "fork_explosion": "B3 Fork explosion",
    "guarded_cascades": "B4 Guarded cascades",
    "nested_preemption": "B5 Nested preemption",
}


def load_values(paths, metric):
    values = defaultdict(list)
    for path in paths:
        with path.open() as fh:
            reader = csv.DictReader(fh)
            for row in reader:
                key = (row["impl"], row["benchmark"], int(row["size"]))
                values[key].append(float(row[metric]))
    return values


def percentile(values, p):
    xs = sorted(values)
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


def geomean_ratio_with_ci(values, sizes, samples, seed):
    rng = random.Random(seed)
    medians = {key: statistics.median(runs) for key, runs in values.items()}

    ratios = []
    ci_low = []
    ci_high = []
    for size in sizes:
        base = []
        for bench in BENCH_ORDER:
            tmed = medians[("tempo", bench, size)]
            rmed = medians[("rml", bench, size)]
            base.append(rmed / tmed)
        ratios.append(math.exp(sum(math.log(x) for x in base) / len(base)))

        draws = []
        for _ in range(samples):
            bs = []
            for bench in BENCH_ORDER:
                tvals = values[("tempo", bench, size)]
                rvals = values[("rml", bench, size)]
                tb = [tvals[rng.randrange(len(tvals))] for _ in range(len(tvals))]
                rb = [rvals[rng.randrange(len(rvals))] for _ in range(len(rvals))]
                bs.append(statistics.median(rb) / statistics.median(tb))
            draws.append(math.exp(sum(math.log(x) for x in bs) / len(bs)))
        ci_low.append(percentile(draws, 0.025))
        ci_high.append(percentile(draws, 0.975))

    return ratios, ci_low, ci_high


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
        "--out-pdf",
        type=Path,
        default=root / "figures" / "evaluation-memory-curves-reference.pdf",
    )
    parser.add_argument(
        "--out-png",
        type=Path,
        default=root / "figures" / "evaluation-memory-curves-reference.png",
    )
    parser.add_argument("--bootstrap-samples", type=int, default=20000)
    parser.add_argument("--seed", type=int, default=20260417)
    args = parser.parse_args()

    values = load_values([args.tempo, args.rml], metric="peak_mb")
    medians = {key: statistics.median(runs) for key, runs in values.items()}
    sizes = sorted({size for (_, _, size) in medians})
    agg, agg_lo, agg_hi = geomean_ratio_with_ci(
        values=values,
        sizes=sizes,
        samples=args.bootstrap_samples,
        seed=args.seed,
    )

    args.out_pdf.parent.mkdir(parents=True, exist_ok=True)

    fig, axes = plt.subplots(3, 2, figsize=(11.5, 11), constrained_layout=True)
    axes = axes.flatten()

    for i, bench in enumerate(BENCH_ORDER):
        ax = axes[i]
        for impl, color, marker, label in [
            ("tempo", "#1f77b4", "o", "Tempo"),
            ("rml", "#d62728", "s", "ReactiveML"),
        ]:
            pts = sorted(
                (size, medians[(impl, bench, size)])
                for size in sizes
            )
            xs = [x for x, _ in pts]
            ys = [y for _, y in pts]
            ax.plot(xs, ys, marker=marker, color=color, linewidth=2, markersize=6, label=label)

        ax.set_title(TITLES[bench], fontsize=11)
        ax.set_xscale("log")
        ax.set_xlabel("size n")
        ax.set_ylabel("median peak RSS (MB)")
        ax.xaxis.set_minor_locator(NullLocator())
        ax.grid(True, which="major", linestyle="--", linewidth=0.5, alpha=0.35)
        ax.legend(fontsize=9)

    ax = axes[-1]
    ax.plot(
        sizes,
        agg,
        marker="D",
        color="#111111",
        linewidth=2,
        markersize=6,
        label="Geomean RML/Tempo",
    )
    ax.fill_between(sizes, agg_lo, agg_hi, color="#7f7f7f", alpha=0.25, label="95% bootstrap CI")
    ax.axhline(1.0, color="#444444", linestyle="--", linewidth=1)
    ax.set_title("B6 Aggregate memory ratio (B1-B5)", fontsize=11)
    ax.set_xscale("log")
    ax.set_xlabel("size n")
    ax.set_ylabel("ratio (RML/Tempo)")
    ax.xaxis.set_minor_locator(NullLocator())
    ax.grid(True, which="major", linestyle="--", linewidth=0.5, alpha=0.35)
    ax.legend(fontsize=9)
    fig.suptitle(
        "Tempo vs ReactiveML: memory footprint by benchmark and aggregate ratio\n"
        f"Data: {args.tempo.name} and {args.rml.name}",
        fontsize=14,
    )

    fig.savefig(args.out_pdf)
    fig.savefig(args.out_png, dpi=220)
    print(f"Tempo source: {args.tempo.name}")
    print(f"ReactiveML source: {args.rml.name}")
    print(f"Wrote {args.out_pdf}")
    print(f"Wrote {args.out_png}")


if __name__ == "__main__":
    main()
