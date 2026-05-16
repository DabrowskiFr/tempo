#!/usr/bin/env python3
"""Analyze per-snapshot Tempo diagnostic traces and extract growth inflection points."""

from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from pathlib import Path
from typing import Dict, Iterable, List, Tuple


def load_summary(path: Path) -> List[dict]:
    rows: List[dict] = []
    with path.open() as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            rows.append(
                {
                    "benchmark": row["benchmark"],
                    "size": int(row["size"]),
                    "run": int(row["run"]),
                    "time_ms": float(row["time_ms"]),
                    "instants": int(row["instants"]),
                    "peak_mb": float(row["peak_mb"]),
                    "diag_csv": Path(row["diag_csv"]),
                }
            )
    return rows


def load_diag(path: Path) -> List[dict]:
    rows: List[dict] = []
    with path.open() as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            rows.append(
                {
                    "timestamp_ms": float(row["timestamp_ms"]),
                    "rss_mb": float(row["rss_mb"]),
                    "phase": row["phase"],
                    "instant": int(row["instant"]),
                    "tracked_signals": int(row["tracked_signals"]),
                    "awaiters": int(row["awaiters"]),
                    "guard_waiters": int(row["guard_waiters"]),
                    "kill_watchers": int(row["kill_watchers"]),
                    "live_tasks": int(row["live_tasks"]),
                    "kill_context_refs": int(row["kill_context_refs"]),
                    "kill_context_nodes": int(row["kill_context_nodes"]),
                    "kill_context_max_depth": int(row["kill_context_max_depth"]),
                }
            )
    return rows


def max_row(rows: Iterable[dict], key: str) -> dict:
    best = None
    for row in rows:
        if best is None or row[key] > best[key]:
            best = row
    if best is None:
        raise ValueError("empty diagnostic trace")
    return best


def write_csv(path: Path, rows: List[dict], header: List[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=header)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def ratio(curr: float, prev: float) -> float:
    if prev == 0.0:
        return float("inf")
    return curr / prev


def main() -> None:
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--summary",
        type=Path,
        required=True,
        help="CSV produced by run_tempo_diag.sh",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=root / "data" / "processed" / "tempo-diag-analysis",
    )
    args = parser.parse_args()

    summary_rows = load_summary(args.summary)
    per_point: List[dict] = []

    for row in summary_rows:
        diag = load_diag(row["diag_csv"])
        rss_peak = max_row(diag, "rss_mb")
        live_peak = max_row(diag, "live_tasks")
        kill_nodes_peak = max_row(diag, "kill_context_nodes")
        kill_refs_peak = max_row(diag, "kill_context_refs")
        kill_depth_peak = max_row(diag, "kill_context_max_depth")

        per_point.append(
            {
                "benchmark": row["benchmark"],
                "size": row["size"],
                "run": row["run"],
                "time_ms": f"{row['time_ms']:.6f}",
                "peak_mb_os": f"{row['peak_mb']:.6f}",
                "peak_rss_diag_mb": f"{rss_peak['rss_mb']:.6f}",
                "rss_peak_phase": rss_peak["phase"],
                "rss_peak_instant": rss_peak["instant"],
                "rss_peak_timestamp_ms": f"{rss_peak['timestamp_ms']:.6f}",
                "max_live_tasks": live_peak["live_tasks"],
                "max_kill_context_refs": kill_refs_peak["kill_context_refs"],
                "max_kill_context_nodes": kill_nodes_peak["kill_context_nodes"],
                "max_kill_context_depth": kill_depth_peak["kill_context_max_depth"],
                "max_tracked_signals": max_row(diag, "tracked_signals")["tracked_signals"],
                "max_awaiters": max_row(diag, "awaiters")["awaiters"],
                "max_guard_waiters": max_row(diag, "guard_waiters")["guard_waiters"],
                "max_kill_watchers": max_row(diag, "kill_watchers")["kill_watchers"],
            }
        )

    per_point.sort(key=lambda r: (r["benchmark"], int(r["size"]), int(r["run"])))

    growth_rows: List[dict] = []
    grouped: Dict[Tuple[str, int], List[dict]] = defaultdict(list)
    for row in per_point:
        grouped[(row["benchmark"], int(row["run"]))].append(row)

    for (bench, run), rows in grouped.items():
        rows.sort(key=lambda r: int(r["size"]))
        prev = None
        for row in rows:
            current_rss = float(row["peak_rss_diag_mb"])
            current_kill_nodes = float(row["max_kill_context_nodes"])
            current_live_tasks = float(row["max_live_tasks"])
            if prev is None:
                growth_rows.append(
                    {
                        "benchmark": bench,
                        "run": run,
                        "size": row["size"],
                        "rss_growth_vs_prev": "",
                        "kill_nodes_growth_vs_prev": "",
                        "live_tasks_growth_vs_prev": "",
                    }
                )
            else:
                growth_rows.append(
                    {
                        "benchmark": bench,
                        "run": run,
                        "size": row["size"],
                        "rss_growth_vs_prev": f"{ratio(current_rss, float(prev['peak_rss_diag_mb'])):.6f}",
                        "kill_nodes_growth_vs_prev": f"{ratio(current_kill_nodes, float(prev['max_kill_context_nodes'])):.6f}",
                        "live_tasks_growth_vs_prev": f"{ratio(current_live_tasks, float(prev['max_live_tasks'])):.6f}",
                    }
                )
            prev = row

    per_point_path = args.out_dir / "per_point.csv"
    growth_path = args.out_dir / "growth_vs_prev_size.csv"

    write_csv(
        per_point_path,
        per_point,
        [
            "benchmark",
            "size",
            "run",
            "time_ms",
            "peak_mb_os",
            "peak_rss_diag_mb",
            "rss_peak_phase",
            "rss_peak_instant",
            "rss_peak_timestamp_ms",
            "max_live_tasks",
            "max_kill_context_refs",
            "max_kill_context_nodes",
            "max_kill_context_depth",
            "max_tracked_signals",
            "max_awaiters",
            "max_guard_waiters",
            "max_kill_watchers",
        ],
    )
    write_csv(
        growth_path,
        sorted(growth_rows, key=lambda r: (r["benchmark"], int(r["run"]), int(r["size"]))),
        [
            "benchmark",
            "run",
            "size",
            "rss_growth_vs_prev",
            "kill_nodes_growth_vs_prev",
            "live_tasks_growth_vs_prev",
        ],
    )

    print(f"wrote {per_point_path}")
    print(f"wrote {growth_path}")


if __name__ == "__main__":
    main()
