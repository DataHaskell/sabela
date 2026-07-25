#!/usr/bin/env python3
"""Grade a transcript directory against the ratcheted pins table (R5.9 +
context pins). Pinned episodes print WITHIN or BREACH per metric; episodes
with no pin row print FIRST-PIN candidates with their measured numbers.
Exit 1 iff any pinned metric breached. Usage: check-pins.py <dir> <pins.tsv>
"""
import sys
from pathlib import Path

from importlib import import_module

sys.path.insert(0, str(Path(__file__).parent))
episode_metrics = import_module("r10-metrics").episode_metrics

METRICS = [("bytes", "max_bytes"), ("discover", "max_discover"),
           ("disc_bw", "max_disc_bw"), ("facts_held", "max_facts_held"),
           ("disc_after_goal", "max_disc_after_goal")]


def load_pins(path):
    pins = {}
    for line in Path(path).read_text().splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        cols = line.split("\t")
        # Older pins files lack the disc_after_goal column: unpinned ("-").
        task, arm, mb, md, mdbw, mfh = cols[:6]
        mdag = cols[6] if len(cols) > 6 else "-"
        pins[(task, arm)] = {"max_bytes": mb, "max_discover": md,
                             "max_disc_bw": mdbw, "max_facts_held": mfh,
                             "max_disc_after_goal": mdag}
    return pins


def main():
    if len(sys.argv) != 3:
        sys.exit("usage: check-pins.py <transcript-dir> <pins.tsv>")
    pins = load_pins(sys.argv[2])
    breached = False
    for p in sorted(Path(sys.argv[1]).glob("*.md")):
        e = episode_metrics(p)
        key = (e["task"], e["arm"])
        row = pins.get(key)
        if row is None:
            print(f"FIRST-PIN {e['task']} {e['arm']} bytes={e['bytes']} "
                  f"discover={e['discover']} disc_bw={e['disc_bw']} "
                  f"wrote={'yes' if e['wrote'] else 'no'} "
                  f"stopped={e['stopped']}")
            continue
        fails = []
        for metric, col in METRICS:
            if row[col] == "-":
                continue
            if e[metric] > int(row[col]):
                fails.append((metric, e[metric], int(row[col])))
        if fails:
            breached = True
            for metric, got, pin in fails:
                print(f"BREACH {e['task']} {e['arm']} {metric}={got} "
                      f"pin<={pin}")
        else:
            print(f"WITHIN {e['task']} {e['arm']}")
    sys.exit(1 if breached else 0)


if __name__ == "__main__":
    main()
