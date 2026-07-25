#!/usr/bin/env python3
"""Property self-test for the per-(query, mode, filter) false-denial counter
(S13). Over randomly generated transcript pairs it must (a) count every
planted scoped/mode-gated denial that a mode-blind (query-keyed) counter
provably misses, and (b) never count a legal R5.4 genuine-absence answer.
"""
import json
import random
import sys
import tempfile
from importlib import import_module
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
m = import_module("r10-metrics")

HDR = """<!-- episode-config
task: {task}
arm: {arm}
levers: grammar={arm}
seed: 1
seeds-tried: 1
model: test-model
stopped: done
final: done
lint: ok
run-id: run-prop
commit: deadbeef
build-time: 2026-07-20T00:00:00Z
run-time: 2026-07-20T01:00:00Z
endpoint: http://localhost:9
relink-probe: ok: test
-->
"""

MODES = ["search", "inventory", "construct"]


def call_block(n, args, env):
    return (
        "**tool calls:**\n- `discover` " + json.dumps(args) + "\n"
        f"## {n}. tool (discover)\n\n```\n" + json.dumps(env) + "\n```\n"
    )


def found_env(name):
    return {
        "hits": [{"install": "installed", "name": name}],
        "query": name,
        "shown": 1,
        "state": "found",
    }


def denial_args(rng, name):
    """A scoped/mode-gated rendering of the query: filter and/or odd mode."""
    return {
        "query": rng.choice([name, "D." + name]),
        "mode": rng.choice(MODES),
        "module": rng.choice(["Zoo", "DataFrame"]),
        "package": "",
    }


def gen_pair(rng, tmp):
    """Two arms: every scoped name is found plainly on one arm and denied
    under a filtered tuple on the other; absent names are denied everywhere
    and found nowhere. Returns (scoped, absent) name lists."""
    scoped = [f"sc{i}" for i in range(rng.randint(1, 3))]
    absent = [f"ab{i}" for i in range(rng.randint(0, 2))]
    noise = [f"nz{i}" for i in range(rng.randint(0, 2))]
    n = 2
    off, on = [], []
    for name in scoped:
        on.append(call_block(n, {"query": name, "mode": "search"}, found_env(name)))
        off.append(
            call_block(
                n + 1,
                denial_args(rng, name),
                {"hits": [], "query": name, "shown": 0, "state": "not_found"},
            )
        )
        n += 2
    for name in absent:
        arm = rng.choice([off, on])
        arm.append(
            call_block(
                n,
                {"query": name, "mode": rng.choice(MODES)},
                {"hits": [], "query": name, "shown": 0, "state": "not_found"},
            )
        )
        n += 1
    for name in noise:
        off.append(call_block(n, {"query": name, "mode": "search"}, found_env(name)))
        n += 1
    for arm, blocks in (("off", off), ("on", on)):
        body = HDR.format(task="propTask", arm=arm) + "# Session: propTask\n"
        rng.shuffle(blocks)
        (tmp / f"propTask-s1-{arm}.md").write_text(body + "".join(blocks))
    return scoped, absent


def mode_blind_count(eps):
    """The provably-worse reference: a query answered found ANYWHERE in the
    task is treated as satisfied, so its scoped denials go uncounted."""
    found = set()
    denied = []
    for e in eps:
        found |= e["resolved"]
        denied += [q for (q, _, _, _) in e["denials"]]
    return [q for q in denied if q not in found and q.split(".")[-1] not in found]


def main():
    for seed in range(40):
        rng = random.Random(seed)
        with tempfile.TemporaryDirectory() as d:
            tmp = Path(d)
            scoped, absent = gen_pair(rng, tmp)
            eps = [m.episode_metrics(p) for p in sorted(tmp.glob("*.md"))]
            flagged = m.false_denials(eps)
            names = sorted({q.split(".")[-1] for (_, _, q, _, _, _) in flagged})
            assert names == sorted(scoped), (
                f"seed {seed}: per-tuple counter flagged {names}, "
                f"planted scoped {sorted(scoped)} (absent {sorted(absent)})"
            )
            blind = mode_blind_count(eps)
            missed = [s for s in scoped if s not in {q.split(".")[-1] for q in blind}]
            assert missed == scoped, (
                f"seed {seed}: mode-blind reference caught scoped denials "
                f"{set(scoped) - set(missed)} — the property no longer "
                "distinguishes the counters"
            )
    print("PROPERTY SELFTEST OK (40 generated pairs)")


if __name__ == "__main__":
    main()
