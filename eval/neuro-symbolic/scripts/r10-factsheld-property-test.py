#!/usr/bin/env python3
"""Property self-test for the discover-calls-after-facts-held metric (R5.9).

Over generated transcripts it must (a) count exactly the discover calls
between the planted facts-held turn (first found envelope whose top hit is
exact and typed — the call-ready consumer fact the ledger harvests) and the
first write, (b) never count a pre-facts call, (c) report 0 when no
call-ready fact is ever held, and (d) with no write, count every post-facts
discover call to episode end.
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


def block(n, tool, args, env):
    return (
        f"**tool calls:**\n- `{tool}` " + json.dumps(args) + "\n"
        f"## {n}. tool ({tool})\n\n```\n" + json.dumps(env) + "\n```\n"
    )


def weak_env(name):
    """A found envelope that is NOT call-ready: no exact typed top hit."""
    return {
        "hits": [{"install": "installed", "name": name,
                  "matchKind": "substring"}],
        "query": name, "shown": 1, "state": "found",
    }


def miss_env(name):
    return {"hits": [], "query": name, "shown": 0, "state": "not_found"}


def ready_env(name):
    """The call-ready fact: exact match with a signature (Advice.sigFacts)."""
    return {
        "hits": [{"install": "installed", "name": name, "matchKind": "exact",
                  "type": name + " :: Int -> Int", "module": "Zoo"}],
        "query": name, "shown": 1, "state": "found",
    }


def gen_episode(rng, tmp, plant_facts, plant_write):
    """Random pre-facts calls, optional facts-held turn, counted post-facts
    calls, optional write, uncounted post-write calls. Returns expected."""
    pre = rng.randint(0, 3)
    post = rng.randint(0, 4)
    tail = rng.randint(0, 2)
    n, blocks = 2, []
    for i in range(pre):
        env = rng.choice([weak_env, miss_env])(f"pre{i}")
        blocks.append(block(n, "discover", {"query": f"pre{i}"}, env))
        n += 1
    if plant_facts:
        blocks.append(block(n, "discover", {"query": "colX"},
                            ready_env("colX")))
        n += 1
    for i in range(post):
        blocks.append(block(n, "discover", {"query": f"post{i}"},
                            miss_env(f"post{i}")))
        n += 1
    if plant_write:
        blocks.append(block(n, "insert_cell", {"source": "x = 1"},
                            {"cell": 1, "state": "inserted"}))
        n += 1
    for i in range(tail):
        blocks.append(block(n, "discover", {"query": f"tail{i}"},
                            miss_env(f"tail{i}")))
        n += 1
    body = HDR.format(task="factsTask", arm="off") + "# Session: factsTask\n"
    p = tmp / "factsTask-s1-off.md"
    p.write_text(body + "".join(blocks))
    if not plant_facts:
        return p, 0
    return p, post if plant_write else post + tail


def main():
    for seed in range(40):
        rng = random.Random(seed)
        plant_facts = seed % 4 != 0
        plant_write = seed % 3 != 0
        with tempfile.TemporaryDirectory() as d:
            p, want = gen_episode(rng, Path(d), plant_facts, plant_write)
            e = m.episode_metrics(p)
            assert "facts_held" in e, "episode_metrics lacks facts_held"
            got = e["facts_held"]
            assert got == want, (
                f"seed {seed}: facts_held={got}, planted {want} "
                f"(facts={plant_facts}, write={plant_write})"
            )
    print("FACTS-HELD PROPERTY SELFTEST OK (40 generated episodes)")


if __name__ == "__main__":
    main()
