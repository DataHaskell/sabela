#!/usr/bin/env python3
"""Property self-test for the R9-T5 channel-attribution metric and the
discover-calls-after-goal-satisfied pin metric, over generated episodes.

Channel attribution: the winning (last clean) write's first-fact channel
must classify as the ONE channel the fact was planted in — card (a
discover envelope), scaffold, nudge-candidate (a post-prompt user steer),
or diagnostic (an erroring tool result) — and never as a prompt echo
(prompt-given identifiers are not attributable facts) or on a write-less
episode. disc_after_goal: counts exactly the discover calls after the
first clean non-dep write; a red or dep-declaring write never satisfies.
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

FACT = "graniteBarsFn"
PROMPT = "Plot the quarterly totals as a bar chart in the notebook."


def sec(n, role, body, tool=None):
    hdr = f"## {n}. {role}" + (f" ({tool})" if tool else "")
    return f"{hdr}\n\n```\n{body}\n```\n"


def call(tool, args):
    """A tool call rendered as the real transcripts render it: inside an
    assistant section, before the answering tool section."""
    return ("## 0. assistant\n\n**tool calls:**\n- `" + tool + "` "
            + json.dumps(args) + "\n")


def channel_block(n, channel, fact):
    """One planted fact-bearing block of the given channel."""
    if channel == "card":
        env = {"hits": [{"name": fact, "matchKind": "exact",
                         "type": fact + " :: Int"}],
               "query": fact, "shown": 1, "state": "found"}
        return call("discover", {"query": fact}) \
            + sec(n, "tool", json.dumps(env), "discover")
    if channel == "scaffold":
        return sec(n, "tool", f"Setup write: use {fact} to draw.", "scaffold")
    if channel == "nudge-candidate":
        return sec(n, "user", f"You hold the fact {fact} :: Int; act on it.")
    if channel == "diagnostic":
        body = json.dumps({"ok": False,
                           "error": f"perhaps you meant {fact}"})
        return call("insert_cell", {"source": "draft = wrongName"}) \
            + sec(n, "tool", body, "insert_cell")
    raise ValueError(channel)


def noise_block(n, rng):
    """A fact-free block on a random channel, to force real selection."""
    ch = rng.choice(["card", "scaffold", "nudge-candidate", "diagnostic"])
    return channel_block(n, ch, "irrelevantOtherName")


def write_block(n, source, ok=True):
    body = json.dumps({"cellId": 1,
                       "execution": {"ok": ok,
                                     "outcome": {"tag": "Succeeded" if ok
                                                 else "CompileError"}},
                       "status": "completed"})
    return call("insert_cell", {"source": source}) \
        + sec(n, "tool", body, "insert_cell")


def discover_miss(n, q):
    env = {"hits": [], "query": q, "shown": 0, "state": "not_found"}
    return call("discover", {"query": q}) \
        + sec(n, "tool", json.dumps(env), "discover")


def gen_channel_episode(rng, tmp, channel):
    """Prompt, noise, ONE planted fact block, then the winning write using
    the fact. Expected channel = the planted one."""
    n = 1
    blocks = [sec(n, "user", PROMPT)]
    n += 1
    for _ in range(rng.randint(0, 2)):
        blocks.append(noise_block(n, rng))
        n += 1
    blocks.append(channel_block(n, channel, FACT))
    n += 1
    blocks.append(write_block(n, f"chart = {FACT} 42"))
    p = tmp / "chanTask-s1-off.md"
    p.write_text(HDR.format(task="chanTask", arm="off")
                 + "# Session: chanTask\n" + "".join(blocks))
    return p


def gen_prompt_only_episode(tmp):
    """The write only uses prompt-given identifiers: channel must be none."""
    blocks = [sec(1, "user", "Define quarterlyTotal as 600."),
              channel_block(2, "card", "unusedName"),
              write_block(3, "quarterlyTotal = 600")]
    p = tmp / "chanTask-s1-off.md"
    p.write_text(HDR.format(task="chanTask", arm="off")
                 + "# Session: chanTask\n" + "".join(blocks))
    return p


def gen_goal_episode(rng, tmp, kind):
    """disc_after_goal cases: clean / red-write-only / dep-write-only."""
    pre = rng.randint(0, 3)
    post = rng.randint(0, 4)
    n = 1
    blocks = [sec(n, "user", PROMPT)]
    n += 1
    for i in range(pre):
        blocks.append(discover_miss(n, f"pre{i}"))
        n += 1
    if kind == "clean":
        blocks.append(write_block(n, "total = 600"))
        want = post
    elif kind == "red":
        blocks.append(write_block(n, "total = oops", ok=False))
        want = 0
    else:
        blocks.append(write_block(n, "-- cabal: build-depends: granite"))
        want = 0
    n += 1
    for i in range(post):
        blocks.append(discover_miss(n, f"post{i}"))
        n += 1
    p = tmp / "goalTask-s1-off.md"
    p.write_text(HDR.format(task="goalTask", arm="off")
                 + "# Session: goalTask\n" + "".join(blocks))
    return p, want


def main():
    channels = ["card", "scaffold", "nudge-candidate", "diagnostic"]
    for seed in range(24):
        rng = random.Random(seed)
        ch = channels[seed % 4]
        with tempfile.TemporaryDirectory() as d:
            p = gen_channel_episode(rng, Path(d), ch)
            got = m.episode_metrics(p)["channel"]
            assert got == ch, f"seed {seed}: channel={got}, planted {ch}"
    with tempfile.TemporaryDirectory() as d:
        got = m.episode_metrics(gen_prompt_only_episode(Path(d)))["channel"]
        assert got == "none", f"prompt-only episode: channel={got}, want none"
    for seed in range(30):
        rng = random.Random(1000 + seed)
        kind = ["clean", "red", "dep"][seed % 3]
        with tempfile.TemporaryDirectory() as d:
            p, want = gen_goal_episode(rng, Path(d), kind)
            got = m.episode_metrics(p)["disc_after_goal"]
            assert got == want, (
                f"seed {seed} ({kind}): disc_after_goal={got}, planted {want}")
    print("CHANNEL-ATTRIBUTION PROPERTY SELFTEST OK "
          "(24 channel + 1 prompt-only + 30 goal episodes)")


if __name__ == "__main__":
    main()
