#!/usr/bin/env python3
"""R10 gate metrics over a transcript directory (see testing-plan.md R10).

Per episode: discover calls, duplicate/not_found closures, same-query
rephrasings, discover calls after a call-ready fact was held but before the
first write (facts_held, R5.9), max single tool-response bytes, transcript
bytes, stop label, final emptiness. Aggregates: useful:noise over discover
envelopes, facts-held breaches (>2), budget
breaches (>2500b), empty finals, unlabeled stops, and false denials counted
per (query, mode, filter) tuple (S13): a not_found whose name some other
rendering of the same task's catalogue resolved is a proven false denial;
remaining not_found queries are listed for manual truth-grading.
"""
import json
import re
import sys
from pathlib import Path

BUDGET = 2500
TOOL_HDR_RE = re.compile(r"^## \d+\. tool \(([a-zA-Z_]+)\)\s*$")
SECTION_RE = re.compile(r"^## \d+\. (\w+)(?: \(([a-zA-Z_]+)\))?\s*$")
CALL_RE = re.compile(r"^- `discover` (\{.*\})\s*$")
ANY_CALL_RE = re.compile(r"^- `([^`]+)` (\{.*\})\s*$")
IDENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_']{2,}")
WRITE_TOOLS = {"insert_cell", "replace_cell_source", "propose_edit"}
OK_MARKS = ('"ok":true', '"ok": true')
ERR_MARKS = ('"ok":false', '"ok": false', "TOOL ERROR", '"error"',
             '"hasError":true')
# Haskell noise words never counted as an attributable fact token.
STOPWORDS = {"import", "qualified", "module", "where", "let", "case", "then",
             "else", "deriving", "instance", "class", "data", "type",
             "newtype", "Int", "Double", "String", "Text", "Maybe", "Just",
             "Nothing", "True", "False", "print", "show", "putStrLn", "sum",
             "map", "filter", "length", "IO", "the", "and"}
# Hit install states that prove the name resolvable in this session's world
# (an absent-known upstream row is R5.4 evidence of absence, not resolution).
RESOLVED_INSTALLS = {"installed", "notebook", "hidden", "live", ""}


def parse_header(text):
    m = re.match(r"<!-- episode-config\n(.*?)\n-->", text, re.S)
    fields = {}
    if m:
        for line in m.group(1).splitlines():
            k, _, v = line.partition(": ")
            fields[k] = v
    return fields


def tool_sections(lines):
    """Yield (line-idx, tool-name, response-body) per '## N. tool (name)'."""
    i = 0
    while i < len(lines):
        m = TOOL_HDR_RE.match(lines[i])
        if not m:
            i += 1
            continue
        body, j, fence = [], i + 1, False
        while j < len(lines) and not lines[j].startswith("## "):
            if lines[j].startswith("```"):
                fence = not fence
            elif fence:
                body.append(lines[j])
            j += 1
        yield i, m.group(1), "\n".join(body)
        i = j


def tool_call_lines(lines):
    """(line-idx, tool-name, parsed-args) for EVERY rendered call line."""
    out = []
    for i, line in enumerate(lines):
        m = ANY_CALL_RE.match(line)
        if not m:
            continue
        try:
            out.append((i, m.group(1), json.loads(m.group(2))))
        except json.JSONDecodeError:
            out.append((i, m.group(1), {}))
    return out


def write_call_before(calls, idx, tool):
    """The nearest preceding rendered call of `tool` before line `idx`."""
    for i, name, args in reversed(calls):
        if i < idx and name == tool:
            return args
    return {}


def goal_satisfying(args, body):
    """A clean deliverable write: ok outcome, not a dep-declaring cell
    (mirror of the loop's deliverableLanded, R6.10)."""
    src = str(args.get("source", args.get("new_source", "")))
    return any(m in body for m in OK_MARKS) and "-- cabal:" not in src


def sections(lines):
    """(line-idx, role, tool-name-or-None, body) per '## N. role (name)'."""
    out, i = [], 0
    while i < len(lines):
        m = SECTION_RE.match(lines[i])
        if not m:
            i += 1
            continue
        body, j = [], i + 1
        while j < len(lines) and not lines[j].startswith("## "):
            body.append(lines[j])
            j += 1
        out.append((i, m.group(1), m.group(2), "\n".join(body)))
        i = j
    return out


def channel_of(role, tool, body, first_user_idx, idx):
    """The injection channel one section belongs to, or None (model text,
    the task prompt itself, clean non-card tool results)."""
    if role == "tool" and tool == "discover":
        return "card"
    if role == "tool" and tool == "scaffold":
        return "scaffold"
    if role == "user" and idx != first_user_idx:
        return "nudge-candidate"
    if role == "tool" and any(m in body for m in ERR_MARKS):
        return "diagnostic"
    return None


def fact_tokens(source, prompt):
    """Identifiers in a write's source that the task prompt did not already
    hand the model — the facts whose supplying channel is attributable."""
    prompt_toks = set(IDENT_RE.findall(prompt))
    return {t for t in IDENT_RE.findall(source)
            if t not in prompt_toks and t not in STOPWORDS}


def first_fact_channel(lines):
    """Which channel {card, scaffold, nudge-candidate, diagnostic} first
    supplied a fact the winning (last clean) write's source uses; 'none'
    when no clean write landed or no channel preceded it with a fact."""
    calls = tool_call_lines(lines)
    secs = sections(lines)
    user_idxs = [i for i, r, _, _ in secs if r == "user"]
    first_user = user_idxs[0] if user_idxs else -1
    prompt = next((b for i, r, _, b in secs
                   if r == "user" and i == first_user), "")
    win_idx, win_src = None, ""
    for idx, tool, body in tool_sections(lines):
        if tool in WRITE_TOOLS:
            args = write_call_before(calls, idx, tool)
            if goal_satisfying(args, body):
                win_idx = idx
                win_src = str(args.get("source", args.get("new_source", "")))
    if win_idx is None:
        return "none"
    facts = fact_tokens(win_src, prompt)
    if not facts:
        return "none"
    for idx, role, tool, body in secs:
        if idx >= win_idx:
            break
        ch = channel_of(role, tool, body, first_user, idx)
        if ch and any(t in set(IDENT_RE.findall(body)) for t in facts):
            return ch
    return "none"


def discover_calls(lines):
    """(line-idx, parsed-args) for every rendered `discover` call line."""
    out = []
    for i, line in enumerate(lines):
        m = CALL_RE.match(line)
        if not m:
            continue
        try:
            out.append((i, json.loads(m.group(1))))
        except json.JSONDecodeError:
            out.append((i, {}))
    return out


def tuple_of(args, env):
    """The S13 tuple one discover answer was rendered under."""
    q = str(args.get("query", env.get("query", ""))).strip()
    return (q, str(args.get("mode", "")), str(args.get("module", "")),
            str(args.get("package", "")))


def resolved_names(env):
    """Names this found-envelope proves resolvable (hit names + modules)."""
    if env.get("state") != "found":
        return set()
    names = set()
    for h in env.get("hits", []):
        if str(h.get("install", "")) in RESOLVED_INSTALLS:
            names |= {str(h.get("name", "")), str(h.get("module", ""))}
    return names - {""}


def call_ready(env):
    """Does this envelope hand the ledger a call-ready consumer fact (R5.6)?
    Mirror of Advice.sigFacts: the TOP hit, exact-matched, with a signature."""
    if env.get("state") != "found":
        return False
    hits = env.get("hits", [])
    if not hits:
        return False
    h = hits[0]
    return (str(h.get("matchKind", "")) == "exact"
            and bool(str(h.get("type", "")))
            and bool(str(h.get("name", ""))))


def episode_metrics(path):
    text = path.read_text(errors="replace")
    hdr = parse_header(text)
    lines = text.splitlines()
    queries, dup, notf, notf_queries, useful = [], 0, 0, [], 0
    max_resp, n_discover, disc_bw, wrote = 0, 0, 0, False
    facts, facts_held = False, 0
    goal_sat, disc_after_goal = False, 0
    denials, resolved = [], set()
    calls, ci = discover_calls(lines), 0
    all_calls = tool_call_lines(lines)
    for idx, tool, body in tool_sections(lines):
        if tool in WRITE_TOOLS:
            wrote = True
            # Goal satisfied at the first clean non-dep write (R9-T5 pin
            # mirror of the loop's deliverableLanded).
            if not goal_sat and goal_satisfying(
                    write_call_before(all_calls, idx, tool), body):
                goal_sat = True
        if tool == "discover":
            n_discover += 1
            if goal_sat:
                disc_after_goal += 1
            if not wrote:
                disc_bw += 1
                # Discover spend AFTER the ledger already held a call-ready
                # fact and BEFORE the first write: the barChart waste class.
                if facts:
                    facts_held += 1
        max_resp = max(max_resp, len(body.encode()))
        if tool != "discover":
            continue
        args = {}
        if ci < len(calls) and calls[ci][0] < idx:
            args = calls[ci][1]
            ci += 1
        try:
            env = json.loads(body)
        except json.JSONDecodeError:
            continue
        if call_ready(env):
            facts = True
        state = env.get("state", "")
        q = str(env.get("query", "")).strip().lower()
        queries.append(q)
        resolved |= resolved_names(env)
        if state == "duplicate":
            dup += 1
        elif state == "not_found":
            notf += 1
            notf_queries.append(env.get("query", ""))
            denials.append(tuple_of(args, env))
        elif state == "found":
            useful += 1
    rephrase = sum(queries.count(q) - 1 for q in set(queries))
    return {
        "task": hdr.get("task", path.stem),
        "arm": hdr.get("arm", "?"),
        "denials": denials,
        "resolved": resolved,
        "stopped": hdr.get("stopped", ""),
        "final": hdr.get("final", ""),
        "lint": hdr.get("lint", ""),
        "bytes": len(text.encode()),
        "discover": n_discover,
        "disc_bw": disc_bw,
        "wrote": wrote,
        "dup": dup,
        "not_found": notf,
        "not_found_queries": notf_queries,
        "rephrase": rephrase,
        "facts_held": facts_held,
        "disc_after_goal": disc_after_goal,
        "channel": first_fact_channel(lines),
        "useful": useful,
        "max_resp": max_resp,
    }


def false_denials(eps):
    """S13: every denial tuple whose (base) name some rendering of the same
    task's catalogue resolved — the mode/filter-gated proven false denials.
    A name resolved nowhere stays a legal R5.4 genuine absence (uncounted)."""
    by_task = {}
    for e in eps:
        by_task.setdefault(e["task"], []).append(e)
    out = []
    for task, group in by_task.items():
        resolved = set().union(*(e["resolved"] for e in group))
        for e in group:
            for (q, mode, mod, pkg) in e["denials"]:
                if q and (q in resolved or q.split(".")[-1] in resolved):
                    out.append((task, e["arm"], q, mode, mod, pkg))
    return out


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: r10-metrics.py <transcript-dir>")
    eps = [episode_metrics(p) for p in sorted(Path(sys.argv[1]).glob("*.md"))]
    if not eps:
        sys.exit("no transcripts found")
    for e in eps:
        print(
            f"{e['task']} {e['arm']} discover={e['discover']} "
            f"disc_bw={e['disc_bw']} "
            f"wrote={'yes' if e['wrote'] else 'no'} "
            f"dup={e['dup']} not_found={e['not_found']} "
            f"rephrase={e['rephrase']} facts_held={e['facts_held']} "
            f"disc_after_goal={e['disc_after_goal']} "
            f"channel={e['channel']} "
            f"max_resp={e['max_resp']}b "
            f"bytes={e['bytes']} stopped={e['stopped']} "
            f"final={'EMPTY' if not e['final'] else 'present'} "
            f"lint={e['lint']}"
        )
    useful = sum(e["useful"] for e in eps)
    noise = sum(e["dup"] + e["not_found"] for e in eps)
    print(f"episodes: {len(eps)}")
    print(f"useful={useful} noise={noise} (discover envelopes)")
    print(f"budget-breaches(>{BUDGET}): "
          f"{sum(1 for e in eps if e['max_resp'] > BUDGET)}")
    print(f"empty-finals: {sum(1 for e in eps if not e['final'])}")
    print(f"unlabeled-stops: {sum(1 for e in eps if not e['stopped'])}")
    print(f"rephrasings-total: {sum(e['rephrase'] for e in eps)}")
    print(f"facts-held-breaches(>2): "
          f"{sum(1 for e in eps if e['facts_held'] > 2)}")
    print(f"disc-after-goal-breaches(>2): "
          f"{sum(1 for e in eps if e['disc_after_goal'] > 2)}")
    fd = false_denials(eps)
    print(f"false-denials(per-tuple): {len(fd)}")
    for task, arm, q, mode, mod, pkg in fd:
        print(f"  FALSE-DENIAL[{task}-{arm}]: query='{q}' mode='{mode}' "
              f"module='{mod}' package='{pkg}' "
              f"(name resolved elsewhere in this task's transcripts)")
    print(f"not_found-total: {sum(e['not_found'] for e in eps)}")
    for e in eps:
        for q in e["not_found_queries"]:
            print(f"  not_found[{e['task']}-{e['arm']}]: {q!r}")


if __name__ == "__main__":
    main()
