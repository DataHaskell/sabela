# Agent tool surface

Guidance for anyone shaping the tools an agent sees. It applies to the siza
client's tool list, the product chat catalogue, and the JSON those tools
return. Repo-wide coding rules live in `CLAUDE.md`.

## The principle

Parsimony, of the surface **and** of the returned message. Both spend the
same budget: the model's context is the scarcest resource in the loop, and
every tool name, parameter and returned field is drawn from it before any
work happens.

The lens is the one in
[what category theory teaches us about dataframes](https://mchav.github.io/what-category-theory-teaches-us-about-dataframes/):
find the core abstraction, express operations as compositions over it, and
resist the urge to add a new primitive for every new use. A surface with a
few composable operations beats a surface with one operation per task, for
the same reason a small algebra beats a catalogue of special cases.

## Rules

- **Fold, do not accrete.** Before adding a tool, ask which existing tool
  should grow a parameter instead. A new name costs every future caller the
  reading of it.
- **Audit call counts, but read them against the corpus.**

  ```bash
  grep -ho '^- `[a-z_]*`' docs/discover/live/*_verbose.md | sort | uniq -c | sort -rn
  ```

  Zero calls is evidence ONLY when the recorded episodes exercise the
  capability. A corpus of write-one-cell tasks cannot condemn `read_cell`,
  `execute_cell` or `delete_cell`: driving a real notebook needs all three,
  and their absence measures the tasks, not the tools. Before folding a tool
  away, name the episode that needed it and did something worse instead.
- **A tool the harness itself uses is load-bearing at zero model calls.**
  Deletion and re-execution complete the write algebra (insert, replace,
  delete, run); the recovery paths depend on them whether or not a model
  ever types the name.
- **A separate lookup is a lookup that will not happen.** If answering one
  question reliably requires a second call, carry the answer on the first
  result. A search hit now carries a haddock synopsis, because a type alone
  does not say which of `describeColumns`, `summarize` and `mean` answers
  "summary statistics". The full documentation stays available on demand:
  carrying the synopsis and offering the detail are complements, not
  alternatives.
- **Check a tool is on the surface before concluding it is unwanted.**
  `describe_function` recorded zero calls across every episode because the
  siza catalogue never offered it, not because any model declined it.
- **The return value is part of the surface.** Return what the caller needs
  in order to decide, not everything that is known. A field nobody acts on
  is a field that displaced one they would have.
- **Measure the spend.** Context cost per call is a design fact. Prefer a
  bounded synopsis to a full document, a ranked few to an exhaustive list,
  and say what was omitted rather than silently truncating.
- **Manage the context, not just the message.** A tool surface is only half
  the budget; the conversation carrying it is the other half, and it grows
  without anyone deciding to grow it. Seeding each prompt with the whole
  prior transcript took a three-part task from 3K characters before its
  first prompt to 250K before its third, and the work done per prompt fell
  away with it (live_test52-54).

  The split is by ROLE, not by age. Keep what the next prompt may refer to:
  the prompts themselves and the assistant's prose, because "animate it"
  needs the "it". Elide what nothing refers back to:

  * the model's own reasoning, scratchpad for the turn that produced it
    (14-24% of a transcript);
  * old tool results, by then either reflected in the notebook or rejected
    (43-54%).

  **Elide by reference, never by deletion.** Each dropped result leaves a
  line naming what produced it and how to read it, and a tool returns it in
  full. Losing the bytes is compression; losing the ability to get them back
  is amnesia.
- **Measure the payload, not the rendering.** A transcript written for a
  human reader is a proxy for what crossed the wire, and a generous one.
  Instrument the request before rebuilding anything on its size.
- **Results, never advice.** Tools report what is so. Anything that tells
  the model what to conclude rests on an inference the harness cannot make,
  and when it is wrong it is expensively wrong.
