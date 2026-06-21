# Write clear, notebook-friendly code

A notebook is a document a human reads top to bottom, not a script. Every cell
you write or edit must be legible on its own. This is not optional polish; dense,
mixed-concern cells make the notebook hostile to read.

- **Separate logic from display.** Pure computation (parsing, aggregation,
  models, ranking) goes in its own cells; rendering (tables, charts,
  `displayMarkdown`, widgets) goes in separate cells that call it. A cell that
  mixes a dense fold with the SVG that draws it is unreadable. This mirrors the
  library-vs-view split.
- **One concept per cell.** Split a long cell that does several things into
  focused cells, each with a single responsibility and a one-line prose lead-in
  saying what question it answers. Never paste a whole subsystem into one cell.
- **Readable, modular code.** Name things for what they are (`pathsBefore`,
  `growthFor`), not `rp'`/`mb'`/`gp`/`ks`. Extract helpers and a type alias or
  two; do not cram nested logic into one expression the reader must hold in their
  head. A short top-level comment states intent.
- **Top-level bindings, not `do let`.** Write a cell's values as top-level
  definitions (`totalDelta = pTotal new - pTotal base`,
  `topRegressors = take 12 regressors`), not stuffed into a `do let … ` block. A
  `do let` forces every binding into one indented wall and pushes you to keep
  cramming; flat top-level bindings each stand alone and read in order. Reserve a
  trailing `do` for the actual effects (the `display*`/`showTable` calls).
  Top-level names are global (reactivity-tracked, can't collide across cells), so
  name them descriptively — never generic `top`/`lead`/`nm`.
- **Shallow `where`/`let`.** Keep local bindings to one level of nesting and at
  most three per clause. The moment a `where` grows a nested `let` (a second
  level) or a fourth binding, lift that inner computation to its own top-level (or
  `-- compile`) helper, parameterised over what it needs. Deeply nested local
  scopes are the hardest notebook code to read.
- **Narrate.** Put prose between cells that makes a claim the next cell
  substantiates. Hide setup (parsers, helpers) under a collapsed "Setup" heading;
  surface the results.
- **Keep heavy work off the default path.** A cell that walks a large structure
  and is slow should not run on every load: gate it behind an on-demand helper
  (`drill "name"`) or compute only what the visible result needs.
- **Compile heavy pure logic.** A tree walk or fold over a large structure in an
  *interpreted* cell can exceed the cell timeout and wedge the kernel. Put pure
  heavy functions in a `-- compile:` cell (native `-O2`) and call them from
  interpreted cells. A pure function can be compiled even when its arguments are
  interpreted runtime values: parameterise it over those values instead of
  closing over them (e.g. take `before`, `after` as `Prof` arguments rather than
  referencing the interpreted `base`/`new`).
- **Describe results, not the algorithm's housekeeping.** Drop headings like
  "Ruling out the false leads — renames" or "Excluding the pseudo-centres": the
  technique is built in, so describe what the reader sees in the result, not the
  implementation's defensive steps. Prose narrates findings, not the algorithm's
  internal book-keeping.
- **Prose style.** Plain and detached, in the user's voice. No em dashes (use a
  colon or parens instead). No marketing or sales tone. No "X, not Y" / "rather
  than" contrasts. Headings are informative and gentle, not reveals ("What grew",
  not "The answer").
