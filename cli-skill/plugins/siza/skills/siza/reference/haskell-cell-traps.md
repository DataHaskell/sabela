# In-cell Haskell traps

A handful of things type-check or run but behave wrongly in the live session.
These cost real debugging time because the failure is silent or the error
misleading. The GHC error or the silent no-op announces itself — read this
reference when you hit the symptom.

## `fromList` needs a type application

`DataFrame`'s `fromList` is return-type-polymorphic (an `Unboxable`/`Columnable`
constraint it can't infer from the list), so a bare `fromList xs` fails with
`GHC-80003: Non type-variable argument in the constraint: SBoolI (… Unboxable
a1)` **and a flatly wrong fix suggestion** ("use record field
`TruncateConfig.maxRows`"). Pin it at the call site: `fromList @Text xs`,
`fromList @Double xs`. (A top-level signature on the whole builder also works
but is heavier.)

## `Data.Text.dropWhileEnd` can silently no-op in-session

`T.dropWhileEnd C.isDigit "abc99"` has returned `"abc99"` unchanged in a live
cell (suspected fusion/RULES interaction; unconfirmed) — a *correctness* trap
with no error, no warning. If a trailing-strip silently does nothing, rewrite as
`T.reverse . T.dropWhile p . T.reverse`.

## Multiple effects need a `do` block

Two bare `putStrLn (…)` statements on consecutive lines parse as one
application — `putStrLn (…) putStrLn (…)` — and fail with a confusing "applied
to N visible arguments". Some statement pairs (e.g. consecutive `showTable …`)
*do* split, so the behaviour is inconsistent; the safe rule is **multiple
effectful statements → wrap them in a `do` block**.

## Reconcile non-additive metrics before you headline them

Ranking by an inherited/subtree/`%`/ratio metric as if it summed produces a
plausible, confident, *wrong* story (a measured case: an inherited-cost metric
summed to ~80× the whole program). Nothing in the toolchain pushes back. Before
narrating "the biggest contributor is X", check a reconciliation invariant —
e.g. Σ(individual deltas) == total delta — in a scratch cell.
