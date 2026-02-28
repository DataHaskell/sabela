# sabela

Sabela is a reactive notebook environment for Haskell. The name is derived from the [Ndebele](https://en.wikipedia.org/wiki/Northern_Ndebele_language) word meaning "to respond." The project has two purposes. Firstly, it is an attempt to design and create a modern Haskell notebook where reactivity is a first class concern. Secondly, it is an experiment ground for package/environment management in Haskell notebooks (a significant pain point in IHaskell).

![A screenshot of the web ui](./static/images/screenshot.png)

## Quick start

```bash
git clone https://github.com/DataHaskell/sabela
cd sabela
cabal update
cabal run
```

Open `localhost:3000/index.html` and explore either:

* `./examples/analysis.md` for a quick tutorial.
* or click on the book icon on the top left for some ready to use snippets.

The execution and dependency management model is based on [scripths](https://github.com/DataHaskell/scripths).

## Tutorial

## What Sabela is good at

Sabela is aimed at exploratory Haskell work where you want:

* regular Haskell code, not a special notebook language
* reactive reruns when upstream cells change
* package directives via `-- cabal:` metadata
* Markdown prose mixed with executable Haskell
* rich output in the browser
* a file explorer and save/load workflow for `.md` notebooks

---

## 1. Install and run

Clone the repo and start the server:

```bash
git clone https://github.com/DataHaskell/sabela
cd sabela
cabal update
cabal run
```

Then open:

```text
http://localhost:3000/index.html
```

By default, Sabela serves the UI from `static/` and uses the current working directory as the file explorer root.

You can also pass explicit arguments:

```bash
cabal run sabela -- 3000 static .
```

The CLI shape is:

```text
sabela [port] [static-dir] [work-dir]
```

---

## 2. The notebook model

A Sabela notebook is just a Markdown file containing prose plus fenced Haskell code blocks.

For example:

````markdown
# My first Sabela notebook

This is prose.

```haskell
x = 10
```

More prose.

```haskell
print (x + 5)
```
````

When Sabela loads a notebook:

* prose sections become **prose cells**
* fenced code blocks become **code cells**
* running the notebook executes the code cells in order

When you save, Sabela writes the current notebook state back out as Markdown again.

That means notebooks stay readable in Git, easy to diff, and editable outside the website.

---

## 3. Your first reactive notebook

Create a file called `examples/tutorial.md`:

````markdown
# Sabela basics

This notebook shows the core reactive workflow.

```haskell
x = 10
```

```haskell
y = 20
```

```haskell
print (x + y)
```
````

Now change the first cell from:

```haskell
x = 10
```

to:

```haskell
x = 100
```

Sabela tracks definitions and uses heuristically, so when an upstream cell changes, downstream cells that depend on those names are rerun automatically.

In this example, the final cell should update from `30` to `120`.

This is the central Sabela workflow:

1. define values in small cells
2. compose later cells from earlier ones
3. edit upstream definitions
4. let the notebook rerun affected dependents

---

## 4. How reactivity works today

Sabela does **not** currently build a full Haskell dependency graph. Instead, it uses a lightweight textual approximation:

* it scans each code cell for names it appears to define
* it scans for names it appears to use
* when a cell is edited, later code cells are rerun if they use names defined by changed cells

This approach is simple and fast, and it works well for many didactic and exploratory notebooks.

You should still understand its limits:

* it is order-sensitive
* it is heuristic, not compiler-accurate
* unusual syntax may not be tracked perfectly
* circular dependencies are not deeply modeled yet

So the best style for Sabela notebooks is:

* keep cells small
* define values and helper functions clearly
* prefer a top-to-bottom narrative order

---

## 5. Running plain Haskell

Anything that works in GHCi generally fits naturally in a Sabela code cell.

Example:

```haskell
let triples =
      [ (a,b,c)
      | c <- [1..20]
      , b <- [1..c]
      , a <- [1..b]
      , a*a + b*b == c*c
      ]

print triples
```

Sabela ships with a small gallery of built-in examples in the UI, including basics, library usage, display examples, concurrency, QuickCheck, and file I/O.

---

## 6. Adding package dependencies inside a cell

One of Sabela’s most important ideas is that notebook package requirements live inside the notebook itself.

You do this with `-- cabal:` metadata at the top of a code cell.

Example:

```haskell
-- cabal: build-depends: text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

let msg = T.pack "Hello, Sabela!"
TIO.putStrLn (T.toUpper msg)
```

You can also declare extensions:

```haskell
-- cabal: build-depends: aeson, text, bytestring
-- cabal: default-extensions: DeriveGeneric, OverloadedStrings
```

### What happens under the hood

Sabela scans **all code cells**, merges their `-- cabal:` metadata, and computes the full required package/extension set for the notebook.

If the dependency set changes, Sabela:

1. resolves or updates the package environment
2. restarts the GHCi session
3. injects its display helper prelude
4. reruns the relevant cells

That means dependencies are notebook-level in effect, even though the directives are written in cells.

A practical tip: put your main dependency directives near the top of the notebook so the environment story is easy to read.

Note: Sabela curently only supports exact versions - not the cabal package range syntax.

That is: `dataframe-0.5.0.0` will work but `dataframe <= 1` won't.

---

## 7. Rich output helpers

Sabela injects helper functions into the GHCi session so cells can emit structured browser output.

The key helpers are:

* `displayHtml`
* `displayMarkdown`
* `displaySvg`
* `displayLatex`
* `displayJson`
* `displayImage`

If you just `print` something, it is treated as plain text.
Note: rich output text must be the only thing output in the cell.

### Markdown output

```haskell
displayMarkdown $ unlines
  [ "# Analysis Results"
  , ""
  , "The computation found **42** as the answer."
  , ""
  , "| Metric | Value |"
  , "|--------|-------|"
  , "| Speed | Fast |"
  , "| Memory | Low |"
  ]
```

### HTML output

```haskell
displayHtml $ unlines
  [ "<h2>Hello from Sabela</h2>"
  , "<p>This is <strong>rich HTML</strong> output.</p>"
  , "<ul><li>Item one</li><li>Item two</li></ul>"
  ]
```

### SVG output

```haskell
-- cabal: build-depends: text, granite
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Granite.Svg

displaySvg $ T.unpack
  (bars [("Q1",12),("Q2",18),("Q3",9),("Q4",15)] defPlot { plotTitle = "Sales" })
```

These helpers work by prefixing output with a MIME marker that the server parses before sending results to the browser.

---

## 8. A minimal didactic notebook

Here is a complete small notebook you can drop into `examples/minimal.md`.

````markdown
# A tiny Sabela notebook

This notebook demonstrates reactivity, Markdown output, and an SVG plot.

```haskell
numbers = [1..10]
```

```haskell
squares = map (^ (2 :: Int)) numbers
```

```haskell
print squares
```

```haskell
displayMarkdown $ unlines
  [ "# Summary"
  , ""
  , "We computed **squares** for the numbers 1 through 10."
  , ""
  , "- Count: 10"
  , "- Max: 100"
  ]
```
````

Edit `numbers`, and the downstream cells should update.

---

## 9. The DataHaskell example notebook

The repository includes a larger example at `examples/analysis.md` based on the California housing dataset.

That notebook shows a very good real Sabela workflow:

* load data with `DataFrame`
* inspect rows
* compute summaries
* get categorical frequencies
* plot histograms
* create derived features
* declare typed column references with Template Haskell
* visualize spatial structure with scatter plots
* compute correlations against a target variable

## 10. Looking up names from the current session

Sabela also provides IDE-style help for the active notebook session.

The UI exposes a lookup panel, and the backend can query GHCi for:

* completions
* `:info`
* `:type`
* `:doc`

So once your notebook has loaded modules and defined names, you can inspect them from the same live session.

This is especially useful for exploratory work where you are mixing notebook execution with interactive discovery.

---

## 11. Files, loading, and saving

Sabela includes a file explorer rooted at the configured working directory.

That gives you a nice workflow for:

* opening existing Markdown notebooks
* creating new files and directories
* reading and editing files
* saving notebooks back to disk

A useful convention is something like:

```text
examples/
  basics.md
  dataframe_intro.md
  plotting.md
  california_housing.md
```

Since notebooks are plain Markdown, they work very naturally with Git and code review.

---

## 12. Errors and debugging

When a cell fails, Sabela captures stderr from GHCi and parses error locations into structured cell errors.

In practice, that means:

* ordinary compile/runtime messages still appear
* line/column information is surfaced when available
* fixing a broken upstream cell can automatically repair downstream cells on rerun

A few debugging tips:

* keep imports and dependency pragmas near the top
* isolate complicated definitions into their own cells
* prefer explicit helper names over deeply nested one-liners
* use `print` for plain debugging and `displayMarkdown` / `displayHtml` for presentation

---

## 13. How Sabela executes cells

The execution model is worth understanding because it explains most notebook behavior.

### Session lifecycle

Sabela maintains a single GHCi session for the notebook.

When needed, it starts GHCi roughly with:

* `--interactive`
* `-ignore-dot-ghci`
* language extensions from notebook metadata
* package flags or package environment information derived from notebook metadata

### Cell execution

For a given code cell, Sabela:

1. parses the source as a script fragment
2. renders it into GHCi-friendly script text
3. sends the lines to the running session
4. places a unique marker after the cell
5. drains stdout until the marker appears
6. collects stderr separately
7. parses MIME markers and error information
8. broadcasts the result to the frontend

This marker-based approach is the trick that lets Sabela separate one cell’s output from the next while still using a single long-lived GHCi process.

---

## 14. Current limitations to know about

Sabela is already quite usable, but it is still an early system. A good tutorial should be honest about that.

Current constraints include:

* dependency tracking is heuristic rather than compiler-accurate
* cell scheduling is currently linear rather than topologically sorted
* circular dependencies are not deeply handled
* notebook semantics are tied to a single session model
* package environment changes require session restart

These are not necessarily flaws; many are reasonable early tradeoffs for a simple and understandable architecture.

---

## 15. A good notebook style for Sabela

If you want notebooks that feel clean and robust, use this style:

### Put setup first

Start with:

* package directives
* extensions
* imports
* small helper functions

### Separate stages

Use one cell per conceptual step:

* data loading
* inspection
* cleaning
* feature engineering
* plotting
* modeling
* interpretation

### Keep cells readable

Prefer:

* named intermediate values
* small helper functions
* explicit imports
* prose between major steps
* for memory efficiency don't declare expensive things in top level variables.

### Use prose cells seriously

Sabela works best when the notebook is both:

* executable
* readable as a document

That means prose should explain:

* what you are doing
* why you are doing it
* what the output means
* what the next cell will test

---
