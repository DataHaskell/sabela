# Haskell and Python in one notebook

Most data work ends up split across two languages. The shape of the data, the
rules it obeys, the transformations that must stay correct, all of that is a good
fit for Haskell and its types. The plotting, the quick numerical work, and the
machine-learning libraries live in Python. Moving between the two usually means
files on disk, a serialisation format, and a script to glue them together.

Sabela runs both in the same document. A Haskell cell and a Python cell sit in
one notebook, each in its own process, and values cross between them over a small
bridge. This notebook works through that, ending with a Haskell data type whose
values are drawn by matplotlib.

Python 3 needs to be on the path. Everything else is declared in the cells.

## A Python cell

A code cell runs Haskell by default. To make it a Python cell, pick **py** from
the language dropdown in the cell's gutter. The border turns yellow and the cell
runs against a Python interpreter instead.

```python
print("Hello from Python")
```

The interpreter stays alive between cells, so anything bound in one cell is in
scope in the cells below it, the same way a Haskell binding is.

```python
greeting = "from the same session"
numbers = [1, 2, 3, 4]
```

```python
print(greeting)
print("sum:", sum(numbers))
```

## Drawing with matplotlib

A Python cell can return rich output, not just text. The helpers `displayHtml`,
`displayMarkdown`, `displaySvg`, and `displayImage` each emit a value the notebook
renders inline. For matplotlib the one to reach for is `displayImage`, which takes
a MIME type and base64 data, so a figure is saved to a buffer and handed over as a
PNG.

The first cell sets up matplotlib and a small helper. The `# pip:` lines declare
the packages; Sabela installs them into a private virtual environment the first
time the cell runs, so there is nothing to install by hand.

```python
# pip: matplotlib
# pip: numpy
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import base64, io


def show_plot(fig=None):
    fig = fig or plt.gcf()
    buf = io.BytesIO()
    fig.savefig(buf, format="png", bbox_inches="tight", dpi=110)
    buf.seek(0)
    displayImage("image/png", base64.b64encode(buf.read()).decode())
    plt.close(fig)
```

With that in place, a plot is ordinary matplotlib followed by `show_plot`.

```python
x = np.linspace(0, 4 * np.pi, 200)
fig, ax = plt.subplots(figsize=(7, 3))
ax.plot(x, np.sin(x), label="sin", color="#c2674a")
ax.plot(x, np.cos(x), label="cos", color="#6083b0")
ax.set_title("A first figure")
ax.legend()
ax.grid(True, alpha=0.25)
show_plot(fig)
```

## Passing a value across

The bridge is the part that makes this one notebook rather than two. A Haskell
cell exports a value with `exportBridge`, giving it a name. Below, the same value
is waiting in a Python variable with a `_bridge_` prefix.

```haskell
let names = ["Tariro", "Farai", "Chipo"] :: [String]
exportBridge "names" (show names)
```

Bridge values always arrive as strings, so the receiving side parses them. A
Haskell list printed with `show` reads cleanly with Python's `ast.literal_eval`.

```python
import ast

names = ast.literal_eval(_bridge_names)
print("received", len(names), "names")
print("first:", names[0])
```

That is enough for lists and numbers. For anything with structure, it is worth
sending JSON.

## A Haskell type, plotted in Python

Here is the point of the whole exercise. Model the data in Haskell with a proper
type, then let Python draw it.

The type below is a record for a city, with a field whose type is itself a small
sum type for the climate. Deriving `Generic` lets Aeson encode any value of these
types to JSON without a hand-written instance. A nullary constructor like
`Temperate` encodes as the string `"Temperate"`, which is exactly what the Python
side will key its colours on.

```haskell
-- cabal: build-depends: aeson, bytestring
:set -XDeriveGeneric

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BL

data Climate = Tropical | Temperate | Arid | Continental
  deriving (Show, Eq, Generic)

instance ToJSON Climate

data City = City
  { cityName :: String
  , population :: Double
  , avgTempC :: Double
  , climate :: Climate
  }
  deriving (Show, Generic)

instance ToJSON City
```

The values are plain Haskell. Encoding the list gives one line of JSON, which the
bridge carries across as a string.

```haskell
let cities =
      [ City "Harare" 1.5 18.5 Temperate
      , City "Cairo" 9.5 22.1 Arid
      , City "Lagos" 15.4 27.0 Tropical
      , City "Nairobi" 4.4 19.0 Temperate
      , City "London" 9.0 11.8 Continental
      , City "Mumbai" 20.7 27.2 Tropical
      , City "Cape Town" 4.6 16.9 Temperate
      , City "Dubai" 3.6 28.0 Arid
      ]

exportBridge "cities" (BL.unpack (encode cities))
putStrLn ("encoded " ++ show (length cities) ++ " cities")
```

On the Python side the JSON becomes a list of dictionaries. The record fields are
the keys, and `climate` is the constructor name as a string, so it can index a
palette directly. Each city is a point placed by temperature and population and
coloured by its climate.

```python
import json

cities = json.loads(_bridge_cities)

palette = {
    "Tropical": "#c2674a",
    "Temperate": "#6f9355",
    "Arid": "#d8a657",
    "Continental": "#6083b0",
}

fig, ax = plt.subplots(figsize=(7.5, 4.5))
for c in cities:
    ax.scatter(
        c["avgTempC"], c["population"],
        s=140, color=palette[c["climate"]],
        edgecolor="#33312e", zorder=3,
    )
    ax.annotate(
        c["cityName"], (c["avgTempC"], c["population"]),
        xytext=(7, 4), textcoords="offset points", fontsize=9,
    )

for label, colour in palette.items():
    ax.scatter([], [], color=colour, edgecolor="#33312e", label=label)
ax.legend(title="climate", loc="upper left")
ax.set_xlabel("average temperature (degC)")
ax.set_ylabel("population (millions)")
ax.set_title("Cities by climate, modelled in Haskell")
ax.grid(True, alpha=0.25)
show_plot(fig)
```

The colours and the legend come straight from the `Climate` constructors. Add a
case to the sum type in Haskell, give it a colour in the palette, and the plot
follows.

## Sending results back

The bridge runs both ways. A Python cell can export a value, and a Haskell cell
below it reads it under the same `_bridge_` name. JSON is the natural carrier.

```python
import json

summary = {"count": len(cities), "hottest": max(cities, key=lambda c: c["avgTempC"])["cityName"]}
exportBridge("summary", json.dumps(summary))
```

```haskell
putStrLn ("Python sent: " ++ _bridge_summary)
```

## How it runs

Each language runs in its own subprocess. They share no memory; the only thing
that crosses is whatever you send over the bridge, and it always crosses as a
string, so the receiving side decides how to read it. CSV from a dataframe goes
through `pandas.read_csv(io.StringIO(...))`, a `show`-printed Haskell value
through `ast.literal_eval`, and JSON through `json.loads`.

When you run the whole notebook, Haskell cells run first in dependency order, then
Lean, then Python top to bottom. That ordering is why a Haskell value is ready by
the time a Python cell asks for it. Sessions persist until you reset them, so the
Python interpreter keeps its state between runs the same way the Haskell session
does.
