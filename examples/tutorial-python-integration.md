# Tutorial: Python Integration in Sabela

This tutorial covers Python cells in Sabela — from basic execution to building polyglot data science pipelines that combine Haskell's type safety with Python's ML ecosystem.

## Prerequisites

Python 3 must be on your PATH. Verify with `python3 --version`.

Install any libraries you plan to use (pandas, scikit-learn, etc.) into your system or virtualenv:

    pip install pandas scikit-learn matplotlib numpy

## Creating a Python cell

Click the language dropdown in any code cell's gutter and select **py**. The cell border turns yellow and you get Python syntax highlighting.

## Basic execution


```python
print("Hello from Python!")
```

> <!-- sabela:mime text/plain -->
> Hello from Python!


Python cells run in a persistent REPL — variables defined in one cell are available in cells below.


```python
x = 42
y = [1, 2, 3]
print(f"x = {x}, y = {y}")
```

> <!-- sabela:mime text/plain -->
> x = 42, y = [1, 2, 3]



```python
# This cell can see x and y from above
print(f"x * 2 = {x * 2}")
print(f"sum(y) = {sum(y)}")
```

> <!-- sabela:mime text/plain -->
> x * 2 = 84
> sum(y) = 6


## Multiline code

Functions, classes, loops, and all Python constructs work naturally:


```python
def fibonacci(n):
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a

for i in range(10):
    print(f"fib({i}) = {fibonacci(i)}")
```

> <!-- sabela:mime text/plain -->
> fib(0) = 0
> fib(1) = 1
> fib(2) = 1
> fib(3) = 2
> fib(4) = 3
> fib(5) = 5
> fib(6) = 8
> fib(7) = 13
> fib(8) = 21
> fib(9) = 34


## Rich output

Python cells support the same MIME output as Haskell cells:


```python
# HTML output
displayHtml("<h3 style='color: #89b4fa'>Styled heading</h3><p>This is <b>rich</b> HTML output.</p>")
```

> <!-- sabela:mime text/html -->
> <h3 style='color: #89b4fa'>Styled heading</h3><p>This is <b>rich</b> HTML output.</p>



```python
# Markdown output
displayMarkdown("""
| Language | Role |
|----------|------|
| Haskell  | Compute |
| Lean     | Verify  |
| Python   | Analyze |
""")
```

> <!-- sabela:mime text/markdown -->
> 
> | Language | Role |
> |----------|------|
> | Haskell  | Compute |
> | Lean     | Verify  |
> | Python   | Analyze |



```python
# JSON output
import json
displayJson(json.dumps({"status": "ok", "languages": ["Haskell", "Lean4", "Python"]}, indent=2))
```

> <!-- sabela:mime application/json -->
> {
>   "status": "ok",
>   "languages": [
>     "Haskell",
>     "Lean4",
>     "Python"
>   ]
> }


## Receiving data from Haskell

When a Haskell cell exports a value with `exportBridge`, it becomes available as a Python string variable.

**Haskell cell:**

```haskell
-- cabal: build-depends: text
:set -XOverloadedStrings

let names = ["Alice", "Bob", "Carol"] :: [String]
exportBridge "names" (show names)
```


**Python cell (below the Haskell cell):**

```python
# The bridge value arrives as a string
print(f"Raw bridge value: {_bridge_names}")

# For Haskell `show` format, ast.literal_eval can parse it
import ast
names = ast.literal_eval(_bridge_names)
print(f"Parsed list: {names}")
print(f"First name: {names[0]}")
```

> <!-- sabela:mime text/plain -->
> Raw bridge value: ["Alice","Bob","Carol"]
> Parsed list: ['Alice', 'Bob', 'Carol']
> First name: Alice


## Sending data to Haskell

Python can export values back to Haskell using `exportBridge`:


```python
import json

result = {"accuracy": 0.95, "model": "gradient_boosting"}
exportBridge("model_info", json.dumps(result))
```


**Haskell cell (below the Python cell):**

```haskell
putStrLn $ "From Python: " ++ _bridge_model_info
```

> <!-- sabela:mime text/plain -->
> From Python: {"accuracy": 0.95, "model": "gradient_boosting"}


## The DataFrame pattern

The most common use case: analyze data in Haskell, train models in Python.

**Step 1: Haskell prepares data**


```haskell
-- cabal: build-depends: dataframe
:set -XOverloadedStrings

import qualified DataFrame as D

df <- D.readCsv "./examples/data/housing.csv"
import qualified Data.Text as T

let sample = D.take 200 df
exportBridge "data" (T.unpack (D.toCsv sample))

putStrLn $ "Exported " ++ show (D.nRows sample) ++ " rows (sampled from " ++ show (D.nRows df) ++ ")"
```

> <!-- sabela:mime text/plain -->
> Exported 200 rows (sampled from 20640)


**Step 2: Python loads and trains**


```python
# pip: pandas
import pandas as pd
import io

df = pd.read_csv(io.StringIO(_bridge_data))
print(f"Loaded {len(df)} rows, {len(df.columns)} columns")
print(df.head())
```

> <!-- sabela:mime text/plain -->
> Loaded 200 rows, 10 columns
>    longitude  latitude  ...  median_house_value  ocean_proximity
> 0    -122.23     37.88  ...            452600.0         NEAR BAY
> 1    -122.22     37.86  ...            358500.0         NEAR BAY
> 2    -122.24     37.85  ...            352100.0         NEAR BAY
> 3    -122.25     37.85  ...            341300.0         NEAR BAY
> 4    -122.25     37.85  ...            342200.0         NEAR BAY
> 
> [5 rows x 10 columns]


## Tips for the bridge

- **CSV strings** (from `D.toCsv`): Use `pd.read_csv(io.StringIO(bridge_var))`
- **Haskell `show` format** (like `[1,2,3]`): Use Python's `ast.literal_eval()` to parse
- **JSON strings**: Use `json.loads(bridge_var)`
- Bridge values are always **strings** — parse them in the target language

## Execution model

- Python cells run top-to-bottom in document order (like a Jupyter notebook)
- Variables persist across cells in the same session
- The session starts automatically on the first Python cell execution
- `Reset` kills all sessions (Haskell, Lean, and Python)
- Each language runs in its own subprocess — they don't share memory, only the bridge

## Error handling

Python errors show as cell errors with the full traceback:


```python
# This will show a traceback in the cell output
1 / 0
```


## Imports and packages

Use any installed Python package. There's no dependency resolution like Haskell's `-- cabal:` comments — manage your Python packages with pip as usual.


```python
import numpy as np
print(f"NumPy {np.__version__}")
print(f"Random matrix:\n{np.random.randn(3, 3).round(2)}")
```

> <!-- sabela:mime text/plain -->
> NumPy 2.4.4
> Random matrix:
> [[ 0.68  0.13  1.  ]
>  [ 1.62 -0.81 -1.55]
>  [-0.44  1.96 -0.64]]


## Execution order across languages

When you click "Run All":

1. **Haskell cells** run first (in dependency order)
2. **Lean cells** run second (full document sent to LSP)
3. **Python cells** run third (top-to-bottom)

This means Haskell exports are available to both Lean and Python. Lean exports are available to Python (and to Haskell cells that reference them — those are re-run automatically).
