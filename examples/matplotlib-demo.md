# Matplotlib from Sabela

Python cells can render matplotlib plots inline using base64-encoded PNG.

## Setup

```python
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
import base64, io

def show_plot(fig=None):
    if fig is None:
        fig = plt.gcf()
    buf = io.BytesIO()
    fig.savefig(buf, format='png', bbox_inches='tight', dpi=100)
    buf.seek(0)
    displayImage("image/png", base64.b64encode(buf.read()).decode())
    plt.close(fig)
```

## Line plot

```python
x = np.linspace(0, 4 * np.pi, 200)
fig, ax = plt.subplots(figsize=(8, 3))
ax.plot(x, np.sin(x), label='sin(x)')
ax.plot(x, np.cos(x), label='cos(x)')
ax.set_title('Trigonometric functions')
ax.legend()
ax.grid(True, alpha=0.3)
show_plot(fig)
```

## Bar chart

```python
categories = ['Haskell', 'Lean', 'Python', 'Rust', 'Go']
values = [85, 72, 95, 78, 68]
colors = ['#89b4fa', '#a6e3a1', '#f9e2af', '#fab387', '#cba6f7']

fig, ax = plt.subplots(figsize=(7, 3))
ax.bar(categories, values, color=colors)
ax.set_ylabel('Score')
ax.set_title('Language benchmark')
show_plot(fig)
```

## Scatter plot with Haskell data

Haskell prepares the data, Python visualizes it.

```haskell
let xs = map (\i -> sin (fromIntegral i * 0.1) * 10 + fromIntegral i :: Double) [1..50]
exportBridge "scatter_x" (show xs)
```

```haskell
let ys = map (\i -> cos (fromIntegral i * 0.15) * 8 + fromIntegral i * 0.5 :: Double) [1..50]
exportBridge "scatter_y" (show ys)
```

```python
import ast
xs = ast.literal_eval(_bridge_scatter_x)
ys = ast.literal_eval(_bridge_scatter_y)

fig, ax = plt.subplots(figsize=(7, 4))
scatter = ax.scatter(xs, ys, c=range(len(xs)), cmap='viridis', s=40)
fig.colorbar(scatter, ax=ax, label='index')
ax.set_title('Haskell data, matplotlib rendering')
ax.set_xlabel('x')
ax.set_ylabel('y')
show_plot(fig)
```

## Histogram

```python
data = np.random.seed(42) or np.random.normal(0, 1, 1000)
fig, ax = plt.subplots(figsize=(7, 3))
ax.hist(data, bins=40, color='#89b4fa', edgecolor='#1e1e2e', alpha=0.8)
ax.set_title('Normal distribution (n=1000)')
ax.axvline(0, color='#f38ba8', linestyle='--', label='mean')
ax.legend()
show_plot(fig)
```
