# Tutorial: Lean 4 Integration in Sabela

This tutorial covers everything you need to know about using Lean 4 in Sabela notebooks — from writing your first theorem to building cross-language verification pipelines.

## Prerequisites

Install Lean 4 and Lake (the Lean build tool):

    curl https://elan.lean-lang.org/install.sh -sSf | sh
    source ~/.elan/env

Verify with `lake --version`.

## Creating a Lean cell

Any code cell can be switched to Lean using the language dropdown in the cell's gutter (top-left corner). Click the label that says **hs** and select **lean**.

New code cells default to Haskell. To write Lean, just switch the dropdown.

## Your first Lean cell

```lean4
#eval "Hello from Lean 4!"
```

`#eval` evaluates an expression and prints the result. It works for any type with a `Repr` or `ToString` instance.

## Evaluation examples

Lean is a full programming language, not just a proof assistant.

```lean4
-- Arithmetic
#eval 2^10

-- String operations
#eval "Sabela".append " Notebook"

-- List operations
#eval [1, 2, 3].map (· * 10)

-- Function definitions persist across cells
def fibonacci : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fibonacci n + fibonacci (n + 1)

#eval (List.range 10).map fibonacci
```

## Writing proofs

Lean's real power is theorem proving. A theorem is a type whose inhabitants are proofs.

```lean4
-- A trivial proof by reflexivity
theorem one_plus_one : 1 + 1 = 2 := rfl

-- Proof by simplification
theorem add_zero (n : Nat) : n + 0 = n := by simp

-- Proof by induction
theorem add_comm (m n : Nat) : m + n = n + m := by
  induction m with
  | zero => simp
  | succ m ih => simp [Nat.succ_add, ih]
```

When a proof succeeds, Sabela shows a green checkmark with "No goals". When it fails, you see the remaining proof state.

## Definitions persist across cells

Like Haskell cells, Lean definitions accumulate. A definition in one cell is visible to all cells below it.

```lean4
def double (n : Nat) := n * 2

-- This cell can use `double` from above
#eval double 21
```

## Imports

Lean's standard library is available. Imports from any cell are automatically hoisted to the top of the document.

```lean4
import Lean.Data.HashMap

-- This works because the import is hoisted
def myMap := Lean.HashMap.empty.insert "key" 42

#eval myMap.find? "key"
```

## Using Mathlib

For advanced mathematics, add Mathlib as a Lake dependency with a comment annotation:

```lean4
-- lake: require mathlib from leanprover-community

import Mathlib.Tactic

-- Now you have access to powerful tactics like `ring`, `norm_num`, `linarith`
example (a b : Int) : (a + b)^2 = a^2 + 2*a*b + b^2 := by ring
```

The first run with Mathlib takes several minutes (it downloads and builds the library). Subsequent runs are cached.

## Receiving data from Haskell

Haskell cells can export values to Lean using `exportBridge`:

```haskell
exportBridge "primes" (show [2, 3, 5, 7, 11, 13])
```

The value appears in Lean as `_bridge_primes : String`:

```lean4
#eval _bridge_primes  -- "[2,3,5,7,11,13]"
```

## Sending data to Haskell

Lean cells can export `#eval` results using a comment annotation:

```lean4
-- export: lean_result
#eval toString (fibonacci 15)
```

The Haskell cell below receives it as `_bridge_lean_result`:

```haskell
putStrLn $ "Fibonacci(15) from Lean: " ++ _bridge_lean_result
```

## Reactivity

When you edit a Lean cell, only that cell and all cells below it are re-checked (Lean elaborates top-to-bottom). Cells above the edit point are cached by the LSP server, so edits at the bottom of a notebook are fast.

## Cell execution model

Unlike Haskell cells which run individually, all Lean cells are assembled into a single `.lean` file and sent to the Lean language server. This means:

- Definitions naturally flow downward
- Errors in one cell may affect cells below it
- The LSP provides incremental checking — only changed regions are re-elaborated

## Tips

- Use `#check` to inspect types: `#check Nat.add_comm`
- Use `#print` to see definitions: `#print Nat.rec`
- Use `sorry` as a placeholder in incomplete proofs
- The `omega` tactic handles linear arithmetic over `Nat` and `Int`
- The `simp` tactic simplifies using known lemmas
- The `decide` and `native_decide` tactics work for decidable propositions
