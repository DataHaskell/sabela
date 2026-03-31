# Lean 4: Proving Properties About Natural Numbers

This notebook demonstrates Lean 4's theorem proving capabilities within Sabela.
We build up from basic definitions to non-trivial proofs about natural number arithmetic.

## Warm-up: Evaluation

First, let's verify Lean can evaluate expressions:

```lean4
#eval 2 + 3 * 7
```

## Defining a recursive function

A recursive function that sums the first *n* natural numbers:

```lean4
def sumTo : Nat → Nat
  | 0     => 0
  | n + 1 => (n + 1) + sumTo n

#eval sumTo 100  -- should be 5050
```

## Proving a property by induction

We can prove that `sumTo` is monotone: if you go further, the sum gets bigger.

```lean4
theorem sumTo_succ_ge (n : Nat) : sumTo (n + 1) ≥ sumTo n := by
  simp [sumTo]
```

## A list property: reverse is involutive

Proving that reversing a list twice yields the original list:

```lean4
theorem reverse_reverse (xs : List α) : xs.reverse.reverse = xs := by
  induction xs with
  | nil => simp
  | cons x xs ih => simp [ih]
```

## Sorted lists

Define what it means for a list of natural numbers to be sorted,
and prove that a singleton list is always sorted:

```lean4
def Sorted : List Nat → Prop
  | []          => True
  | [_]         => True
  | a :: b :: t => a ≤ b ∧ Sorted (b :: t)

theorem sorted_singleton (x : Nat) : Sorted [x] := by
  simp [Sorted]
```

## Non-empty append preserves head

If we prepend an element to a list, the head of the result is that element:

```lean4
theorem head_cons (x : α) (xs : List α) :
    (x :: xs).head (by simp) = x := by
  rfl
```

## Decidable equality

Proving that two computed values are equal, verified at type-check time:

```lean4
example : sumTo 10 = 55 := by native_decide
```
