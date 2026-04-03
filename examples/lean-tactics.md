# Lean 4: Commands and Tactic Proofs

This notebook demonstrates Lean 4's dual nature as both a programming language and a proof assistant. We define data types and functions (commands), then prove properties about them (tactics).

## Defining a binary tree

An inductive type for binary trees with values at the leaves:

```lean4
inductive Tree (α : Type) where
  | leaf : α → Tree α
  | node : Tree α → Tree α → Tree α
deriving Repr
```

## Computing with trees

Recursive functions over our tree type — size counts leaves, depth measures the longest path:

```lean4
def Tree.size : Tree α → Nat
  | .leaf _ => 1
  | .node l r => l.size + r.size

def Tree.depth : Tree α → Nat
  | .leaf _ => 0
  | .node l r => 1 + max l.depth r.depth

def Tree.map (f : α → β) : Tree α → Tree β
  | .leaf x => .leaf (f x)
  | .node l r => .node (l.map f) (r.map f)

-- Test with a small tree
def exTree := Tree.node (Tree.node (Tree.leaf 1) (Tree.leaf 2)) (Tree.leaf 3)

#eval exTree.size   -- 3
#eval exTree.depth  -- 2
```

## Proving size is always positive

Every tree has at least one leaf, so size is always at least 1:

```lean4
theorem Tree.size_pos (t : Tree α) : t.size ≥ 1 := by
  induction t with
  | leaf _ => simp [Tree.size]
  | node l r ihl ihr => simp [Tree.size]; omega
```

## Map preserves structure

Mapping a function over a tree doesn't change its shape:

```lean4
theorem Tree.size_map (f : α → β) (t : Tree α) : (t.map f).size = t.size := by
  induction t with
  | leaf _ => simp [Tree.map, Tree.size]
  | node l r ihl ihr => simp [Tree.map, Tree.size, ihl, ihr]

theorem Tree.depth_map (f : α → β) (t : Tree α) : (t.map f).depth = t.depth := by
  induction t with
  | leaf _ => simp [Tree.map, Tree.depth]
  | node l r ihl ihr => simp [Tree.map, Tree.depth, ihl, ihr]
```

## Depth bounds size

The depth of a tree is always less than its size (since depth counts edges on the longest path, while size counts all leaves):

```lean4
theorem Tree.depth_lt_size (t : Tree α) : t.depth < t.size := by
  induction t with
  | leaf _ => simp [Tree.size, Tree.depth]
  | node l r ihl ihr =>
    simp [Tree.size, Tree.depth]
    omega
```

## A verified lookup function

Define a mirroring operation and prove it is involutive (applying it twice gives back the original):

```lean4
def Tree.mirror : Tree α → Tree α
  | .leaf x => .leaf x
  | .node l r => .node r.mirror l.mirror

theorem Tree.mirror_mirror (t : Tree α) : t.mirror.mirror = t := by
  induction t with
  | leaf _ => simp [Tree.mirror]
  | node l r ihl ihr => simp [Tree.mirror, ihl, ihr]
```

## Composing proofs

We can combine earlier results. Mirror preserves size because it just swaps children:

```lean4
theorem Tree.size_mirror (t : Tree α) : t.mirror.size = t.size := by
  induction t with
  | leaf _ => simp [Tree.mirror, Tree.size]
  | node l r ihl ihr =>
    simp [Tree.mirror, Tree.size, ihl, ihr]
    omega
```
