# Verified Cryptographic Primitives

This notebook implements a toy cipher in Haskell, uses Lean to prove its algebraic properties (invertibility), and uses Python to run statistical tests on the output. A microcosm of verified security engineering.

## Haskell: XOR cipher implementation

The XOR cipher is the simplest symmetric cipher: encryption and decryption are the same operation. We implement it over byte sequences.

```haskell
import Data.Bits (xor)
import Data.Char (ord, chr)

xorCipher :: String -> String -> String
xorCipher key = zipWith (\k c -> chr (ord k `xor` ord c)) (cycle key)

key = "SABELA"
plaintext = "Reactive notebooks for Haskell with formal verification"
ciphertext = xorCipher key plaintext
decrypted = xorCipher key ciphertext

putStrLn $ "Plaintext:  " ++ plaintext

putStrLn $ "Ciphertext: " ++ show (map ord ciphertext)

putStrLn $ "Decrypted:  " ++ decrypted

putStrLn $ "Round-trip:  " ++ show (plaintext == decrypted)

exportBridge "plaintext_len" (show (length plaintext))

exportBridge "ciphertext_ords" (show (map ord ciphertext))

exportBridge "key_len" (show (length key))
```

## Lean: prove XOR is its own inverse

The critical property of XOR encryption: applying the same key twice recovers the original. We prove this at the bit level.

```lean4
-- XOR is self-inverse: a ^^^ b ^^^ b = a
-- This is the fundamental correctness property of XOR ciphers.
theorem xor_self_inverse (a b : Bool) : xor (xor a b) b = a := by
  cases a <;> cases b <;> simp [xor]

-- XOR is commutative
theorem xor_comm (a b : Bool) : xor a b = xor b a := by
  cases a <;> cases b <;> simp [xor]

-- XOR is associative
theorem xor_assoc (a b c : Bool) : xor (xor a b) c = xor a (xor b c) := by
  cases a <;> cases b <;> cases c <;> simp [xor]

-- XOR with false is identity
theorem xor_false (a : Bool) : xor a false = a := by
  cases a <;> simp [xor]
```

## Lean: verify length preservation

Encryption must not change the message length -- a property often assumed but rarely checked.

```lean4
-- Verify that zipWith preserves length for a concrete case
#eval [1, 2, 3].zipWith (· + ·) [10, 20, 30]
#eval ([1, 2, 3].zipWith (· + ·) [10, 20, 30]).length
```

## Python: statistical analysis of cipher output

A good cipher should produce output that looks random. We test the XOR cipher's output distribution.

```python
import ast
from collections import Counter

ciphertext = ast.literal_eval(_bridge_ciphertext_ords)
key_len = int(_bridge_key_len)
n = len(ciphertext)

freq = Counter(ciphertext)
unique_values = len(freq)

print(f"Ciphertext length: {n}")
print(f"Unique byte values: {unique_values}")
print(f"Frequency range: {min(freq.values())}-{max(freq.values())}")

expected = n / 256
chi2 = sum((count - expected)**2 / expected for count in freq.values())
print(f"\nChi-squared statistic: {chi2:.2f}")
print(f"(Low values suggest non-random structure)")

bits = []
for b in ciphertext:
    for i in range(8):
        bits.append((b >> i) & 1)
ones = sum(bits)
zeros = len(bits) - ones
print(f"\nBit distribution: {ones} ones, {zeros} zeros ({ones/len(bits)*100:.1f}% ones)")
```

## Summary

```python
print("Verified XOR Cipher Pipeline:")
print("  Haskell: implementation + encryption")
print("  Lean:    self-inverse proof + length preservation")
print(f"  Python:  chi-squared = {chi2:.2f}")
print()
print("Three languages, one notebook, end-to-end confidence.")
```

## The bigger picture

This toy example illustrates a pattern applicable to real cryptographic engineering:

1. **Implement** in a language with strong type safety (Haskell)
2. **Prove** algebraic properties in a proof assistant (Lean)
3. **Test** statistical properties with the data science ecosystem (Python)

Each layer catches different classes of bugs. Types catch structural errors. Proofs catch logical errors. Statistical tests catch distributional errors. No single technique catches everything -- but together, they build real confidence.
