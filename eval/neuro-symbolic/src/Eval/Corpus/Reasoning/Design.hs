module Eval.Corpus.Reasoning.Design (
    designTasks,
) where

import Eval.Task (Grader (..), Task (..))

designTasks :: [Task]
designTasks =
    [ lruCacheTask
    , tokenBucketTask
    , exprEvalTask
    , ringBufferTask
    ]

lruCacheTask :: Task
lruCacheTask =
    Task
        "lruCache"
        "Implement a capacity-bounded LRU (least-recently-used) cache with O(1) \
        \amortised get and put over Int keys and Char values. Provide exactly \
        \these three operations (define the cache type however you like):\n\n\
        \  newLRU :: Int -> LRU              -- empty cache of the given capacity\n\
        \  getLRU :: Int -> LRU -> (Maybe Char, LRU)  -- lookup; a hit becomes most-recently-used\n\
        \  putLRU :: Int -> Char -> LRU -> LRU         -- insert/update; evicts the LRU key when full\n\n\
        \A get that hits counts as a use (most-recently-used). Inserting a new \
        \key into a full cache evicts the least-recently-used key. Define the \
        \cache type as `LRU`. The implementation is pure (no IO)."
        ( ByValue
            "let { c0 = newLRU 2 \
            \    ; c1 = putLRU 1 'a' c0 \
            \    ; c2 = putLRU 2 'b' c1 \
            \    ; (g1, c3) = getLRU 1 c2 \
            \    ; c4 = putLRU 3 'c' c3 \
            \    ; (g2, c5) = getLRU 2 c4 \
            \    ; (g3, c6) = getLRU 3 c5 \
            \    ; c7 = putLRU 4 'd' c6 \
            \    ; (g4, _)  = getLRU 1 c7 } \
            \in [g1, g2, g3, g4] == [Just 'a', Nothing, Just 'c', Nothing]"
        )

tokenBucketTask :: Task
tokenBucketTask =
    Task
        "tokenBucket"
        "Implement a token-bucket rate limiter. The bucket has a maximum \
        \capacity and refills at a fixed number of tokens per time tick, never \
        \exceeding capacity; it starts FULL. Provide exactly:\n\n\
        \  newBucket :: Int -> Int -> Bucket  -- newBucket capacity refillPerTick, starts full\n\
        \  request   :: Int -> Int -> Bucket -> (Bool, Bucket)  -- request tick cost bucket\n\n\
        \For `request tick cost b`: first add (tokens earned since the previous \
        \request's tick) = (tick - lastTick) * refillPerTick, capped at capacity. \
        \If at least `cost` tokens are then available, ALLOW it (return True), \
        \deduct `cost`, and record `tick`; otherwise DENY (return False), deduct \
        \nothing, but still record `tick` and the refilled level. Requests arrive \
        \in non-decreasing tick order. Define the bucket type as `Bucket`. The \
        \implementation is pure (no IO)."
        ( ByValue
            "let { b0 = newBucket 5 1 \
            \    ; step (b, acc) (tk, c) = let (ok, b') = request tk c b in (b', acc ++ [ok]) \
            \    ; (_, allows) = foldl step (b0, []) [(0,3),(0,3),(1,1),(2,3),(2,1)] } \
            \in allows == [True, False, True, True, False]"
        )

exprEvalTask :: Task
exprEvalTask =
    Task
        "evalArith"
        "Write an arithmetic expression evaluator. Define \
        \`evalArith :: String -> Maybe Double` that parses and evaluates an \
        \expression over the binary operators + - * / with parentheses and the \
        \usual precedence: * and / bind tighter than + and -, and all four are \
        \left-associative. Numbers are non-negative integer or decimal literals; \
        \arbitrary whitespace anywhere is ignored. Return `Just` the value, or \
        \`Nothing` if the whole string is not a well-formed expression. \
        \Examples: evalArith \"2 + 3 * 4\" == Just 14.0, \
        \evalArith \"(2 + 3) * 4\" == Just 20.0, evalArith \"10 / 4 - 1\" gives \
        \1.5, evalArith \"2 +\" == Nothing. The implementation is pure (no IO)."
        ( ByValue
            "evalArith \"2 + 3 * 4\" == Just 14.0 \
            \&& evalArith \"(2 + 3) * 4\" == Just 20.0 \
            \&& (case evalArith \"10 / 4 - 1\" of { Just v -> abs (v - 1.5) < 1e-9; Nothing -> False }) \
            \&& evalArith \"2*(3+4)-5\" == Just 9.0 \
            \&& evalArith \"2 +\" == Nothing"
        )

ringBufferTask :: Task
ringBufferTask =
    Task
        "ringBuffer"
        "Implement a bounded FIFO (first-in, first-out) ring buffer over Int \
        \with a fixed capacity. Provide exactly:\n\n\
        \  newRing  :: Int -> Ring                 -- empty buffer of the given capacity\n\
        \  pushRing :: Int -> Ring -> (Bool, Ring)  -- enqueue; False (buffer unchanged) when full\n\
        \  popRing  :: Ring -> (Maybe Int, Ring)    -- dequeue oldest; Nothing when empty\n\n\
        \A push when the buffer is full is REJECTED (returns False and leaves the \
        \buffer unchanged). A pop returns the oldest element, or Nothing when \
        \empty. Define the buffer type as `Ring`. The implementation is pure \
        \(no IO)."
        ( ByValue
            "let { r0 = newRing 3 \
            \    ; (p1, r1) = pushRing 1 r0 \
            \    ; (p2, r2) = pushRing 2 r1 \
            \    ; (p3, r3) = pushRing 3 r2 \
            \    ; (p4, r4) = pushRing 4 r3 \
            \    ; (o1, r5) = popRing r4 \
            \    ; (p5, r6) = pushRing 4 r5 \
            \    ; (o2, r7) = popRing r6 \
            \    ; (o3, r8) = popRing r7 \
            \    ; (o4, r9) = popRing r8 \
            \    ; (o5, _)  = popRing r9 } \
            \in [p1,p2,p3,p4,p5] == [True,True,True,False,True] \
            \   && [o1,o2,o3,o4,o5] == [Just 1, Just 2, Just 3, Just 4, Nothing]"
        )
