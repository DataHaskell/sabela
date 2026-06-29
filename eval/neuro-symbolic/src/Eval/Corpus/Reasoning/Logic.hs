{- | Reasoning corpus — logic / deduction puzzles.

Each puzzle is stated FULLY in the prompt and has a unique deterministic answer;
the deliverable is that answer as a top-level binding and the 'ByValue' check is
pinned to the LIVE-VALIDATED solution (uniqueness was confirmed by exhaustive
search, see the haddock above each task). These probe deduction, not library
use, so nothing renders and no package is named. See the validated values in the
haddock above each task.
-}
module Eval.Corpus.Reasoning.Logic (
    logicTasks,
) where

import Eval.Task (Grader (..), Task (..))

-- | The logic reasoning tasks, in catalogue order.
logicTasks :: [Task]
logicTasks =
    [ knaveKnightTask
    , zebraFishTask
    , waterJugTask
    , seatingOrderTask
    ]

{- | Knights (always truthful) and knaves (always lying). A says "We are both
knaves." If A were a knight the statement would be true, forcing A to be a knave
— a contradiction; so A is a knave, the statement is false, hence NOT both are
knaves, so B is the knight. Validated unique answer: "B".
-}
knaveKnightTask :: Task
knaveKnightTask =
    Task
        "knightIs"
        "On an island every inhabitant is either a knight, who always tells the \
        \truth, or a knave, who always lies. You meet two inhabitants, A and B. \
        \A says: \"We are both knaves.\" Exactly one consistent assignment of \
        \knight/knave to A and B satisfies this. Define `knightIs :: String` as \
        \the name (\"A\" or \"B\") of the one who is the KNIGHT. The reasoning is \
        \pure (no IO)."
        (ByValue "knightIs == \"B\"")

{- | A three-house Zebra-style puzzle. Houses 1..3 left to right, each with a
nationality (Brit, Swede, Dane), colour (red, green, white) and pet (dog, fish,
bird). Clues: Brit↔red; Swede↔dog; green immediately left of white; Dane in the
leftmost house; fish owner↔green house. Exhaustive search gives the unique
solution where the Dane owns the fish. Validated unique answer: "Dane".
-}
zebraFishTask :: Task
zebraFishTask =
    Task
        "fishOwner"
        "Three houses stand in a row, numbered 1, 2, 3 from left to right. Each \
        \house has a different nationality of owner (Brit, Swede, Dane), a \
        \different colour (red, green, white), and a different pet (dog, fish, \
        \bird). The clues:\n\n\
        \  1. The Brit lives in the red house.\n\
        \  2. The Swede keeps the dog.\n\
        \  3. The green house is immediately to the left of the white house.\n\
        \  4. The Dane lives in the leftmost house (house 1).\n\
        \  5. The fish owner lives in the green house.\n\n\
        \Exactly one assignment satisfies every clue. Define `fishOwner :: String` \
        \as the nationality (\"Brit\", \"Swede\", or \"Dane\") of the person who \
        \owns the FISH. The reasoning is pure (no IO)."
        (ByValue "fishOwner == \"Dane\"")

{- | Water-pouring (die-hard) puzzle: jugs of capacity 3 and 5, both empty;
operations are fill a jug, empty a jug, or pour one into the other until the
source empties or the destination fills. A breadth-first search reaches exactly
4 litres in the minimum number of operations. Validated minimum: 6.
-}
waterJugTask :: Task
waterJugTask =
    Task
        "minPours"
        "You have two unmarked jugs of capacity 3 litres and 5 litres, both \
        \initially empty. One OPERATION is any of: fill a jug to its capacity, \
        \empty a jug completely, or pour from one jug into the other until the \
        \source is empty or the destination is full. Define `minPours :: Int` as \
        \the least number of operations after which some jug contains EXACTLY 4 \
        \litres. The reasoning is pure (no IO)."
        (ByValue "minPours == 6")

{- | Seating deduction over four people A, B, C, D in a left-to-right row.
Constraints: A is left of B; C is immediately to the right of A; D sits at an
end; B is not at an end. Exhaustive search yields the unique order
["A","C","B","D"].
-}
seatingOrderTask :: Task
seatingOrderTask =
    Task
        "seating"
        "Four people — A, B, C, D — sit in a single row of four seats, numbered \
        \1 to 4 from left to right. The constraints:\n\n\
        \  1. A sits somewhere to the LEFT of B.\n\
        \  2. C sits IMMEDIATELY to the right of A (in the very next seat).\n\
        \  3. D sits at one of the two ENDS (seat 1 or seat 4).\n\
        \  4. B does NOT sit at either end.\n\n\
        \Exactly one ordering satisfies all four constraints. Define \
        \`seating :: [String]` as that left-to-right order, e.g. \
        \[\"A\",\"C\",\"B\",\"D\"] means A in seat 1, C in seat 2, and so on. The \
        \reasoning is pure (no IO)."
        (ByValue "seating == [\"A\", \"C\", \"B\", \"D\"]")
