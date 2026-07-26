module Eval.Corpus.Reasoning.Logic (
    logicTasks,
) where

import Eval.Task (Grader (..), Task (..))

logicTasks :: [Task]
logicTasks =
    [ knaveKnightTask
    , zebraFishTask
    , waterJugTask
    , seatingOrderTask
    ]

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
