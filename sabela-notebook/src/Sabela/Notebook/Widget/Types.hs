{- | What a widget program is made of.

A widget is a pure function from what the reader has done to what they see.
Each control instruction hands back the 'Event' of its own occurrences, so
the FRP vocabulary in "Sabela.Notebook.Event" (@accumB@, @stepper@,
@snapshot@, @filterE@) is the whole of the state vocabulary: nothing is
stored but the log of interactions.
-}
module Sabela.Notebook.Widget.Types (
    Ui,
    UiF (..),
    Layout (..),
    Control (..),
    ControlKind (..),
    Slot,
    Log,
    decodeLog,
    encodeLog,
    logTime,
) where

import Sabela.Notebook.Behavior (Time)
import Sabela.Notebook.Event (Event)
import Sabela.Notebook.Picture (Canvas, Picture)
import Sabela.Notebook.Widget.Free (Free)

-- | Where a control's occurrences are kept, one name per control.
type Slot = String

-- | What the reader did to one control, in the order they did it.
type Log = [(Time, String)]

{- | A widget program. Write one with @do@ notation and the constructors in
"Sabela.Notebook.Widget".
-}
type Ui = Free UiF

data Layout = Across | Down
    deriving (Eq, Show)

{- | The controls a widget can offer. Each carries what it needs to draw itself
before anyone has touched it.
-}
data ControlKind
    = Press
    | Typing String
    | Sliding Double Double Double
    | Switching Bool
    | Choosing [String] String
    deriving (Eq, Show)

data Control = Control
    { controlKey :: String
    , controlLabel :: String
    , controlKind :: ControlKind
    }
    deriving (Eq, Show)

-- | One instruction of a widget program.
data UiF next
    = Say String next
    | Draw Canvas Picture next
    | Ask Control (Event String -> next)
    | Open Layout next
    | Close next
    | Now (Time -> next)

instance Functor UiF where
    fmap f (Say s next) = Say s (f next)
    fmap f (Draw c p next) = Draw c p (f next)
    fmap f (Ask spec k) = Ask spec (f . k)
    fmap f (Open l next) = Open l (f next)
    fmap f (Close next) = Close (f next)
    fmap f (Now k) = Now (f . k)

{- | Reads a control's log back off the wire. Anything unreadable is treated as
no interactions at all, so a mangled slot degrades to a fresh control.
-}
decodeLog :: String -> Log
decodeLog s = case reads s of
    [(occurrences, rest)] | all (`elem` " \t\n") rest -> occurrences
    _ -> []

-- | The form the browser writes and 'decodeLog' reads.
encodeLog :: Log -> String
encodeLog = show

{- | The latest moment in a set of logs, which is what \"now\" means to a
widget: nothing has happened since.
-}
logTime :: [Log] -> Time
logTime logs = maximum (0 : map fst (concat logs))
