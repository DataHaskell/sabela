{- | Running a widget program.

One traversal serves both callers: 'renderUi' builds the HTML the reader
sees, and 'valueOf' throws that away and keeps the answer, which is what
makes widget logic testable against a made-up history with no browser in
sight.
-}
module Sabela.Notebook.Widget.Interpret (
    Store,
    renderUi,
    valueOf,
    describeUi,
    slotsOf,
    escapeHtml,
) where

import Data.List (isPrefixOf)
import Sabela.Notebook.Behavior (Time)
import Sabela.Notebook.Event (Event, eventFromList)
import Sabela.Notebook.Markup (unSvg)
import Sabela.Notebook.Picture (renderSvg)
import Sabela.Notebook.Widget.Free (Free (..))
import Sabela.Notebook.Widget.Types (
    Control (..),
    ControlKind (..),
    Layout (..),
    Log,
    Slot,
    Ui,
    UiF (..),
    decodeLog,
    encodeLog,
    logTime,
 )

-- | What the browser has stored for this cell: slot name to encoded log.
type Store = [(Slot, String)]

-- | Everything the traversal needs that does not change as it goes.
data Ctx = Ctx
    { ctxWidget :: String
    , ctxStore :: Store
    , ctxNow :: Time
    }

{- | Slot names already handed out, so that two controls sharing a label still
get a slot each.
-}
type Used = [(String, Int)]

-- | The HTML for a widget and the value its program computed.
renderUi :: String -> Store -> Ui a -> (String, a)
renderUi widget store program =
    let ctx = Ctx{ctxWidget = widget, ctxStore = store, ctxNow = storeTime widget store}
        (html, value, _) = go ctx [] program
     in (concat html, value)

{- | The value alone. Feed it a store you wrote by hand to test a widget's
behaviour without rendering anything.
-}
valueOf :: String -> Store -> Ui a -> a
valueOf widget store = snd . renderUi widget store

{- | Every slot a program asks for, in order. Useful for checking that a
control keeps its identity when the program around it changes.
-}
slotsOf :: String -> Ui a -> [Slot]
slotsOf widget program = [s | Just s <- map slotOf (describeParts widget [] program)]
  where
    slotOf (DescribeControl s _) = Just s
    slotOf _ = Nothing

-- | A structural dump, one line per instruction, for golden tests.
describeUi :: String -> Ui a -> [String]
describeUi widget program = map render (describeParts widget [] program)
  where
    render (DescribeControl s kind) = "control " ++ s ++ " " ++ kind
    render (DescribeOther s) = s

data Part = DescribeControl Slot String | DescribeOther String

-- | The latest moment recorded anywhere in this widget.
storeTime :: String -> Store -> Time
storeTime widget store =
    logTime [decodeLog v | (k, v) <- store, (widget ++ ":") `isPrefixOf` k]

go :: Ctx -> Used -> Ui a -> ([String], a, Used)
go _ used (Pure a) = ([], a, used)
go ctx used (Free instruction) = case instruction of
    Say s next ->
        let (rest, a, used') = go ctx used next
         in (("<div class='sbw-say'>" ++ escapeHtml s ++ "</div>") : rest, a, used')
    Draw canvas picture next ->
        let (rest, a, used') = go ctx used next
         in (unSvg (renderSvg canvas picture) : rest, a, used')
    Open layout next ->
        let (rest, a, used') = go ctx used next
            klass = case layout of
                Across -> "sbw-across"
                Down -> "sbw-down"
         in (("<div class='" ++ klass ++ "'>") : rest, a, used')
    Close next ->
        let (rest, a, used') = go ctx used next
         in ("</div>" : rest, a, used')
    Now k -> go ctx used (k (ctxNow ctx))
    Ask spec k ->
        let (slot, used') = nameSlot (ctxWidget ctx) spec used
            occurrences = decodeLog (maybe "" id (lookup slot (ctxStore ctx)))
            (rest, a, used'') = go ctx used' (k (eventOf occurrences))
         in (controlHtml slot spec occurrences : rest, a, used'')

eventOf :: Log -> Event String
eventOf = eventFromList

-- | @widget:key@, with a counter appended when a key repeats.
nameSlot :: String -> Control -> Used -> (Slot, Used)
nameSlot widget spec used = (slot, bump)
  where
    key = controlKey spec
    seen = maybe 0 id (lookup key used)
    slot = widget ++ ":" ++ key ++ (if seen == 0 then "" else "#" ++ show (seen + 1))
    bump = (key, seen + 1) : filter ((/= key) . fst) used

-- | What the reader last left in a control, or what it started with.
currentValue :: ControlKind -> Log -> String
currentValue kind occurrences = case reverse occurrences of
    ((_, v) : _) -> v
    [] -> case kind of
        Press -> ""
        Typing initial -> initial
        Sliding _ _ initial -> show initial
        Switching initial -> show initial
        Choosing _ initial -> initial

controlHtml :: Slot -> Control -> Log -> String
controlHtml slot spec occurrences = case controlKind spec of
    Press ->
        "<button class='sbw-press' " ++ dataAttrs "press" ++ ">" ++ label ++ "</button>"
    Typing _ ->
        wrap ("<input type='text' " ++ dataAttrs "text" ++ value ++ ">")
    Sliding lo hi _ ->
        wrap
            ( "<input type='range' min='"
                ++ show lo
                ++ "' max='"
                ++ show hi
                ++ "' step='any' "
                ++ dataAttrs "number"
                ++ value
                ++ "><span class='sbw-read'>"
                ++ escapeHtml current
                ++ "</span>"
            )
    Switching _ ->
        wrap
            ( "<input type='checkbox' "
                ++ dataAttrs "switch"
                ++ (if current == "True" then " checked" else "")
                ++ ">"
            )
    Choosing options _ ->
        wrap
            ( "<select "
                ++ dataAttrs "choice"
                ++ ">"
                ++ concatMap option options
                ++ "</select>"
            )
  where
    current = currentValue (controlKind spec) occurrences
    label = escapeHtml (controlLabel spec)
    value = " value='" ++ escapeHtml current ++ "'"
    dataAttrs kind =
        "data-slot='"
            ++ escapeHtml slot
            ++ "' data-kind='"
            ++ kind
            ++ "' data-log='"
            ++ escapeHtml (encodeLog occurrences)
            ++ "'"
    wrap inner =
        "<label class='sbw-field'><span>" ++ label ++ "</span>" ++ inner ++ "</label>"
    option o =
        "<option"
            ++ (if o == current then " selected" else "")
            ++ ">"
            ++ escapeHtml o
            ++ "</option>"

describeParts :: String -> Used -> Ui a -> [Part]
describeParts _ _ (Pure _) = []
describeParts widget used (Free instruction) = case instruction of
    Say s next -> DescribeOther ("say " ++ show s) : describeParts widget used next
    Draw _ _ next -> DescribeOther "draw" : describeParts widget used next
    Now k -> describeParts widget used (k 0)
    Open layout next ->
        DescribeOther ("open " ++ show layout) : describeParts widget used next
    Close next -> DescribeOther "close" : describeParts widget used next
    Ask spec k ->
        let (slot, used') = nameSlot widget spec used
         in DescribeControl slot (kindName (controlKind spec))
                : describeParts widget used' (k (eventFromList []))

kindName :: ControlKind -> String
kindName Press = "press"
kindName (Typing _) = "text"
kindName (Sliding _ _ _) = "number"
kindName (Switching _) = "switch"
kindName (Choosing _ _) = "choice"

-- | Makes a string safe to put in element text or a single-quoted attribute.
escapeHtml :: String -> String
escapeHtml = concatMap one
  where
    one '<' = "&lt;"
    one '>' = "&gt;"
    one '&' = "&amp;"
    one '\'' = "&#39;"
    one '"' = "&quot;"
    one c = [c]
