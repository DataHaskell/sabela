{-# LANGUAGE OverloadedStrings #-}

{- | Finds the argument a type mismatch was blamed on. Wrapping that argument
in a hole asks the compiler what converts the type the caller holds into the
type the slot wants: both ends of the hole are pinned by the rejection itself.
-}
module Sabela.AI.Capabilities.Edit.HoleRewrite.Adapter (
    AdapterTarget (..),
    adapterTarget,
    asksHole,
    mismatchBlock,
    uniqueArgSpan,
) where

import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ArgRepair (ArgBlame (..), argumentBlame)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Call (argumentSpans)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Head (lineTokens)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Section (
    Reported (..),
    blockSpan,
    errorBlocks,
 )
import Sabela.AI.TypedHole (containsTypedHole)
import Sabela.Errors (ghcCodeIn)

{- | The blamed argument the harness answered, where it stands in the
candidate, the structural code GHC printed for it, the blames that follow from
it, and the blames it does not account for.
-}
data AdapterTarget = AdapterTarget
    { atBlame :: ArgBlame
    , atSpan :: (Int, Int)
    , atCode :: Maybe Int
    , atSubsumed :: [Text]
    , atUnanswered :: [Text]
    }
    deriving (Eq, Show)

-- | One blamed argument, with the structural code and span of its block.
data Blamed = Blamed
    { bmBlame :: ArgBlame
    , bmCode :: Maybe Int
    , bmAt :: Maybe Reported
    }

{- | The block to answer: one blaming an argument the candidate leaves in a
single unambiguous place, and which is no other block's knock-on. A knock-on
follows from the block it stands inside and is never answered alone.
-}
adapterTarget :: Text -> Text -> Maybe AdapterTarget
adapterTarget diagnostic src = do
    (chosen, place) <- listToMaybe roots
    let answered = abExpr (bmBlame chosen)
        rest = [b | b <- blamed, abExpr (bmBlame b) /= answered]
        follows = nub [abExpr (bmBlame b) | b <- rest, b `provenFollows` chosen]
    pure
        AdapterTarget
            { atBlame = bmBlame chosen
            , atSpan = place
            , atCode = bmCode chosen
            , atSubsumed = follows
            , atUnanswered =
                nub [e | b <- rest, let e = abExpr (bmBlame b), e `notElem` follows]
            }
  where
    blamed =
        [ Blamed blame (ghcCodeIn block) (blockSpan block)
        | block <- errorBlocks diagnostic
        , mismatchBlock block
        , Just blame <- [argumentBlame block]
        ]
    placeable =
        [ (b, place)
        | b <- blamed
        , not (asksHole (abExpr (bmBlame b)))
        , Just place <- [uniqueArgSpan (bmBlame b) src]
        ]
    roots = [c | c@(b, _) <- placeable, not (any (b `mayFollow`) blamed)]

{- | Whether one blame may stand inside another: its expression is part of the
other's text and the compiler reported it no earlier. Never answered in the
other's place, so an undecided position declines rather than descends.
-}
mayFollow :: Blamed -> Blamed -> Bool
mayFollow inner outer =
    strictlyInside (abExpr (bmBlame inner)) (abExpr (bmBlame outer))
        && notBefore (bmAt inner) (bmAt outer)
  where
    notBefore (Just i) (Just o) = rdStart i >= rdStart o
    notBefore _ _ = True

{- | Whether the compiler's own spans put one blame inside another. Only this
is published as a consequence: reading order alone relates two rejections that
merely share a name.
-}
provenFollows :: Blamed -> Blamed -> Bool
provenFollows inner outer =
    mayFollow inner outer
        && fromMaybe False (within <$> bmAt inner <*> bmAt outer)
  where
    within i o =
        rdStart o <= rdStart i && rdEnd i <= rdEnd o && rdEnd o /= rdStart o

-- | Whether one blamed expression stands strictly inside another.
strictlyInside :: Text -> Text -> Bool
strictlyInside inner outer = inner /= outer && inner `T.isInfixOf` outer

{- | Whether a diagnostic block rejected something for its type. The structural
code is reported rather than required, so a compiler that printed none is still
answered.
-}
mismatchBlock :: Text -> Bool
mismatchBlock block = "Couldn't match" `T.isInfixOf` block

{- | Whether a text already puts a hole to the compiler, annotated or bare.
Answering beside one would report the type of a slot the caller left open
rather than the conversion the rejection was about.
-}
asksHole :: Text -> Bool
asksHole expr =
    containsTypedHole expr || any ((== "_") . snd) (lineTokens expr)

{- | Where the blamed argument stands, when the candidate leaves no doubt: on
the call's own line and on the side of it that position takes its arguments
from. Ambiguity declines rather than report a type for a slot GHC never rejected.
-}
uniqueArgSpan :: ArgBlame -> Text -> Maybe (Int, Int)
uniqueArgSpan blame src =
    case argumentSpans (abFunction blame) (abIndex blame) (abExpr blame) src of
        [place] -> Just place
        _ -> Nothing
