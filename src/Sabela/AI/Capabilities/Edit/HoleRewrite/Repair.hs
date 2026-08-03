{-# LANGUAGE OverloadedStrings #-}

{- | Everything a failing compile earns the caller, for whichever tool put the
code to the compiler. The write gate and the trial share this seam, so neither
can carry a repair the other does not, nor read different words to build it.
-}
module Sabela.AI.Capabilities.Edit.HoleRewrite.Repair (
    diagnosticRepairPairs,
    dropSelfKnockOns,
    holeAnswerPairs,
    printedThroughPairs,
    repairDiagnostic,
    repairPairs,
) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.HoleNudge (holeNudgePairs)
import Sabela.AI.Capabilities.Edit.HoleRewrite (
    holeFitCap,
    holeRewritePairs,
    runHoleRewrite,
 )
import Sabela.AI.Capabilities.Edit.HoleRewrite.Adapter (asksHole)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Section (errorBlocks)
import Sabela.AI.FitRule (holeFitsJson)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.Hints (expectedTypeOf)
import Sabela.AI.HoleFits (qualifiedNames)
import Sabela.AI.HoleProbe (holeProbeJson)
import Sabela.Errors (scrubHarnessFrames)
import Sabela.Parse (cellNames)

{- | Every repair a rejection carries: what the diagnostic alone says, what one
rewritten compile adds, and which modules the compiler reached the published
names through. The compiler's own words are recorded as they arrive, so the
last is said of names this payload really prints.
-}
repairPairs :: (Text -> IO Text) -> Text -> Text -> IO [Pair]
repairPairs compile raw src = do
    saidRef <- newIORef [diagnostic]
    let recording s = do
            blob <- compile s
            modifyIORef' saidRef (blob :)
            pure blob
    holePairs <- holeAnswerPairs recording diagnostic src
    said <- readIORef saidRef
    let published = diagnosticRepairPairs diagnostic <> holePairs
    pure (published <> printedThroughPairs said published)
  where
    diagnostic = repairDiagnostic src raw

{- | The text every repair is read from, on either surface: the harness's own
frames removed, and the knock-ons about names this very candidate defines
dropped. One normaliser, so the trial and the write gate read the same words.
-}
repairDiagnostic :: Text -> Text -> Text
repairDiagnostic src = dropSelfKnockOns src . scrubHarnessFrames

{- | The diagnostic without the chunks blaming a name the candidate itself
defines: the compiler reports one for every later use of a definition it
already rejected, and none of them is a rejection of its own.
-}
dropSelfKnockOns :: Text -> Text -> Text
dropSelfKnockOns src raw
    | null kept = raw
    | otherwise = T.intercalate "\n\n" kept
  where
    defined = fst (cellNames src)
    kept = filter (not . phantom) (T.splitOn "\n\n" raw)
    phantom chunk = maybe False (`Set.member` defined) (scopeSubject chunk)

{- | What a rejection carries without asking the compiler anything more: the
fits it already printed, the hole it already answered, and the type it wanted.
The fits are read off the blocks, so GHC's echoed source cannot run into one.
-}
diagnosticRepairPairs :: Text -> [Pair]
diagnosticRepairPairs diagnostic =
    [ "holeFits" .= fits
    | fits <- [holeFitsJson holeFitCap (T.unlines (errorBlocks diagnostic))]
    , not (null fits)
    ]
        <> ["holeProbe" .= v | Just v <- [holeProbeJson diagnostic]]
        <> ["expectedType" .= g | Just g <- [expectedTypeOf diagnostic]]

{- | The rejection's hole answer over any probe, so its cost can be counted.
The standing nudge answers when no hole can be placed, unless the candidate
asks one of its own: its unnamed hole cannot then be told from that.
-}
holeAnswerPairs :: (Text -> IO Text) -> Text -> Text -> IO [Pair]
holeAnswerPairs probe diagnostic src = do
    rewrite <- runHoleRewrite probe diagnostic src
    case rewrite of
        Just hr -> pure (holeRewritePairs hr)
        Nothing
            | asksHole src -> pure []
            | otherwise -> holeNudgePairs probe diagnostic src

{- | The modules the compiler reached a published name through. A module is
named only where this payload prints a plain name that the compiler wrote as
that module's and never wrote plain itself: anything less cannot tell the
published occurrence from another name spelt the same way.
-}
printedThroughPairs :: [Text] -> [Pair] -> [Pair]
printedThroughPairs said published =
    [ "printedThrough" .= object ["modules" .= named, "note" .= printedThroughNote]
    | not (null named)
    ]
  where
    shown = T.pack (show (object published))
    named =
        sort
            ( nub
                [ modulePath
                | (modulePath, entity) <- concatMap qualifiedNames said
                , tokenIn entity shown
                , not (tokenIn modulePath shown)
                , not (any (tokenIn entity) said)
                ]
            )

{- | Whether a name stands in a text as a name of its own rather than as part
of a longer one. A module path is part of the name it qualifies, so a payload
printing the qualified spelling hid nothing and is not reported.
-}
tokenIn :: Text -> Text -> Bool
tokenIn name t =
    not (T.null name)
        && or
            [ ends (T.take i t) && starts (T.drop (i + T.length name) t)
            | i <- [0 .. T.length t - T.length name]
            , name `T.isPrefixOf` T.drop i t
            ]
  where
    ends before = maybe True (not . partChar . snd) (T.unsnoc before)
    starts after = maybe True (not . partChar . fst) (T.uncons after)
    partChar c = isAlphaNum c || c `elem` ("_.'" :: String)

printedThroughNote :: Text
printedThroughNote =
    "these modules carry the names above; a name is writable only where its \
    \module is in scope"
