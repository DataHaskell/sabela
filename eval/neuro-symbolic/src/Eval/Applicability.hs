{-# LANGUAGE OverloadedStrings #-}

{- | Lever-axis applicability (search-api.md section 13, R8.2): a task pair that
makes zero discover-class calls on BOTH arms cannot be moved by the grammar or
search lever, so its axis is reported 'PairNotApplicable' — a category, never a
measurement — instead of a byte-identical VOID. A byte-identical pair whose
discover surface ANSWERED is 'PairSaturated' (the lever fired, both arms got
the same answers — post-R5-T1 the card path is mode-invariant); one whose
calls were never answered is 'PairVoid' (lever dead). Everything else is
'PairSound'. Saturated is a decided category, never a measurement, and never
mislabelled VOID.

The classifier reads the rendered transcript body: structural discover markers
only (a @tool (discover)@ result header, or a @- \`find_function\`@ call line),
never prose — the system prompt's cheat-sheet lists @discover \<query\>@ without
either shape, so it is not miscounted.
-}
module Eval.Applicability (
    PairCategory (..),
    transcriptBody,
    voidPair,
    discoverClassCalls,
    discoverClassNames,
    surfaceAnswered,
    classifyPair,
    readVoidFlags,
    readNaFlags,
    readSaturatedFlags,
    voidNote,
    naNote,
    saturatedNote,
    excludeFlagged,
) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Read (readMaybe)

-- | The four ways a (task, seed) arm pair can score against a lever.
data PairCategory = PairSound | PairVoid | PairSaturated | PairNotApplicable
    deriving (Eq, Show)

headerOpen, headerClose :: Text
headerOpen = "<!-- episode-config"
headerClose = "-->"

-- | The transcript with its config header removed: the unit VOID compares.
transcriptBody :: Text -> Text
transcriptBody t = case T.lines t of
    (h : rest)
        | h == headerOpen ->
            T.unlines (drop 1 (dropWhile (/= headerClose) rest))
    _ -> t

-- | R8.2: two arms whose transcript bodies are byte-identical measured nothing.
voidPair :: Text -> Text -> Bool
voidPair a b = transcriptBody a == transcriptBody b

{- | Wire names of the discover-class (search/discovery) tools whose presence in
a trajectory marks it as search-USING. The merged @discover@ tool plus the
legacy discovery aliases it subsumes.
-}
discoverClassNames :: [Text]
discoverClassNames =
    [ "discover"
    , "find_function"
    , "search_capability"
    , "api_reference"
    , "describe_function"
    , "find_by_type"
    , "find_example_cell"
    ]

{- | How many discover-class markers a rendered transcript body carries: a
harness result header (@## N. tool (discover)@) or a model call line
(@- \`find_function\` …@). Prose in a fenced content block matches neither.
-}
discoverClassCalls :: Text -> Int
discoverClassCalls t =
    length (filter isCallLine ls) + length (filter isResultHeader ls)
  where
    ls = map T.strip (T.lines t)

{- | The lever-fired marker: a discover-class RESULT header in the body — the
discover surface actually answered, so the lever's mechanism demonstrably ran.
-}
surfaceAnswered :: Text -> Bool
surfaceAnswered = any (isResultHeader . T.strip) . T.lines

isCallLine :: Text -> Bool
isCallLine s = any (\n -> ("- `" <> n <> "`") `T.isPrefixOf` s) discoverClassNames

isResultHeader :: Text -> Bool
isResultHeader s =
    ("## " `T.isPrefixOf` s)
        && any (\n -> ("(" <> n <> ")") `T.isInfixOf` s) discoverClassNames

{- | Classify a (task, seed) pair from its two full arm transcripts (headers
included). Search-free on both arms wins as 'PairNotApplicable' even when the
bodies also happen to be identical; an identical search-using pair splits on
the lever-fired marker — answered surface = saturated, unanswered = VOID.
-}
classifyPair :: Text -> Text -> PairCategory
classifyPair off on
    | searchFree off && searchFree on = PairNotApplicable
    | transcriptBody off /= transcriptBody on = PairSound
    | surfaceAnswered (transcriptBody on) = PairSaturated
    | otherwise = PairVoid
  where
    searchFree = (== 0) . discoverClassCalls . transcriptBody

-- | The (task, seed) pairs flagged VOID in a transcript directory.
readVoidFlags :: FilePath -> IO [(Text, Int)]
readVoidFlags = readFlags ".VOID"

-- | The (task, seed) pairs flagged not-applicable in a transcript directory.
readNaFlags :: FilePath -> IO [(Text, Int)]
readNaFlags = readFlags ".NA"

-- | The (task, seed) pairs flagged lever-saturated in a transcript directory.
readSaturatedFlags :: FilePath -> IO [(Text, Int)]
readSaturatedFlags = readFlags ".SATURATED"

readFlags :: Text -> FilePath -> IO [(Text, Int)]
readFlags suffix dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else sort . mapMaybe parseFlag <$> listDirectory dir
  where
    parseFlag name = do
        stem <- T.stripSuffix suffix (T.pack name)
        let (t, s) = T.breakOnEnd "-s" stem
        task <- T.stripSuffix "-s" t
        seed <- readMaybe (T.unpack s)
        pure (task, seed)

-- | The report line naming byte-identical dead-lever pairs.
voidNote :: [(Text, Int)] -> Text
voidNote =
    flagNote
        "VOID pairs (byte-identical arms, discover surface never answered — \
        \lever dead; excluded from measurement)"

-- | The report line naming lever-saturated pairs (lever fired, arms identical).
saturatedNote :: [(Text, Int)] -> Text
saturatedNote =
    flagNote
        "lever-saturated pairs (lever fired — the discover surface answered \
        \identically in both arms; a decided category, excluded from lever deltas)"

-- | The report line naming search-free pairs the lever axis cannot move.
naNote :: [(Text, Int)] -> Text
naNote =
    flagNote
        "not-applicable pairs (zero discover-class calls both arms — lever axis \
        \inapplicable, excluded from deltas as a category)"

flagNote :: Text -> [(Text, Int)] -> Text
flagNote _ [] = ""
flagNote label vs =
    label
        <> ": "
        <> T.intercalate ", " [t <> " s" <> tshow s | (t, s) <- vs]
        <> "\n\n"

{- | Drop every measurement row whose (task, seed) is flagged, stripping the
seed. No VOID or not-applicable pair survives into a lever delta.
-}
excludeFlagged ::
    [(Text, Int)] -> [(Text, Int, a, b)] -> [(Text, a, b)]
excludeFlagged flagged rows =
    [(t, m, s) | (t, seed, m, s) <- rows, (t, seed) `notElem` flagged]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
