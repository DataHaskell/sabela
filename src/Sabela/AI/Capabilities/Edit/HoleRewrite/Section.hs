{-# LANGUAGE OverloadedStrings #-}

{- | Reads the one diagnostic block GHC wrote about a named hole. Selecting the
block by the hole's name is what keeps a hole the harness did not write out of
the payload, and what stops the fit reader running on into another error's prose.
-}
module Sabela.AI.Capabilities.Edit.HoleRewrite.Section (
    Reported (..),
    blockSpan,
    errorBlocks,
    foundHole,
    holeSection,
    typeVariables,
    uninformativeResult,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower)
import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.AI.HoleFits (stripPackageQualifiers)

{- | The diagnostic split at blank lines and at each new position header, with
GHC's echoed source lines dropped: they carry @::@ and would otherwise be read
as fits.
-}
errorBlocks :: Text -> [Text]
errorBlocks =
    map (T.intercalate "\n") . filter (not . null) . go [] . T.lines
  where
    go acc [] = [reverse acc]
    go acc (l : ls)
        | T.null (T.strip l) = reverse acc : go [] ls
        | headerLine l = reverse acc : go [l] ls
        | snippetLine l = go acc ls
        | otherwise = go (l : acc) ls

{- | The span the compiler reported a block at, in the coordinates of the
source it read. Blocks of one compile are comparable, which is what tells a
knock-on inside an expression apart from a separate rejection elsewhere.
-}
data Reported = Reported
    { rdStart :: (Int, Int)
    , rdEnd :: (Int, Int)
    }
    deriving (Eq, Show)

{- | The span off a block's header, in each spelling GHC uses: a point, a range
on one line (@7:15-22@), and a range across lines (@(7,15)-(9,20)@). A point is
read as the one column it names and nothing wider.
-}
blockSpan :: Text -> Maybe Reported
blockSpan block = listToMaybe (crossLine header <> oneLine header)
  where
    header = fromMaybe "" (listToMaybe (T.lines block))

crossLine :: Text -> [Reported]
crossLine header =
    [ Reported s e
    | i <- [0 .. T.length header - 1]
    , Just (s, r1) <- [rowCol (T.drop i header)]
    , Just r2 <- [T.stripPrefix "-" r1]
    , Just (e, _) <- [rowCol r2]
    ]
  where
    rowCol t = do
        r0 <- T.stripPrefix "(" t
        let (ls, r1) = T.span isDigit r0
        r2 <- T.stripPrefix "," r1
        let (cs, r3) = T.span isDigit r2
        r4 <- T.stripPrefix ")" r3
        l <- number ls
        c <- number cs
        pure ((l, c), r4)

oneLine :: Text -> [Reported]
oneLine header =
    [ Reported (l, c) (l, endCol c rest)
    | let fields = T.splitOn ":" header
    , (a, b) <- zip fields (drop 1 fields)
    , let (digits, rest) = T.span isDigit b
    , Just l <- [number a]
    , Just c <- [number digits]
    ]
  where
    endCol c rest = case T.stripPrefix "-" rest of
        Just r -> fromMaybe c (number (T.takeWhile isDigit r))
        Nothing -> c

number :: Text -> Maybe Int
number t
    | not (T.null t) && T.all isDigit t = Just (read (T.unpack t))
    | otherwise = Nothing

-- | A new diagnostic starts at column zero and names its severity.
headerLine :: Text -> Bool
headerLine l = case T.uncons l of
    Just (c, _) ->
        c /= ' ' && any (`T.isInfixOf` l) [": error", ": warning"]
    Nothing -> False

-- | A line of the source excerpt GHC prints under a diagnostic.
snippetLine :: Text -> Bool
snippetLine l = "|" `T.isPrefixOf` T.stripStart (T.dropWhile isDigit stripped)
  where
    stripped = T.stripStart l

{- | The name and printed type GHC gave for the block's hole, joined across the
continuation lines and stopped at its own prose. The type is shortened exactly
as a published fit's is: one payload never presents a name two ways.
-}
foundHole :: Text -> Maybe (Text, Text)
foundHole block = do
    (indent, rest, after) <- marker (T.lines block)
    let joined = T.unwords (T.words (T.unwords (rest : takeWhile (cont indent) after)))
    (nm, tyPart) <- splitOnce "::" joined
    let name = T.strip nm
        ty = sanitizeTypeText (stripPackageQualifiers (T.strip tyPart))
    if T.null name || T.null ty then Nothing else Just (name, ty)
  where
    cont indent l = indentOf l > indent && not (prose (T.strip l))

marker :: [Text] -> Maybe (Int, Text, [Text])
marker [] = Nothing
marker (l : ls) = case T.breakOn found l of
    (_, r)
        | not (T.null r) -> Just (indentOf l, T.drop (T.length found) r, ls)
    _ -> marker ls
  where
    found = "Found hole:"

indentOf :: Text -> Int
indentOf = T.length . T.takeWhile (== ' ')

{- | Prose GHC writes inside the same bullet as the hole's type. A labelled
line (@Where:@) ends the type as surely as a new bullet does.
-}
prose :: Text -> Bool
prose s = any (`T.isPrefixOf` s) starts || labelled s
  where
    starts =
        [ "\8226"
        , "Or perhaps"
        , "Perhaps"
        , "Suggested"
        , "Valid "
        , "Relevant "
        , "Constraints "
        , "In "
        , "with "
        , "(imported"
        , "(bound"
        , "(defined"
        , "(and"
        ]
    labelled t = case T.span isAlpha t of
        (w, r) -> not (T.null w) && ":" `T.isPrefixOf` r && not ("::" `T.isPrefixOf` r)

splitOnce :: Text -> Text -> Maybe (Text, Text)
splitOnce needle t = case T.breakOn needle t of
    (_, r) | T.null r -> Nothing
    (pre, r) -> Just (pre, T.drop (T.length needle) r)

-- | The block GHC wrote about the hole of this name, if it wrote one.
holeSection :: Text -> Text -> Maybe Text
holeSection name diagnostic =
    listToMaybe
        [b | b <- errorBlocks diagnostic, fmap fst (foundHole b) == Just name]

{- | The type variables a printed type mentions. A fact about the text, not a
claim about what the compiler resolved. A package qualifier is lowercase and
stands where a variable could, so it comes off before the text is read.
-}
typeVariables :: Text -> [Text]
typeVariables ty = nub [w | w <- identTokens (stripPackageQualifiers ty), isVar w]
  where
    isVar w = maybe False (isLower . fst) (T.uncons w) && w /= "forall"

{- | The result a printed type ends in, when that result carries no
information. GHC prints such a result both where the slot really wants it and
where its own default rules picked it, so it confirms nothing either way.
-}
uninformativeResult :: Text -> Maybe Text
uninformativeResult ty
    | unqualify result `elem` ["()", "Any"] = Just result
    | otherwise = Nothing
  where
    result = T.strip (lastOf (splitTopArrow (T.strip ty)))
    unqualify = T.takeWhileEnd (/= '.')
    lastOf xs = if null xs then "" else last xs

-- | A type split at the arrows outside any bracket.
splitTopArrow :: Text -> [Text]
splitTopArrow t = case breakTopArrow (0 :: Int) 0 of
    Nothing -> [t]
    Just i -> T.take i t : splitTopArrow (T.drop (i + 2) t)
  where
    breakTopArrow d i
        | i >= T.length t = Nothing
        | d == 0 && "->" `T.isPrefixOf` T.drop i t = Just i
        | otherwise = breakTopArrow (depth d (T.index t i)) (i + 1)
    depth d c
        | c `elem` ("([" :: String) = d + 1
        | c `elem` (")]" :: String) = d - 1
        | otherwise = d

identTokens :: Text -> [Text]
identTokens =
    filter (maybe False (isIdentStart . fst) . T.uncons) . T.groupBy same
  where
    same a b = isIdent a == isIdent b

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c `elem` ("_'" :: String)
