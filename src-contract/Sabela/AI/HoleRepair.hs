{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.HoleRepair (
    afterInfixCI,
    goalFromError,
    goalSpans,
    holeSpans,
    holeQueryFor,
    arityFromError,
    holeTypeFromDiagnostic,
    droppableAnnotation,
    dropAnnotation,
    holeFitNames,
    holeFitRewrites,
    suggestedNames,
    orderBySimilarity,
    editDistance,
    substituteName,
    substituteNameAt,
    substituteNameAtAll,
) where

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isDigit)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.GoalText (holeQueryFor)
import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..), bareCellError)

holeTypeFromDiagnostic :: Text -> Maybe Text
holeTypeFromDiagnostic t = do
    rest <- afterInfix "Found hole: _ :: " t
    let ty = joinTypeLines rest
    if T.null ty then Nothing else Just ty

droppableAnnotation :: Text -> Maybe Text
droppableAnnotation err = do
    rest <- afterInfix "In an expression type signature: " err
    let ty = T.strip (T.takeWhile (/= '\n') rest)
    if T.null ty then Nothing else Just ty

dropAnnotation :: Text -> Text -> Text
dropAnnotation ty src = T.intercalate "\n" (go (T.lines src))
  where
    tyToks = T.words ty
    go [] = []
    go (l : ls)
        | Just l' <- scan "" l = l' : ls
        | otherwise = l : go ls
    scan acc rest = case T.breakOn "::" rest of
        (_, r) | T.null r -> Nothing
        (pre, r) ->
            let acc' = acc <> pre
             in case matchTail (T.drop 2 r) of
                    Just tl -> Just (T.stripEnd acc' <> tl)
                    Nothing -> scan (acc' <> "::") (T.drop 2 r)
    matchTail afterColon = matchToks tyToks (T.stripStart afterColon)
    matchToks [] rest
        | boundary rest = Just rest
        | otherwise = Nothing
    matchToks (tok : toks) rest = case T.stripPrefix tok rest of
        Just r -> matchToks toks (if null toks then r else T.stripStart r)
        Nothing -> Nothing
    boundary rest = case T.uncons rest of
        Nothing -> True
        Just (c, _) -> not (isIdent c)
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

goalFromError :: Text -> Maybe (Text, Text)
goalFromError err = do
    rest <- afterInfixCI "not in scope:" err
    let (lhs, rhs) = T.breakOn "::" rest
    if T.null rhs || "•" `T.isInfixOf` lhs
        then Nothing
        else do
            name <- lastWord (T.strip lhs)
            let ty = joinTypeLines (T.drop 2 rhs)
            if T.null ty then Nothing else Just (name, ty)
  where
    lastWord t = case reverse (T.words t) of
        (w : _) | not (T.all (\c -> isDigit c || c == '.') w) -> Just w
        _ -> Nothing

goalSpans :: Either Text ExecutionResult -> [(Text, Text, Maybe (Int, Int))]
goalSpans (Left e) = [(w, t, Nothing) | Just (w, t) <- [goalFromError e]]
goalSpans (Right er) =
    [ (w, t, (,) <$> ceLine ce <*> ceCol ce)
    | ce <- resultDiags er
    , Just (w, t) <- [goalFromError (ceMessage ce)]
    ]

holeSpans :: Either Text ExecutionResult -> [(Text, Text, Maybe (Int, Int))]
holeSpans (Left e) =
    [("_", t, Nothing) | Just t <- [holeTypeFromDiagnostic e]]
holeSpans (Right er) =
    [ ("_", t, (,) <$> ceLine ce <*> ceCol ce)
    | ce <- resultDiags er
    , Just t <- [holeTypeFromDiagnostic (ceMessage ce)]
    ]

resultDiags :: ExecutionResult -> [CellError]
resultDiags er =
    maybe [] (\m -> [bareCellError Nothing Nothing m]) (erError er)
        ++ erErrors er

joinTypeLines :: Text -> Text
joinTypeLines afterColon =
    T.unwords (T.words (T.unlines (firstLine : conts)))
  where
    ls = T.lines afterColon
    firstLine = case ls of
        (x : _) -> x
        [] -> ""
    conts = takeWhile isCont (drop 1 ls)
    isCont l =
        not (T.null s)
            && T.length (T.takeWhile (== ' ') l) >= 2
            && not ("::" `T.isInfixOf` l)
            && not (hintLine s)
      where
        s = T.strip l
    hintLine s =
        any
            (`T.isPrefixOf` s)
            ["Perhaps", "Suggested", "Valid hole fits", "•", "In ", "(imported"]

arityFromError :: Text -> Maybe Text
arityFromError err = do
    rest <- afterInfix "Couldn't match expected type:" err
    let ty = T.strip (T.takeWhile (/= '\n') rest)
    if T.null ty || not ("->" `T.isInfixOf` ty) then Nothing else Just ty

holeFitNames :: Text -> [Text]
holeFitNames blob = [hfWrite f | f <- parseHoleFits blob, not (hfRefined f)]

holeFitRewrites :: Text -> Text -> Text -> [Text]
holeFitRewrites wrong blob src =
    nub [s | n <- holeFitNames blob, let s = substituteName wrong n src, s /= src]

substituteName :: Text -> Text -> Text -> Text
substituteName wrong fit src
    | wrong == fit || T.null wrong = src
    | otherwise = T.concat (map sub (T.groupBy sameClass src))
  where
    sub tok = if tok == wrong then fit else tok
    sameClass a b = isIdent a == isIdent b
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

substituteNameAt :: (Int, Int) -> Text -> Text -> Text -> Maybe Text
substituteNameAt (line, col) wrong fit src
    | T.null wrong || line < 1 || col < 1 = Nothing
    | otherwise = case splitAt (line - 1) (T.splitOn "\n" src) of
        (before, target : after) -> do
            newLine <- replaceAt target
            Just (T.intercalate "\n" (before ++ newLine : after))
        _ -> Nothing
  where
    replaceAt l =
        let (pre, rest) = T.splitAt (col - 1) l
            (tok, post) = T.span isIdent rest
         in if tok == wrong then Just (pre <> fit <> post) else Nothing
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

suggestedNames :: Text -> [Text]
suggestedNames err = maybe [] pickStyle (afterInfix "Perhaps use" err)
  where
    pickStyle s = case quotedNames '\8216' '\8217' s of
        [] -> quotedNames '`' '\'' s
        ns -> ns
    quotedNames open close s = case T.breakOn (T.singleton open) s of
        (_, r)
            | T.null r -> []
            | otherwise ->
                let (nm, r2) = T.breakOn (T.singleton close) (T.drop 1 r)
                 in if T.null r2 then [] else nm : quotedNames open close (T.drop 1 r2)

orderBySimilarity :: Text -> [Text] -> [Text]
orderBySimilarity wrong = sortOn (editDistance wrong)

editDistance :: Text -> Text -> Int
editDistance a b = go (T.unpack a) (T.unpack b)
  where
    go xs ys = last (foldl transform [0 .. length xs] ys)
      where
        transform ns@(n : ns') y = scanl calc (n + 1) (zip3 xs ns ns')
          where
            calc z (x, diag, nxt) =
                minimum [z + 1, nxt + 1, diag + if x == y then 0 else 1]
        transform [] _ = []

afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest) | not (T.null rest) -> Just (T.drop (T.length needle) rest)
    _ -> Nothing

afterInfixCI :: Text -> Text -> Maybe Text
afterInfixCI needle t = case T.breakOn (T.toLower needle) (T.toLower t) of
    (pre, rest)
        | not (T.null rest) ->
            Just (T.drop (T.length pre + T.length needle) t)
    _ -> Nothing

substituteNameAtAll :: [(Int, Int)] -> Text -> Text -> Text -> Maybe Text
substituteNameAtAll [] _ _ _ = Nothing
substituteNameAtAll spans wrong fit src =
    foldM (\s sp -> substituteNameAt sp wrong fit s) src ordered
  where
    ordered = sortOn Down (nub spans)
