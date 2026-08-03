{-# LANGUAGE OverloadedStrings #-}

{- | Staged minimisation: cut a rejected candidate back to the largest prefix
that compiles and name the component whose addition does not. Same algebra as
the repair frontier, applied to the candidate's own parts instead of to fixes.
-}
module Sabela.AI.Capabilities.Edit.GateFrontier.Localise (
    Component (..),
    Split (..),
    splitCandidate,
    prefixSource,
    Localisation (..),
    localiseCandidate,
    localisationPairs,
) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Parser (Line, parseScriptNumbered)
import ScriptHs.Render (
    Kind (..),
    Piece (..),
    bindStatementBody,
    lineText,
    mergePieces,
    toPieces,
 )

import Sabela.AI.Capabilities.Edit.GateFrontier (disposableDiagnostic)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
 )

-- | One independently compilable part of a candidate, under the name it binds.
data Component = Component
    { compName :: Text
    , compSource :: Text
    }
    deriving (Eq, Show)

{- | A candidate as its scaffolding plus its components. The scaffolding —
cabal directives, pragmas and imports — belongs to every prefix, so a prefix is
still a candidate the same planner accepts.
-}
data Split = Split
    { splitPreamble :: [Text]
    , splitComponents :: [Component]
    }
    deriving (Eq, Show)

{- | Split a candidate into the parts a compiler can be asked about separately.
Directive lines are recovered by line number, because the script parser removes
them from the code stream while preserving the numbering of what survives.
-}
splitCandidate :: Text -> Split
splitCandidate src = Split (directives <> scaffolding) (attachComments units)
  where
    (_, numbered) = parseScriptNumbered src
    codeNums = S.fromList (map fst numbered)
    directives =
        [ l
        | (i, l) <- zip [1 ..] (T.lines src)
        , not (i `S.member` codeNums)
        , not (T.null (T.strip l))
        ]
    pieces = regroupByBinder (mergePieces (toPieces (map snd numbered)))
    scaffolding = concatMap scaffoldLines pieces
    units = concatMap unitOf pieces

scaffoldLines :: Piece -> [Text]
scaffoldLines (PPragma t) = [t]
scaffoldLines (PImport t) = [t]
scaffoldLines _ = []

{- | A comment run with nothing after it is scaffolding; one that leads a
binding travels with it, so a prefix never separates a haddock from its subject.
-}
attachComments :: [(Bool, Text, Text)] -> [Component]
attachComments = go []
  where
    go pending [] = [Component "" (T.intercalate "\n" (reverse pending)) | not (null pending)]
    go pending ((isComment, name, body) : rest)
        | isComment = go (body : pending) rest
        | otherwise =
            Component name (T.intercalate "\n" (reverse (body : pending)))
                : go [] rest

unitOf :: Piece -> [(Bool, Text, Text)]
unitOf (PUnit KComment ls) = [(True, "", bodyOf ls)]
unitOf (PUnit _ ls) = [(False, unitName ls, bodyOf ls)]
unitOf (PGhciCommand t) = [(False, t, t)]
unitOf _ = []

bodyOf :: [Line] -> Text
bodyOf = T.intercalate "\n" . map lineText

{- | What a unit is called, for a report the reader can act on: the name it
binds when it binds one, else its own leading text.
-}
unitName :: [Line] -> Text
unitName [] = ""
unitName (l : _)
    | not (T.null bound) = bound
    | not (T.null binder) = binder
    | otherwise = T.take 40 (T.strip lead)
  where
    lead = lineText l
    bound =
        maybe "" (const (T.strip (T.takeWhile (/= '<') lead))) (bindStatementBody lead)
    binder = binderOf lead

binderOf :: Text -> Text
binderOf t
    | isIndented t = ""
    | tok `elem` declKeywords = ""
    | not (T.null tok), isAlpha (T.head tok) || T.head tok == '_' = tok
    | otherwise = ""
  where
    tok = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') (T.stripStart t)
    isIndented x = maybe True (\(c, _) -> c == ' ' || c == '\t') (T.uncons x)

declKeywords :: [Text]
declKeywords =
    ["data", "newtype", "type", "class", "instance", "foreign", "pattern"]

regroupByBinder :: [Piece] -> [Piece]
regroupByBinder = concatMap split
  where
    split (PUnit KDeclaration ls) = [PUnit KDeclaration g | g <- groupByBinder ls]
    split p = [p]

groupByBinder :: [Line] -> [[Line]]
groupByBinder = foldr step []
  where
    step l [] = [[l]]
    step l (g : gs)
        | any (continues l) (take 1 g) = (l : g) : gs
        | otherwise = [l] : g : gs
    continues l next =
        let name = binderOf (lineText l)
         in indented (lineText next)
                || (not (T.null name) && name == binderOf (lineText next))
    indented x = maybe True (\(c, _) -> c == ' ' || c == '\t') (T.uncons x)

-- | The candidate cut back to its first @n@ components, scaffolding kept.
prefixSource :: Split -> Int -> Text
prefixSource split n =
    T.intercalate "\n\n" (kept (splitPreamble split) <> map compSource taken)
  where
    taken = take n (splitComponents split)
    kept [] = []
    kept ls = [T.intercalate "\n" ls]

{- | Where a rejected candidate stops compiling: the largest prefix that was
measured green, and the component whose addition was measured red.
-}
data Localisation = Localisation
    { locProven :: [Text]
    , locProvenSource :: Text
    , locFirstFailing :: Text
    , locFailingDiagnostic :: Text
    , locProbes :: Int
    }
    deriving (Eq, Show)

{- | Bisect a rejected candidate over its own components: the answer is the
longest prefix that compiled and the component whose addition did not. Costs
@log2 n@ probes, and gives up rather than guessing when a probe neither
compiled nor produced a compile error.
-}
localiseCandidate ::
    (Monad m) =>
    Int ->
    (Text -> m DisposableResult) ->
    Text ->
    DisposableResult ->
    m (Maybe Localisation)
localiseCandidate budget probe src result
    | disposableVerdict result /= DisposableCompileError = pure Nothing
    | n < 2 = pure Nothing
    | otherwise = search 0 n 0
  where
    split = splitCandidate src
    names = map compName (splitComponents split)
    n = length names
    search lo hi probes
        | hi - lo <= 1 = pure (Just (localised lo probes))
        | probes >= budget = pure Nothing
        | otherwise = do
            let mid = (lo + hi) `div` 2
            r <- probe (prefixSource split mid)
            case disposableVerdict r of
                DisposableOk -> search mid hi (probes + 1)
                DisposableCompileError -> search lo mid (probes + 1)
                _ -> pure Nothing
    localised lo probes =
        Localisation
            { locProven = take lo names
            , locProvenSource = if lo == 0 then "" else prefixSource split lo
            , locFirstFailing = names !! lo
            , locFailingDiagnostic = disposableDiagnostic result
            , locProbes = probes
            }

{- | The localisation a rejection carries. Both halves are measurements: the
proven prefix compiled in a disposable session, and the named component did not
compile on top of it.
-}
localisationPairs :: Localisation -> [Pair]
localisationPairs loc =
    [ "localisation"
        .= object
            [ "provenComponents" .= locProven loc
            , "provenSource" .= locProvenSource loc
            , "firstFailingComponent" .= locFirstFailing loc
            , "probes" .= locProbes loc
            , "note" .= note
            ]
    ]
  where
    note :: Text
    note
        | null (locProven loc) =
            "Nothing was committed. `"
                <> locFirstFailing loc
                <> "` is the first component of the candidate and it does not \
                   \compile on its own."
        | otherwise =
            "Nothing was committed. The components in `provenComponents` \
            \compiled together as `provenSource`; adding `"
                <> locFirstFailing loc
                <> "` on top of them did not."
