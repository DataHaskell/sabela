{- | Query normalisation for discover (docs/discover/search-api.md section 4):
strip decoration to the bare name (R2.1), resolve qualification through the
notebook's own aliases (R1.6, R2.2), classify the query shape, and build the
notebook environment from a full cell listing.
-}
module Siza.Agent.Discover.Interpret (
    interpret,
    constructGoal,
    stripDecoration,
    stripVersion,
    envFromCells,
    parseCells,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlpha, isDigit, isUpper)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (nub, nubBy)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (
    Interpreted (..),
    NotebookEnv (..),
    seededBuiltins,
 )

-- | Normalise a query against the notebook environment.
interpret :: NotebookEnv -> Text -> Interpreted
interpret env raw
    | Just ty <- constructGoal raw =
        Interpreted raw ty Nothing "construct" "a value of this type" []
    | typeShaped =
        Interpreted raw stripped Nothing "type" "" []
    | prose =
        Interpreted raw stripped Nothing "prose" "" terms
    | otherwise =
        let (nm, scope, note) = resolveQualified env stripped
            shape = shapeOf nm
         in Interpreted raw nm scope shape note []
  where
    stripped = stripDecoration raw
    typeShaped = "->" `T.isInfixOf` raw || "::" `T.isInfixOf` raw
    prose = length (T.words stripped) > 1
    terms = map T.toLower (T.words stripped)

{- | The type a "how do I construct a value of T" question names (section
7.1): "value of type T", "make a T", "default T", "a T value". 'Nothing'
when the query is not a constructibility question. The goal is the type token
following the phrase (upper-head, a data\/record type name).
-}
constructGoal :: Text -> Maybe Text
constructGoal raw = firstJust (map afterPhrase phrases)
  where
    ws = T.words (T.strip raw)
    low = map T.toLower ws
    phrases =
        [ ["value", "of", "type"]
        , ["values", "of", "type"]
        , ["a", "value", "of"]
        , ["make", "a"]
        , ["make", "an"]
        , ["construct", "a"]
        , ["default"]
        , ["a", "default"]
        ]
    afterPhrase p = case dropUntilAfter p (zip low ws) of
        (ty : _) | typeToken ty -> Just ty
        _ -> Nothing
    typeToken t = upperHead t && T.all identChar t
    identChar c = isAlpha c || isDigit c || c == '.' || c == '_' || c == '\''
    dropUntilAfter p pairs
        | map fst (take (length p) pairs) == p = map snd (drop (length p) pairs)
        | otherwise = case pairs of
            (_ : rest) -> dropUntilAfter p rest
            [] -> []
    firstJust = foldr orElse Nothing
    orElse (Just x) _ = Just x
    orElse Nothing y = y

{- | Reduce a decorated name to its bare identifier: drop backtick/quote
wrappers, type applications (@col \@T.Text@) and applied literal arguments
(@col "revenue"@). Operators in parens survive verbatim (R2.3).
-}
stripDecoration :: Text -> Text
stripDecoration raw
    | "(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t && not (T.any (== ' ') t) = t
    | otherwise = case T.words t of
        (h : rest) | not (null rest), all decoration rest, identLike h -> h
        _ -> t
  where
    -- Quote wrappers unwrap only as PAIRS (R2.1): a bare trailing prime is
    -- part of the identifier (the foldl'\/data' class), never decoration.
    t = unwrapQuotes (T.strip raw)
    unwrapQuotes s = case (T.uncons s, T.unsnoc s) of
        (Just (o, _), Just (_, c))
            | T.length s >= 2
            , (o, c) `elem` quotePairs ->
                unwrapQuotes (T.strip (T.init (T.drop 1 s)))
        _ -> s
    quotePairs =
        [ ('`', '`')
        , ('`', '\'')
        , ('"', '"')
        , ('\'', '\'')
        , ('\x2018', '\x2019')
        ]
    decoration w =
        "@" `T.isPrefixOf` w
            || "\"" `T.isPrefixOf` w
            || T.all isDigit w
    identLike w = maybe False (\(c, _) -> isAlpha c || c == '_') (T.uncons w)

{- | Resolve a qualified name: an alias the notebook imports wins (@D.col@ ->
@col@ in @DataFrame@); a dotted or imported qualifier scopes by module; an
unknown qualifier falls back to the bare name and says so.
-}
resolveQualified ::
    NotebookEnv -> Text -> (Text, Maybe Text, Text)
resolveQualified env t
    | Just (qual, nm) <- splitQualified t =
        case lookup qual (neAliases env) of
            Just m ->
                ( nm
                , Just m
                , "alias " <> qual <> " = " <> m <> " (notebook import)"
                )
            Nothing
                | T.any (== '.') qual || qual `elem` neImports env ->
                    (nm, Just qual, "")
                | otherwise ->
                    ( nm
                    , Nothing
                    , "qualifier '"
                        <> qual
                        <> "' is not a notebook alias; searched the bare name"
                    )
    | otherwise = (t, Nothing, "")

{- | Split @Qual.name@ at the last dot when the head is a module-shaped
qualifier and the tail is a value name; module paths return Nothing.
-}
splitQualified :: Text -> Maybe (Text, Text)
splitQualified t
    | T.any (== '.') t
    , not (T.any (== ' ') t)
    , upperHead t
    , (qual, nm) <- breakLast t
    , not (T.null nm)
    , not (upperHead nm) =
        Just (qual, nm)
    | otherwise = Nothing
  where
    breakLast s =
        let parts = T.splitOn "." s
         in (T.intercalate "." (init parts), last parts)

upperHead :: Text -> Bool
upperHead s = maybe False (isUpper . fst) (T.uncons s)

-- | The shape of a resolved single-token query.
shapeOf :: Text -> Text
shapeOf nm
    | upperHead nm = "module"
    | T.any (== '-') nm = "package"
    | otherwise = "name"

-- | @dataframe-0.7.0.0@ reduces to @dataframe@ (R2.5).
stripVersion :: Text -> Text
stripVersion u
    | null kept = u
    | otherwise = T.intercalate "-" kept
  where
    parts = T.splitOn "-" u
    kept = reverse (dropWhile isVer (reverse parts))
    isVer p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p

{- | Build the environment from (source, defines) cell pairs: alias imports,
imported modules, cell-defined names — builtins seeded from the prompt source.
-}
envFromCells :: [(Text, [Text])] -> NotebookEnv
envFromCells cells =
    seededBuiltins
        NotebookEnv
            { neAliases = nub (concatMap (aliasesOf . fst) cells)
            , neImports = nub (concatMap (importsOf . fst) cells)
            , neImportCells = nubBy ((==) `on` fst) importCells
            , neBindings = nub (concatMap snd cells)
            , neBuiltins = []
            , neBuiltinModules = []
            }
  where
    aliasesOf src =
        [ (a, m)
        | l <- T.lines src
        , Just (m, Just a) <- [importParts l]
        ]
    importsOf src =
        [m | l <- T.lines src, Just (m, _) <- [importParts l]]
    importCells =
        [ (m, i)
        | (i, (src, _)) <- zip [0 ..] cells
        , l <- T.lines src
        , Just (m, _) <- [importParts l]
        ]

-- | The (module, alias) of a one-line @import [qualified] M [as A]@.
importParts :: Text -> Maybe (Text, Maybe Text)
importParts line = case T.words (T.strip line) of
    ("import" : rest) -> case dropWhile (== "qualified") rest of
        (m : more)
            | upperHead m -> Just (m, aliasIn more)
        _ -> Nothing
    _ -> Nothing
  where
    aliasIn ws = case dropWhile (/= "as") ws of
        ("as" : a : _) -> Just a
        _ -> Nothing

-- | The (source, defines) pairs of a @list_cells@ result.
parseCells :: Value -> [(Text, [Text])]
parseCells v = case v of
    Object o | Just cells <- KM.lookup "cells" o -> fromArray cells
    Array _ -> fromArray v
    _ -> []
  where
    fromArray (Array a) = map cellPair (toList a)
    fromArray _ = []
    cellPair (Object c) =
        ( textAt "source" c
        , [d | Just (Array ds) <- [KM.lookup "defines" c], String d <- toList ds]
        )
    cellPair _ = ("", [])
    textAt k c = case KM.lookup k c of
        Just (String s) -> s
        _ -> ""
