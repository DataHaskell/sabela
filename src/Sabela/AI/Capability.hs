{-# LANGUAGE OverloadedStrings #-}

{- | A local, Hoogle-style search over the session's installed modules: each
exposed function is a 'Capability' (module, name, type), and 'searchCapabilities'
ranks the index (built from @:browse@) against a free-text query.
-}
module Sabela.AI.Capability (
    Capability (..),
    Match (..),
    Hit (..),
    Synonyms,
    defaultSynonyms,
    searchCapabilities,
    parseCapabilities,
) where

import Control.Monad (guard)
import Data.Char (isAlphaNum)
import Data.List (nubBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

-- | One exposed function: the unit of search, parsed from a @:browse@ line.
data Capability = Capability
    { capModule :: Text
    , capName :: Text
    , capType :: Text
    }
    deriving (Eq, Show)

-- | Why a capability matched (the scoring signal that won).
data Match = ByName | ByType | BySynonym | ByModule
    deriving (Eq, Show)

data Hit = Hit
    { hitCap :: Capability
    , hitScore :: Int
    , hitVia :: Match
    }
    deriving (Eq, Show)

-- | Query-vocabulary → API-vocabulary bridges, all lower-case.
type Synonyms = [(Text, [Text])]

{- | Query-word → API-word bridges for cases substring matching misses
(e.g. "classification" → "logistic").
-}
defaultSynonyms :: Synonyms
defaultSynonyms =
    [ ("animation", ["anim"])
    , ("animate", ["anim"])
    , ("move", ["anim"])
    , ("frame", ["anim"])
    , ("classification", ["logistic"])
    , ("classify", ["logistic"])
    , ("classifier", ["logistic"])
    , ("plot", ["bars", "linegraph", "scatter", "pie", "area"])
    , ("chart", ["bars", "linegraph"])
    , ("graph", ["linegraph", "scatter"])
    , ("regression", ["linear", "fit"])
    , ("reactive", ["frp", "behavior", "event"])
    , ("frp", ["behavior", "event"])
    ]

{- | Rank an index against a free-text query; best matches first, FOCUSED: a
high-confidence (exact/prefix) hit silences the low-confidence tail, and the
list is capped well below a wall — low-confidence walls are negative
information for a small model. Ties break toward the SHORTER module path (the
public umbrella API, not deep internals); duplicates of the same function — an
umbrella module re-exporting it — collapse to one row.
-}
searchCapabilities :: Synonyms -> [Capability] -> Text -> [Hit]
searchCapabilities syns idx query =
    focus $
        nubByNameType $
            sortOn rank $
                [Hit c s v | c <- idx, Just (s, v) <- [scoreCap syns ql qToks c]]
  where
    ql = lexQuery (T.toLower (T.strip query))
    qToks = tokens ql
    rank h = (Down (hitScore h), T.length (capModule (hitCap h)))
    nubByNameType =
        nubBy (\a b -> sameKey (hitCap a) (hitCap b))
    sameKey x y = capName x == capName y && capType x == capType y
    focus hits = case hits of
        (h : _) | hitScore h >= 80 -> take 5 (takeWhile ((>= 80) . hitScore) hits)
        _ -> take 8 hits

{- | Strip Haskell surface syntax a name index can never match — type
applications (@\@Type@) and string-literal arguments — so a query written the
way code is written still lands on the exact tier.
-}
lexQuery :: Text -> Text
lexQuery q =
    T.unwords
        [ w
        | w <- T.words (dropLiterals q)
        , not ("@" `T.isPrefixOf` w)
        , w /= "@"
        ]
  where
    dropLiterals = T.pack . go False . T.unpack
    go _ [] = []
    go True ('"' : cs) = go False cs
    go True (_ : cs) = go True cs
    go False ('"' : cs) = go True cs
    go False (c : cs) = c : go False cs

{- | The highest-scoring signal a capability matches on, or Nothing — ordered
strongest first (exact > prefix > substring > token > type > synonym > module),
so 'listToMaybe' takes the winner.
-}
scoreCap :: Synonyms -> Text -> [Text] -> Capability -> Maybe (Int, Match)
scoreCap syns ql qToks c =
    listToMaybe $
        catMaybes
            [ (100, ByName) <$ guard (ql == nameL)
            , (80, ByName) <$ guard (ql `T.isPrefixOf` nameL)
            , (60, ByName) <$ guard (ql `T.isInfixOf` nameL)
            , (55, ByName) <$ guard (any tokenInName qToks)
            , (50, ByType) <$ guard typeMatch
            , (40, BySynonym) <$ guard synMatch
            , (30, ByModule) <$ guard (ql `T.isInfixOf` T.toLower (capModule c))
            ]
  where
    nameL = T.toLower (capName c)
    typeL = T.toLower (capType c)
    tokenInName t = T.length t >= 3 && t `T.isInfixOf` nameL
    typeMatch = length qToks >= 2 && all (`T.isInfixOf` typeL) qToks
    synMatch = any (`T.isInfixOf` nameL) (synonymsFor syns ql)

{- | Synonym expansions whose key occurs as a WHOLE token of the query — a
substring match let a key inside a longer domain word bridge to an unrelated
vocabulary, and the wrong-domain "hit" then blocked the discover fallthrough.
-}
synonymsFor :: Synonyms -> Text -> [Text]
synonymsFor syns ql = concat [vs | (k, vs) <- syns, k `elem` toks]
  where
    toks = tokens ql

-- | Alphanumeric tokens: "double -> picture" → ["double","picture"].
tokens :: Text -> [Text]
tokens = filter (not . T.null) . T.split (not . isAlphaNum)

{- | Parse @:browse M@ output into capabilities, handling its two quirks:
fully-qualified names (stripped) and signatures wrapped across indented
continuation lines (coalesced). Skips type/data/class declarations.
-}
parseCapabilities :: Text -> Text -> [Capability]
parseCapabilities modName raw =
    concat
        [ case valueBinding ent of
            Just (nm, ty) -> [Capability modName (unqualify nm) (cleanType ty)]
            Nothing -> recordSelectors modName ent ++ classMethods modName ent
        | ent <- coalesce (T.lines raw)
        ]

{- | Class methods from a coalesced @class C ... where m :: T@ declaration, so
@find_function@ finds a polymorphic verb like @fit@ / @predict@ — buried inside
the @class@ block, not a top-level binding. The method type is prefixed with the
class context so the model sees what it ranges over. Empty for non-class lines.
-}
classMethods :: Text -> Text -> [Capability]
classMethods modName line
    | "class " `T.isPrefixOf` line
    , (_, afterWhere) <- T.breakOn " where " line
    , not (T.null afterWhere) =
        [ Capability modName (unqualify nm) (ctx <> cleanType ty)
        | (nm, ty) <- methodSigs (T.drop 7 afterWhere)
        ]
    | otherwise = []
  where
    -- The class head ("Fit cfg input model") becomes the method's constraint.
    classHead = T.strip (T.takeWhile (/= '|') (afterClass (fst (T.breakOn " where " line))))
    ctx = "(" <> cleanType classHead <> ") => "
    afterClass l = fromMaybe l (T.stripPrefix "class " l)

{- | Split a class body's @m1 :: T1 m2 :: T2@ run into @(name, type)@ pairs. The
token just before each @::@ is the next method name; the rest is the prior
method's type (GHCi prints one method per line, coalesced to one run here).
-}
methodSigs :: Text -> [(Text, Text)]
methodSigs body = case T.splitOn " :: " body of
    (n0 : rest) -> pair n0 rest
    [] -> []
  where
    pair _ [] = []
    pair nm [ty] = [(T.strip nm, T.strip ty)]
    pair nm (chunk : more) =
        (T.strip nm, T.strip (T.dropWhileEnd (/= ' ') chunk))
            : pair (lastWord chunk) more
    lastWord = T.takeWhileEnd (/= ' ')

{- | Record field selectors from a coalesced @data@/@newtype@ declaration —
@field :: Record -> FieldType@ — so @find_function@ finds a field by name
(e.g. @maxTreeDepth@) even though @:browse@ buries it inside the data block,
not as a top-level binding. Empty for non-record declarations.
-}
recordSelectors :: Text -> Text -> [Capability]
recordSelectors modName line
    | isData
    , Just braces <- betweenBraces line =
        [ Capability
            modName
            (unqualify (T.strip nm))
            (tyName <> " -> " <> cleanType (T.strip ty))
        | grp <- groupFields (T.splitOn ", " braces)
        , let (nm, rest) = T.breakOn " :: " grp
        , not (T.null rest)
        , let ty = T.drop 4 rest
        ]
    | otherwise = []
  where
    isData = "data " `T.isPrefixOf` line || "newtype " `T.isPrefixOf` line
    tyName = unqualify (T.takeWhile (\c -> c /= ' ' && c /= '=') (afterKeyword line))
    afterKeyword l
        | Just r <- T.stripPrefix "data " l = r
        | Just r <- T.stripPrefix "newtype " l = r
        | otherwise = l

-- | The text between the first @{@ and the next @}@, if any.
betweenBraces :: Text -> Maybe Text
betweenBraces t = case T.breakOn "{" t of
    (_, r) | not (T.null r) -> Just (T.takeWhile (/= '}') (T.drop 1 r))
    _ -> Nothing

{- | Regroup brace pieces split on @", "@ so a field type carrying its own comma
(e.g. @[(Text, Text)]@) stays whole: a piece without @::@ continues the field
before it rather than starting a new one.
-}
groupFields :: [Text] -> [Text]
groupFields = reverse . foldl step []
  where
    step acc p
        | " :: " `T.isInfixOf` p = p : acc
        | otherwise = case acc of
            (cur : rest) -> (cur <> ", " <> p) : rest
            [] -> [p]

-- | Join indented continuation lines into the entry they continue.
coalesce :: [Text] -> [Text]
coalesce [] = []
coalesce (l : ls) = go l ls
  where
    go cur [] = [cur]
    go cur (x : xs)
        | isCont x = go (cur <> " " <> T.strip x) xs
        | otherwise = cur : go x xs
    isCont x = case T.uncons x of
        Just (h, _) -> h == ' ' || h == '\t'
        Nothing -> False

-- | A @name :: type@ value binding (not a type/data/class/instance decl).
valueBinding :: Text -> Maybe (Text, Text)
valueBinding ent
    | any (`T.isPrefixOf` ent) declKeywords = Nothing
    | otherwise =
        let (nm, rest) = T.breakOn " :: " ent
         in if T.null rest
                then Nothing
                else Just (T.strip nm, T.strip (T.drop 4 rest))

declKeywords :: [Text]
declKeywords = ["type ", "data ", "newtype ", "class ", "instance ", "pattern "]

-- | The unqualified name: the last @.@-separated component.
unqualify :: Text -> Text
unqualify = T.takeWhileEnd (/= '.')

{- | Render a qualified signature readably: drop @pkg-version:@ prefixes and
module qualifiers, keeping each name's last component (so
@sabela-notebook-…:…Picture.Internal.Picture@ becomes @Picture@).
-}
cleanType :: Text -> Text
cleanType = T.concat . map reduce . chunk
  where
    chunk s
        | T.null s = []
        | isAtom (T.head s) = let (a, r) = T.span isAtom s in a : chunk r
        | otherwise = let (a, r) = T.break isAtom s in a : chunk r
    isAtom c = isAlphaNum c || c `elem` ['.', '_', '\'', ':', '-']
    reduce a
        | T.any (`elem` ['.', ':']) a = lastComponent a
        | otherwise = a
    lastComponent a =
        let afterColon =
                if T.any (== ':') a then T.drop 1 (T.dropWhile (/= ':') a) else a
         in T.takeWhileEnd (/= '.') afterColon
