{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capability (
    Capability (..),
    Match (..),
    Hit (..),
    Synonyms,
    defaultSynonyms,
    searchCapabilities,
    parseCapabilities,
    relevanceScore,
    unqualify,
) where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isUpper)
import Data.List (nubBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Similarity (trigramSimilarity)

data Capability = Capability
    { capModule :: Text
    , capName :: Text
    , capType :: Text
    , capField :: Maybe Text
    }
    deriving (Eq, Show)

data Match = ByName | ByType | BySynonym | ByModule
    deriving (Eq, Show)

data Hit = Hit
    { hitCap :: Capability
    , hitScore :: Int
    , hitVia :: Match
    }
    deriving (Eq, Show)

type Synonyms = [(Text, [Text])]

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
    , ("overlay", ["group", "mconcat"])
    , ("superimpose", ["group", "mconcat"])
    , ("combine", ["group", "mconcat"])
    , ("compose", ["group", "mconcat"])
    , ("stack", ["group", "mconcat"])
    , ("pictures", ["picture"])
    ]

searchCapabilities :: Synonyms -> [Capability] -> Text -> [Hit]
searchCapabilities syns idx query =
    focus $
        nubByNameType $
            sortOn rank $
                [Hit c s v | c <- idx, Just (s, v) <- [scoreCap syns ql qToks c]]
  where
    ql = lexQuery (T.toLower (T.strip query))
    qRaw = lexQuery (T.strip query)
    qToks = tokens ql
    rank h = (Down (hitScore h), caseRank h, T.length (capModule (hitCap h)))
    caseRank h = if capName (hitCap h) == qRaw then 0 else 1 :: Int
    nubByNameType =
        nubBy (\a b -> sameKey (hitCap a) (hitCap b))
    sameKey x y = capName x == capName y && capType x == capType y
    focus hits = case hits of
        (h : _)
            | hitScore h >= exactScore ->
                take 5 (takeWhile ((>= exactScore) . hitScore) hits)
        _ -> take 8 hits
    exactScore = 100

relevanceScore :: Synonyms -> Text -> Capability -> Int
relevanceScore syns query c = maybe 0 fst (scoreCap syns ql (tokens ql) c)
  where
    ql = lexQuery (T.toLower (T.strip query))

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

scoreCap :: Synonyms -> Text -> [Text] -> Capability -> Maybe (Int, Match)
scoreCap syns ql qToks c =
    listToMaybe $
        catMaybes
            [ (100, ByName) <$ guard (ql == nameL)
            , (80, ByName) <$ guard (ql `T.isPrefixOf` nameL)
            , (60, ByName) <$ guard (ql `T.isInfixOf` nameL)
            , (58, ByType) <$ guard (typeShaped && typeMatch)
            , (55, ByName) <$ guard (any tokenInName qToks)
            , (50, ByType) <$ guard typeMatch
            , (45, ByName) <$ guard nearSpelling
            , (40, BySynonym) <$ guard synMatch
            , (30, ByModule) <$ guard (ql `T.isInfixOf` T.toLower (capModule c))
            ]
  where
    nameL = T.toLower (capName c)
    typeL = T.toLower (capType c)
    tokenInName t = T.length t >= 3 && t `T.isInfixOf` nameL
    typeMatch = length qToks >= 2 && all (`T.isInfixOf` typeL) qToks
    synMatch = any (`T.isInfixOf` nameL) (synonymsFor syns ql)
    nearSpelling =
        T.length ql >= minFuzzyQuery
            && trigramSimilarity ql nameL >= fuzzyNameThreshold
    typeShaped = "->" `T.isInfixOf` ql

fuzzyNameThreshold :: Double
fuzzyNameThreshold = 0.4

minFuzzyQuery :: Int
minFuzzyQuery = 4

synonymsFor :: Synonyms -> Text -> [Text]
synonymsFor syns ql = concat [vs | (k, vs) <- syns, k `elem` toks]
  where
    toks = tokens ql

tokens :: Text -> [Text]
tokens = filter (not . T.null) . T.split (not . isAlphaNum)

parseCapabilities :: Text -> Text -> [Capability]
parseCapabilities modName raw =
    concat
        [ case valueBinding ent of
            Just (nm, ty) -> [Capability modName (unqualify nm) (cleanType ty) Nothing]
            Nothing ->
                typeDeclaration modName ent
                    ++ recordSelectors modName ent
                    ++ classMethods modName ent
        | ent <- coalesce (T.lines raw)
        ]

typeDeclaration :: Text -> Text -> [Capability]
typeDeclaration modName ent =
    [ Capability modName (unqualify nm) (cleanType head_) Nothing
    | Just kw <- [keywordOf ent]
    , let head_ = declHead kw
          nm = T.takeWhile (\c -> c /= ' ' && c /= ':') head_
    , not (" :: " `T.isInfixOf` head_)
    , not (T.null nm)
    , isUpper (T.head (unqualify nm))
    ]
  where
    keywordOf l =
        listToMaybe
            [ k
            | k <- ["type ", "data ", "newtype ", "class "]
            , k `T.isPrefixOf` l
            ]
    declHead kw =
        T.strip
            . T.takeWhile (\c -> c /= '=' && c /= '{')
            . fst
            . T.breakOn " where "
            . snd
            . T.breakOnEnd " => "
            . T.strip
            . T.drop (T.length kw)
            $ T.strip ent

classMethods :: Text -> Text -> [Capability]
classMethods modName line
    | "class " `T.isPrefixOf` line
    , (_, afterWhere) <- T.breakOn " where " line
    , not (T.null afterWhere) =
        [ Capability modName (unqualify nm) (ctx <> cleanType ty) Nothing
        | (nm, ty) <- methodSigs (T.drop 7 afterWhere)
        ]
    | otherwise = []
  where
    classHead = T.strip (T.takeWhile (/= '|') (afterClass (fst (T.breakOn " where " line))))
    ctx = "(" <> cleanType classHead <> ") => "
    afterClass l = fromMaybe l (T.stripPrefix "class " l)

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

recordSelectors :: Text -> Text -> [Capability]
recordSelectors modName line
    | isData
    , Just braces <- betweenBraces line =
        [ Capability
            modName
            (unqualify (T.strip nm))
            (tyName <> " -> " <> cleanType (T.strip ty))
            (Just tyName)
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

betweenBraces :: Text -> Maybe Text
betweenBraces t = case T.breakOn "{" t of
    (_, r) | not (T.null r) -> Just (T.takeWhile (/= '}') (T.drop 1 r))
    _ -> Nothing

groupFields :: [Text] -> [Text]
groupFields = reverse . foldl step []
  where
    step acc p
        | " :: " `T.isInfixOf` p = p : acc
        | otherwise = case acc of
            (cur : rest) -> (cur <> ", " <> p) : rest
            [] -> [p]

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

unqualify :: Text -> Text
unqualify t
    | "(" `T.isPrefixOf` t
    , ")" `T.isSuffixOf` t =
        "(" <> dropQualifier (dropUnit (T.init (T.drop 1 t))) <> ")"
    | otherwise = dropQualifier (dropUnit t)

dropUnit :: Text -> Text
dropUnit t = case T.breakOnEnd ":" t of
    (pre, rest) | not (T.null pre) -> rest
    _ -> t

dropQualifier :: Text -> Text
dropQualifier t = case T.uncons t of
    Just (c, _)
        | isUpper c
        , (comp, rest) <- T.breakOn "." t
        , not (T.null rest)
        , T.all (\x -> isAlphaNum x || x == '\'' || x == '_') comp ->
            dropQualifier (T.drop 1 rest)
    _ -> t

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
