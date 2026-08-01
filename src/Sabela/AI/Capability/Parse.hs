{-# LANGUAGE OverloadedStrings #-}

{- | Reading a GHCi @:browse@ listing into the entities it declares.

One declaration is one entity, and a declaration that carries members — a
class's methods, a record's selectors — declares those too. 'parseCapabilities'
and the browse card both read a listing through 'coalesce' and this
decomposition, so one listing means the same thing to each.
-}
module Sabela.AI.Capability.Parse (
    Capability (..),
    coalesce,
    declMembers,
    parseCapabilities,
    unqualify,
) where

import Data.Char (isAlphaNum, isUpper)
import Data.List (foldl')
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data Capability = Capability
    { capModule :: Text
    , capName :: Text
    , capType :: Text
    , capField :: Maybe Text
    }
    deriving (Eq, Show)

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

{- | The named members a declaration carries: a class's methods and a record's
selectors, each with its own type. The head of the declaration is not a member
of itself, so the caller keeps it separately.
-}
declMembers :: Text -> [(Text, Text)]
declMembers ent = classMethodSigs ent ++ recordSelectorSigs ent

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
classMethods modName line =
    [ Capability modName (unqualify nm) (cleanType (ctx <> ty)) Nothing
    | (nm, ty) <- classMethodSigs line
    ]
  where
    ctx = "(" <> classHead line <> ") => "

{- | The methods a coalesced @class … where@ entity declares. The class head is
dropped: it is the entity, not a member of it.
-}
classMethodSigs :: Text -> [(Text, Text)]
classMethodSigs line
    | "class " `T.isPrefixOf` line
    , (_, afterWhere) <- T.breakOn " where " line
    , not (T.null afterWhere) =
        methodSigs (T.drop 7 afterWhere)
    | otherwise = []

{- | The class a method's constraint names, with any superclass context
dropped: @class (Typeable a) => ToJSON a where@ constrains its methods by
@ToJSON a@, not by the whole line.
-}
classHead :: Text -> Text
classHead line =
    T.strip
        . T.takeWhile (/= '|')
        . snd
        . T.breakOnEnd " => "
        $ fromMaybe headText (T.stripPrefix "class " headText)
  where
    headText = fst (T.breakOn " where " line)

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
recordSelectors modName line =
    [ Capability modName (unqualify nm) (cleanType ty) (Just (recordType line))
    | (nm, ty) <- recordSelectorSigs line
    ]

{- | The selectors a coalesced record declaration declares, typed as the
functions they are: a field @knots :: Int@ of @Speed@ is @Speed -> Int@.
-}
recordSelectorSigs :: Text -> [(Text, Text)]
recordSelectorSigs line
    | isData
    , Just braces <- betweenBraces line =
        [ (T.strip nm, recordType line <> " -> " <> T.strip ty)
        | grp <- groupFields (T.splitOn ", " braces)
        , let (nm, rest) = T.breakOn " :: " grp
        , not (T.null rest)
        , let ty = T.drop 4 rest
        ]
    | otherwise = []
  where
    isData = "data " `T.isPrefixOf` line || "newtype " `T.isPrefixOf` line

-- | The type a record declaration defines, type parameters included.
recordType :: Text -> Text
recordType line = unqualify (T.strip (T.takeWhile (/= '=') (afterKeyword line)))
  where
    afterKeyword l
        | Just r <- T.stripPrefix "data " l = r
        | Just r <- T.stripPrefix "newtype " l = r
        | otherwise = l

betweenBraces :: Text -> Maybe Text
betweenBraces t = case T.breakOn "{" t of
    (_, r) | not (T.null r) -> Just (T.takeWhile (/= '}') (T.drop 1 r))
    _ -> Nothing

groupFields :: [Text] -> [Text]
groupFields = reverse . foldl' step []
  where
    step acc p
        | " :: " `T.isInfixOf` p = p : acc
        | otherwise = case acc of
            (cur : rest) -> (cur <> ", " <> p) : rest
            [] -> [p]

{- | GHCi indents the continuation of a wrapped declaration, so an indented
line belongs to the declaration above it.
-}
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
