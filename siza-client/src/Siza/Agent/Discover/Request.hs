module Siza.Agent.Discover.Request (
    DiscoverMode (..),
    DiscoverRequest (..),
    defaultRequest,
    defaultLimit,
    discoverKey,
    discoverQuery,
    limitBounds,
    parseRequest,
    requestArgNames,
    requestProperties,
    requestRequired,
    inScope,
    scopeActive,
    scopeDisclosure,
    scopeFallbackQuery,
    scopeText,
    requestKey,
    effectiveQuery,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (DHit (..), Scope (..), emptyScope)

data DiscoverMode = ModeSearch | ModeInventory | ModeConstruct
    deriving (Eq, Show)

data DiscoverRequest = DiscoverRequest
    { drQuery :: Text
    , drScope :: Scope
    , drLimit :: Int
    , drMode :: DiscoverMode
    }
    deriving (Eq, Show)

defaultRequest :: Text -> DiscoverRequest
defaultRequest q = DiscoverRequest q emptyScope defaultLimit ModeSearch

defaultLimit :: Int
defaultLimit = 8

limitBounds :: (Int, Int)
limitBounds = (1, 25)

requestSchema :: [(Text, Text, Bool, Text)]
requestSchema =
    [
        ( "query"
        , "string"
        , False
        , "A name (\"divvy\"), a goal type (\"[Int] -> Int\"), a module \
          \(\"Granite.Svg\"), a package, or a plain-language description. \
          \Required non-blank, EXCEPT in mode \"inventory\" with a module \
          \or package set, where it may be omitted."
        )
    ,
        ( "module"
        , "string"
        , False
        , "Only hits in this module (or its submodules); honoured, or the \
          \result says the filter matched nothing."
        )
    ,
        ( "package"
        , "string"
        , False
        , "Only hits in this package; honoured, or the result says the \
          \filter matched nothing."
        )
    ,
        ( "limit"
        , "integer"
        , False
        , "Result cap, 1..25 (default 8). An upper bound: not-installed packages and internal modules are capped below it, so fewer may be shown."
        )
    ,
        ( "mode"
        , "string"
        , False
        , "Leave unset for \"search\" (the default), which is how you find a \
          \function, type or module and is the only mode that states \
          \signatures. \"inventory\" answers the narrower question 'which \
          \PACKAGES exist for this topic': one bounded card listing them as \
          \installed, hidden, or absent-known (with the build-depends line \
          \to declare), and no signatures — so it cannot say what a function \
          \takes or returns. \"construct\" answers 'how do I make a value of \
          \type T' by ranking producers of T (a ready-made value first, then \
          \constructors, then functions returning T)."
        )
    ]

requestArgNames :: [Text]
requestArgNames = [n | (n, _, _, _) <- requestSchema]

requestProperties :: [(Text, Value)]
requestProperties =
    [ (n, object ["type" .= ty, "description" .= d])
    | (n, ty, _, d) <- requestSchema
    ]

requestRequired :: [Text]
requestRequired = [n | (n, _, req, _) <- requestSchema, req]

parseRequest :: Text -> Value -> Either Text DiscoverRequest
parseRequest q (Object o) = do
    m <- optText "module" o
    p <- optText "package" o
    l <- optLimit o
    md <- optMode o
    Right (DiscoverRequest q (Scope m p) (fromMaybe defaultLimit l) md)
parseRequest q _ = Right (defaultRequest q)

optMode :: KM.KeyMap Value -> Either Text DiscoverMode
optMode o = case KM.lookup "mode" o of
    Nothing -> Right ModeSearch
    Just Null -> Right ModeSearch
    Just (String s) | T.null (T.strip s) -> Right ModeSearch
    Just (String "search") -> Right ModeSearch
    Just (String "inventory") -> Right ModeInventory
    Just (String "construct") -> Right ModeConstruct
    Just _ -> Left "mode must be \"search\", \"inventory\" or \"construct\""

optText :: Text -> KM.KeyMap Value -> Either Text (Maybe Text)
optText k o = case KM.lookup (K.fromText k) o of
    Nothing -> Right Nothing
    Just Null -> Right Nothing
    Just (String s)
        | T.null (T.strip s) -> Right Nothing
        | otherwise -> Right (Just (T.strip s))
    Just _ -> Left (k <> " must be a string (or omitted)")

optLimit :: KM.KeyMap Value -> Either Text (Maybe Int)
optLimit o = case KM.lookup "limit" o of
    Nothing -> Right Nothing
    Just Null -> Right Nothing
    Just (Number n)
        | fromIntegral r == n && r >= lo && r <= hi -> Right (Just r)
      where
        r = round n :: Int
    Just _ -> Left limitReason
  where
    (lo, hi) = limitBounds
    limitReason =
        "limit must be an integer between "
            <> tShow lo
            <> " and "
            <> tShow hi

inScope :: Scope -> DHit -> Bool
inScope (Scope m p) h =
    maybe True moduleMatch m && maybe True (== dhPackage h) p
  where
    moduleMatch f = dhModule h == f || (f <> ".") `T.isPrefixOf` dhModule h

scopeActive :: Scope -> Bool
scopeActive s = s /= emptyScope

scopeFallbackQuery :: Scope -> Maybe Text
scopeFallbackQuery (Scope m p) = case (m, p) of
    (Just f, _) -> Just f
    (_, Just f) -> Just f
    _ -> Nothing

{- | What a request actually asks. A bare @module=@ or @package=@ scope with no
query asks for that node's card, in every mode: naming a scope is a question,
not a malformed search.
-}
effectiveQuery :: DiscoverRequest -> Text
effectiveQuery req = case T.strip (drQuery req) of
    "" -> fromMaybe "" (scopeFallbackQuery (drScope req))
    q -> q

scopeText :: Scope -> Text
scopeText (Scope m p) =
    T.intercalate
        " "
        ( ["module=" <> f | Just f <- [m]]
            ++ ["package=" <> f | Just f <- [p]]
        )

scopeDisclosure :: Scope -> Int -> Int -> Maybe Text
scopeDisclosure scope kept unfiltered
    | not (scopeActive scope) = Nothing
    | kept == 0 && unfiltered > 0 =
        Just $
            scopeText scope
                <> " matched none of the "
                <> tShow unfiltered
                <> " hits; drop the filter to see them"
    | kept == unfiltered =
        Just $
            scopeText scope
                <> " changed nothing: all "
                <> tShow kept
                <> " hits already match"
    | otherwise =
        Just $
            scopeText scope
                <> " kept "
                <> tShow kept
                <> " of "
                <> tShow unfiltered
                <> " hits"

discoverQuery :: Text -> Value -> Maybe Text
discoverQuery name argv = case T.break (== ' ') (T.strip name) of
    ("discover", rest) -> argQuery `orElse` inline rest
    _ -> Nothing
  where
    argQuery = case argv of
        Object o | Just (String q) <- KM.lookup "query" o -> Just q
        _ -> Nothing
    inline rest =
        let q = T.dropAround (\c -> c `elem` ("\"'` " :: String)) rest
         in if T.null q then Nothing else Just q
    orElse (Just x) _ = Just x
    orElse Nothing y = y

discoverKey :: Text -> Value -> Maybe Text
discoverKey name argv = do
    q <- discoverQuery name argv
    pure (requestKey q argv)

requestKey :: Text -> Value -> Text
requestKey q args = case parseRequest q args of
    Left reason -> q <> " [invalid: " <> reason <> "]"
    Right req ->
        T.strip q
            <> ( if scopeActive (drScope req)
                    then " [" <> scopeText (drScope req) <> "]"
                    else ""
               )
            <> ( if drLimit req /= defaultLimit
                    then " [limit=" <> tShow (drLimit req) <> "]"
                    else ""
               )
            <> modeKey (drMode req)

modeKey :: DiscoverMode -> Text
modeKey ModeSearch = ""
modeKey ModeInventory = " [mode=inventory]"
modeKey ModeConstruct = " [mode=construct]"

tShow :: Int -> Text
tShow = T.pack . show
