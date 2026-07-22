{- | The @discover@ request schema (docs/discover/search-api.md §3): ONE
definition generates the model-facing catalogue entry AND drives the
validator, so the offered surface and the enforced surface cannot drift
(R1.7 by construction — no dead knobs). Every accepted argument observably
changes the result or the call is rejected with a one-line reason.
-}
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
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (DHit (..), Scope (..), emptyScope)

{- | The three answer shapes: ranked search hits, the M6 inventory card, or
the section-7.1 constructibility facet (producers of the queried type).
-}
data DiscoverMode = ModeSearch | ModeInventory | ModeConstruct
    deriving (Eq, Show)

-- | A validated discover call: query, scope, result bound and answer mode.
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

-- | The inclusive @limit@ range the schema declares.
limitBounds :: (Int, Int)
limitBounds = (1, 25)

{- | The one schema definition: (name, JSON type, required, description).
'requestProperties' (the catalogue entry) and 'parseRequest' (the validator)
both derive from this list.
-}
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
        , "Result cap, 1..25 (default 8); honoured exactly, including 1."
        )
    ,
        ( "mode"
        , "string"
        , False
        , "\"search\" (default) ranks matching names; \"inventory\" answers \
          \'what is available for this topic': one bounded card listing \
          \candidate packages as installed, hidden, or absent-known (with \
          \the -- cabal: line to declare); \"construct\" answers 'how do I \
          \make a value of type T' by ranking producers of T (a ready-made \
          \value first, then constructors, then functions returning T)."
        )
    ]

requestArgNames :: [Text]
requestArgNames = [n | (n, _, _, _) <- requestSchema]

-- | The catalogue entry's property schemas, generated from 'requestSchema'.
requestProperties :: [(Text, Value)]
requestProperties =
    [ (n, object ["type" .= ty, "description" .= d])
    | (n, ty, _, d) <- requestSchema
    ]

requestRequired :: [Text]
requestRequired = [n | (n, _, req, _) <- requestSchema, req]

{- | Validate a call's arguments against 'requestSchema'. The query arrives
separately (it may be baked into the tool name); unknown extra keys are
tolerated, declared keys are type- and bounds-checked.
-}
parseRequest :: Text -> Value -> Either Text DiscoverRequest
parseRequest q (Object o) = do
    m <- optText "module" o
    p <- optText "package" o
    l <- optLimit o
    md <- optMode o
    Right (DiscoverRequest q (Scope m p) (fromMaybe defaultLimit l) md)
parseRequest q _ = Right (defaultRequest q)

-- Blank strings mean "omitted" throughout: the schema offers every property
-- and a literal caller fills them all, so "" must read as absence, never as
-- a bad_request (the run-20260720 barChart derail).
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

-- | Does a merged hit fall within the request's scope filters?
inScope :: Scope -> DHit -> Bool
inScope (Scope m p) h =
    maybe True moduleMatch m && maybe True (== dhPackage h) p
  where
    moduleMatch f = dhModule h == f || (f <> ".") `T.isPrefixOf` dhModule h

scopeActive :: Scope -> Bool
scopeActive s = s /= emptyScope

{- | The scope's own name, standing in for a blank query in inventory mode
(R2.6): the miss guidance steers exactly there, so it must be a legal call.
-}
scopeFallbackQuery :: Scope -> Maybe Text
scopeFallbackQuery (Scope m p) = case (m, p) of
    (Just f, _) -> Just f
    (_, Just f) -> Just f
    _ -> Nothing

-- | The filter's spelled-out form for disclosure lines.
scopeText :: Scope -> Text
scopeText (Scope m p) =
    T.intercalate
        " "
        ( ["module=" <> f | Just f <- [m]]
            ++ ["package=" <> f | Just f <- [p]]
        )

{- | The R2.7 disclosure for a scope filter, from (kept, unfiltered) counts:
matched-none and changed-nothing are stated; a genuine narrowing reports
kept-of-total. 'Nothing' when no filter is active.
-}
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

{- | The query of a @discover@ call: the @query@ argument, or (weak models
bake arguments into the name) the remainder of the tool name. 'Nothing' for
any other tool.
-}
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

{- | The history-ledger key of a discover call: query plus non-default knobs
('requestKey'), so a filtered or re-bounded repeat is never deduped into an
earlier answer (the knobs stay observable, R1.7). 'Nothing' for other tools.
-}
discoverKey :: Text -> Value -> Maybe Text
discoverKey name argv = do
    q <- discoverQuery name argv
    pure (requestKey q argv)

{- | The history-ledger key of a request: the query plus any non-default
knobs, so a re-query with a different filter or bound is never deduped into
the earlier answer (the knobs stay observable, R1.7).
-}
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

-- | The ledger-key decoration a non-default mode adds (default is blank).
modeKey :: DiscoverMode -> Text
modeKey ModeSearch = ""
modeKey ModeInventory = " [mode=inventory]"
modeKey ModeConstruct = " [mode=construct]"

tShow :: Int -> Text
tShow = T.pack . show
