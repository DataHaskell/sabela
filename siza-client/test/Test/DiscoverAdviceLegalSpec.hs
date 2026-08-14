{-# LANGUAGE OverloadedStrings #-}

{- | Advice that names a call the tool would reject is worse than no advice:
the caller spends a turn learning the harness contradicts itself. The
2026-08-07 hodatime episode was told, on every one of eight misses, to "call
discover with mode=\"inventory\" and no query" — which the entry guard rejects
as a bad_request unless a scope supplies the query.
-}
module Test.DiscoverAdviceLegalSpec (discoverAdviceLegalSpec) where

import Control.Monad (when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Data.Either (isRight)
import Sabela.AI.ReadSourceArgs (parseReadSourceArgs)
import Siza.Agent.Discover.Request (
    effectiveQuery,
    parseRequest,
 )
import Test.DiscoverFixtures (
    installFactsRows,
    installNamesFileWith,
    runCatArgs,
    synHackageNames,
    textField,
 )
import Test.LadderFixtures (hodatimeFactsRow)

{- | Every call the advice names for one tool, as the caller would have to
type it. Reads @tool {k: "v", …}@ (and the older @k="v"@) out of the prose;
advice that names no call yields none.
-}
callsNamedIn :: Text -> Text -> [[(Text, Text)]]
callsNamedIn tool = map pairsOf . braceBodies
  where
    opener = tool <> " {"
    braceBodies t = case T.breakOn opener t of
        (_, rest)
            | T.null rest -> []
            | otherwise ->
                let body = T.drop (T.length opener) rest
                    (inside, after) = T.breakOn "}" body
                 in inside : braceBodies after
    pairsOf = map pairOf . T.splitOn ","
    pairOf kv =
        let (k, v) = T.break (\c -> c == '=' || c == ':') kv
         in (T.strip k, unquote (T.strip (T.drop 1 v)))
    unquote = T.dropAround (\c -> c == '"' || c == '\'')

-- | Whether discover would accept the call, by the guard the tool applies.
accepts :: [(Text, Text)] -> Bool
accepts kvs = case parseRequest (queryIn kvs) (object fields) of
    Left _ -> False
    Right req -> not (T.null (T.strip (effectiveQuery req)))
  where
    fields = [K.fromText k .= v | (k, v) <- kvs, not (T.null k)]

-- | Whether read_source would accept the call, by its own shared grammar.
acceptsReadSource :: [(Text, Text)] -> Bool
acceptsReadSource kvs =
    isRight (parseReadSourceArgs (object [K.fromText k .= v | (k, v) <- kvs]))

queryIn :: [(Text, Text)] -> Text
queryIn kvs = fromMaybe "" (lookup "query" kvs)

installWorld :: IO ()
installWorld = do
    installNamesFileWith (synHackageNames ++ ["hodatime"])
    installFactsRows [hodatimeFactsRow]

discoverAdviceLegalSpec :: Spec
discoverAdviceLegalSpec =
    before_ installWorld $
        describe "advice names a call the tool accepts (live 20260807)" $ do
            it "never advises a call the entry guard would reject" $
                mapM_ assertLegal probes

            it "does not advise inventory with neither query nor scope" $
                mapM_ assertNoBareInventory probes

            {- Inventory lists candidate PACKAGES for a topic; it cannot state
            a signature. Offered on a miss for a name, it sends the caller to a
            mode that structurally cannot answer them — the live 20260807-2214
            episode spent all 14 of its calls there. -}
            it "does not offer inventory for a miss on a name" $ do
                next <- nextOf (object ["query" .= ("zzznotathing" :: Text)])
                next `shouldSatisfy` (not . T.isInfixOf "inventory")

            it "does not offer it for a miss on a module either" $ do
                next <- nextOf (object ["query" .= ("Zzz.Notathing" :: Text)])
                next `shouldSatisfy` (not . T.isInfixOf "inventory")

            it "still offers it for a topic, which is what it answers" $ do
                next <- nextOf (object ["query" .= ("zzz qqq topic" :: Text)])
                next `shouldSatisfy` T.isInfixOf "inventory"

nextOf :: Value -> IO Text
nextOf args = textField "next" <$> runCatArgs (queryOf args) args

assertLegal :: (Text, Value) -> Expectation
assertLegal (label, args) = do
    v <- runCatArgs (queryOf args) args
    let advice = textField "next" v <> " " <> textField "narrow" v
        illegal tool ok =
            [kvs | kvs <- callsNamedIn tool advice, not (ok kvs)]
    mapM_
        ( \(tool, ok) ->
            mapM_
                ( \kvs ->
                    expectationFailure
                        ( T.unpack label
                            <> " advises a rejected "
                            <> T.unpack tool
                            <> " call: "
                            <> show kvs
                            <> " in: "
                            <> T.unpack advice
                        )
                )
                (illegal tool ok)
        )
        [("discover", accepts), ("read_source", acceptsReadSource)]

assertNoBareInventory :: (Text, Value) -> Expectation
assertNoBareInventory (label, args) = do
    v <- runCatArgs (queryOf args) args
    let next = textField "next" v
    when ("inventory" `T.isInfixOf` next && "no query" `T.isInfixOf` next) $
        expectationFailure
            (T.unpack label <> " advises inventory with no query: " <> T.unpack next)

queryOf :: Value -> Text
queryOf (Object o) = case KM.lookup "query" o of
    Just (String s) -> s
    _ -> ""
queryOf _ = ""

{- | Calls that miss in different ways: nothing anywhere, a scope no index
covers, a module fragment, and a topic phrase. Each reaches a different arm of
the miss advice.
-}
probes :: [(Text, Value)]
probes =
    [ ("a name nothing holds", object ["query" .= ("zzznotathing" :: Text)])
    , ("a topic phrase", object ["query" .= ("date arithmetic" :: Text)])
    ,
        ( "a module scope that matches nothing"
        , object
            [ "query" .= ("zzznotathing" :: Text)
            , "module" .= ("Nimbus.Sky" :: Text)
            ]
        )
    ,
        ( "a package scope that matches nothing"
        , object
            [ "query" .= ("zzznotathing" :: Text)
            , "package" .= ("nimbus" :: Text)
            ]
        )
    , ("a bare module scope", object ["module" .= ("Zephyr.Core" :: Text)])
    , ("a bare package scope", object ["package" .= ("zephyr" :: Text)])
    ,
        ( "a scope on an absent-known module"
        , object
            [ "query" .= ("zzznotathing" :: Text)
            , "module" .= ("Data.HodaTime" :: Text)
            ]
        )
    ,
        ( "a scope on an absent-known package"
        , object
            [ "query" .= ("zzznotathing" :: Text)
            , "package" .= ("hodatime" :: Text)
            ]
        )
    ]
