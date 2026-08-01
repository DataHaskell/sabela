{-# LANGUAGE OverloadedStrings #-}

{- | The query reaches the ranker, or the card is ordered by nothing: the
module-browse path handed 'Nothing' to a scorer that then collapsed to input
order. Driven end to end against a session that answers with one listing.
-}
module Test.ModuleCardRankSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec

import Sabela.AI.Capabilities.ModuleSearch (execFindFunction)
import Sabela.AI.Types (ToolOutcome, toolOutcomeValue)
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (setHaskellSession)

spec :: Spec
spec =
    describe "a card ranks against the query that produced it" $
        it "leads with the export that produces the type asked about" $ do
            out <- browseThrough "Widgetry" widgetryListing
            take 1 (exportsOf (toolOutcomeValue out))
                `shouldBe` ["zetaBuild :: Int -> Widgetry"]

{- | A listing whose relevant rows sort last and whose producer and consumer are
the same length, so only relevance can separate them.
-}
widgetryListing :: Text
widgetryListing =
    T.unlines
        [ "alphaHelper :: Int -> Int"
        , "betaHelper :: Int -> Int"
        , "consumeIt :: Widgetry -> Int"
        , "zetaBuild :: Int -> Widgetry"
        ]

exportsOf :: Value -> [Text]
exportsOf v = case v of
    Object o -> case KM.lookup (Key.fromText "exports") o of
        Just (Array es) -> [s | String s <- foldr (:) [] es]
        _ -> []
    _ -> []

-- | Drive @find_function@ against a session that answers with one listing.
browseThrough :: Text -> Text -> IO ToolOutcome
browseThrough modName raw = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    backend <- listingBackend modName raw
    setHaskellSession (appSessions app) (Just backend)
    execFindFunction app (object ["query" .= modName])

listingBackend :: Text -> Text -> IO ST.SessionBackend
listingBackend modName raw = do
    uid <- newUnique
    let backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbJsonDiagnostics = False
                , ST.sbRunBlock = \_ -> pure ("", "")
                , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
                , ST.sbClose = pure ()
                , ST.sbReset = pure backend
                , ST.sbInterrupt = pure ()
                , ST.sbBusy = pure False
                , ST.sbSessionGen = pure 0
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure [modName]
                , ST.sbQueryType = \_ -> pure ""
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \m -> pure (if m == modName then raw else "")
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                , ST.sbEvalPureLive = \req ->
                    pure (ST.pureEvalUnavailableResult req "listing backend")
                }
    pure backend
