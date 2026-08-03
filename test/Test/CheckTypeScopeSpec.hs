{-# LANGUAGE OverloadedStrings #-}

{- | The check_type entry point itself, asked with imports it cannot establish.
The seam under test is the call that acquires the scratch scope, so a wiring
that takes the backend and drops the report is observable here.
-}
module Test.CheckTypeScopeSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec
import Test.QuickCheck

import qualified Sabela.SessionTypes as ST

import Sabela.AI.Capabilities.Query.CheckType (checkTypeAnswer)
import Sabela.AI.Capabilities.Scratchpad.Key (scratchpadIdentity)
import Sabela.AI.Store (newAIStore, setScratchpad)
import Sabela.AI.Types (ScratchpadSession (..))
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.State (App (..), newApp, setAIStore)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Test.ScratchScopeSpec (
    genImportFailure,
    genModule,
    genName,
    recordingBackend,
 )

spec :: Spec
spec = describe "check_type answers with imports it could not establish" $ do
    it "reports the import's failure, and never asks about the expression" $
        property $
            forAll ((,) <$> genModule <*> genName) $ \(m, n) ->
                forAll (genImportFailure m) $ \out -> ioProperty $ do
                    (answered, asked) <- askWithImport m n (refusing m out)
                    pure (blames ("import " <> m) answered && null asked)

    it "puts the question when the session refused no scope line" $
        property $
            forAll ((,) <$> genModule <*> genName) $ \(m, n) -> ioProperty $ do
                (answered, asked) <- askWithImport m n (const ("", ""))
                pure (isRight answered && length asked == 1)

-- | A session that refuses exactly one scope line and takes every other.
refusing :: Text -> Text -> Text -> (Text, Text)
refusing m out line
    | ("import " <> m) `T.isInfixOf` line = ("", out)
    | otherwise = ("", "")

{- | A check_type call that needs a scratch scope, answered by a session that
responds as given, plus what that session was asked about the expression.
-}
askWithImport ::
    Text ->
    Text ->
    (Text -> (Text, Text)) ->
    IO (Either A.Value ([Pair], Text), [Text])
askWithImport m n respond = do
    asked <- newIORef []
    backend <- recordingBackend asked respond
    app <- scratchedApp backend
    answered <- checkTypeAnswer app (object ["expr" .= expr, "imports" .= [m]])
    (,) answered <$> readIORef asked
  where
    expr = n <> " " <> n

{- | An app whose scratchpad is already this backend, so establishing a scope
reuses it and no compiler is started.
-}
scratchedApp :: ST.SessionBackend -> IO App
scratchedApp backend = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    store <-
        newAIStore (AnthropicConfig "" "placeholder" "https://api.anthropic.com") mgr
    nb <- readNotebook (appNotebook app)
    let key = snd (scratchpadIdentity ST.Haskell (envGlobalDeps (appEnv app)) nb [])
    setScratchpad store (Just (ScratchpadSession backend "" ST.Haskell key))
    setAIStore app (Just store)
    pure app

isRight :: Either a b -> Bool
isRight = either (const False) (const True)

-- | A refusal that names the scope line and is stamped as a diagnostic.
blames :: Text -> Either A.Value b -> Bool
blames line = either named (const False)
  where
    named v = all (`T.isInfixOf` rendered v) [line, verdictTag VerdictDiagnostic]
    rendered = TE.decodeUtf8 . LBS.toStrict . A.encode
