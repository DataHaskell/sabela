{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The live proof behind the gate's bind rendering: a real disposable
session compiles a top-level @<-@ bind followed by a declaration that uses
the bound name, the shape the live notebook has always accepted.
-}
module Test.TryBindLiveSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import System.Directory (doesFileExist, findExecutable, makeAbsolute)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Sabela.AI.Capabilities.Try (execTry)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Server (newApp)

spec :: Spec
spec = describe "a bind compiles as the declaration it leaves behind" $
    it "the bound name is in scope for the declaration after it (join-fanout)" $ do
        cabal <- findExecutable "cabal"
        case cabal of
            Nothing -> pendingWith "cabal not found on PATH"
            Just _ ->
                withSystemTempDirectory "sabela-try-bind" $ \dir -> do
                    overlay <- supportOverlay
                    app <- newApp dir Set.empty Nothing Nothing overlay
                    outcome <-
                        bounded
                            ( execTry
                                app
                                ( object
                                    [ "code"
                                        .= ( "x <- pure (1 :: Int)\n\
                                             \y = x + 1" ::
                                                Text
                                           )
                                    ]
                                )
                            )
                    let v = toolOutcomeValue outcome
                    textField "outcome" v `shouldBe` Just "ok"
                    field "executed" v `shouldBe` Just (Bool False)

bounded :: IO a -> IO a
bounded action = do
    result <- timeout 180_000_000 action
    case result of
        Nothing ->
            expectationFailure "try integration timed out" >> error "unreachable"
        Just value -> pure value

supportOverlay :: IO [FilePath]
supportOverlay = do
    present <- doesFileExist ("sabela-notebook" </> "sabela-notebook.cabal")
    if present then (: []) <$> makeAbsolute "sabela-notebook" else pure []

field :: Text -> Value -> Maybe Value
field key (Object obj) = KM.lookup (Key.fromText key) obj
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key v = case field key v of
    Just (String t) -> Just t
    _ -> Nothing
