{-# LANGUAGE OverloadedStrings #-}

{- | On the stdio MCP server stdout IS the JSON-RPC channel, so anything the
vetting path may reach must be able to run without writing to it.
-}
module Test.VetSilenceSpec (vetSilenceSpec) where

import Control.Exception (finally)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hFlush, openTempFile, stdout)
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check.Vet (
    emptyScope,
    typedScope,
    vetProposalAgainst,
    vetVerdictAgainst,
 )

vetSilenceSpec :: Spec
vetSilenceSpec = describe "vetting a proposed check" $ do
    it "reaches its verdict without writing to stdout" $ do
        (out, verdict) <-
            captureStdout (vetVerdictAgainst uncheckable emptyScope "total == 42")
        verdict `shouldBe` Left "does not compile"
        out `shouldBe` ""
    it "keeps a good check, still silently" $ do
        (out, verdict) <-
            captureStdout
                (vetVerdictAgainst passing (typedScope [("total", "Int")]) "total == 42")
        verdict `shouldBe` Right "total == 42"
        out `shouldBe` ""
    it "refuses a check with nothing in scope to be about, still silently" $ do
        (out, verdict) <-
            captureStdout (vetVerdictAgainst passing emptyScope "total == 42")
        verdict `shouldSatisfy` either (const True) (const False)
        out `shouldBe` ""
    it "still narrates the discard on the CLI path" $ do
        (out, kept) <-
            captureStdout (vetProposalAgainst uncheckable emptyScope "total == 42")
        kept `shouldBe` ""
        out `shouldSatisfy` T.isInfixOf "discarded a check"

{- | `try` declines what it cannot evaluate, which is what an uncheckable
check looks like on the wire.
-}
uncheckable :: ToolName -> Value -> IO (Either Text ToolOutcome)
uncheckable _ _ =
    pure
        ( Right
            ( ToolErr
                ( object
                    [ "verdict" .= ("could-not-run" :: Text)
                    , "reason" .= ("Variable not in scope: total" :: Text)
                    ]
                )
            )
        )

{- | A kernel whose check holds and whose perturbations do not — what a check
that can actually fail looks like on the wire.
-}
passing :: ToolName -> Value -> IO (Either Text ToolOutcome)
passing _ args =
    pure
        ( Right
            ( ToolOk
                ( object
                    [ "type" .= ("Bool" :: Text)
                    , "stdout" .= holds (argText "code" args)
                    ]
                )
            )
        )
  where
    holds code = if code == "total == 42" then "True" else "False" :: Text

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (Key.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""

captureStdout :: IO a -> IO (Text, a)
captureStdout act = do
    tmp <- getTemporaryDirectory
    (path, h) <- openTempFile tmp "siza-vet-stdout.txt"
    saved <- hDuplicate stdout
    hDuplicateTo h stdout
    r <- act `finally` restore saved h
    captured <- TIO.readFile path
    removeFile path
    pure (captured, r)
  where
    restore saved h = do
        hFlush stdout
        hDuplicateTo saved stdout
        hClose saved
        hClose h
