{-# LANGUAGE OverloadedStrings #-}

{- | R6.7: the scratchpad never returns silence. For any snippet outcome the
response carries output, a diagnostic, or a typed statement of what the
scratchpad cannot do (isolation, own cabal line) with the in-session
alternative — empty-and-empty is unrepresentable.
-}
module Test.ScratchpadSilenceSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Scratchpad (isolationDiagnostic, silentDiagnostic)

-- | The observable outcome classes a snippet can produce (R10 test method).
outcomeCases :: [(String, Text, Text)]
outcomeCases =
    [ ("unresolvable import", "", "error: Could not load module 'DataFrame'")
    , ("runtime error", "", "*** Exception: Prelude.head: empty list")
    , ("pure value with print", "42\n", "")
    , ("pure binding, no print", "", "")
    , ("empty snippet", "", "")
    , ("whitespace-only output", "  \n", "  ")
    ]

spec :: Spec
spec = describe "Scratchpad silence guard (R6.7)" $ do
    it "never leaves stdout, stderr and diagnostic all empty" $
        mapM_
            ( \(label, out, err) -> do
                let d = silentDiagnostic out err ([] :: [()])
                    silent =
                        T.null (T.strip out)
                            && T.null (T.strip err)
                            && isNothing d
                (label, silent) `shouldBe` (label, False)
            )
            outcomeCases
    it "adds the diagnostic ONLY when the response would otherwise be silent" $
        mapM_
            ( \(label, out, err) -> do
                let expectDiag = T.null (T.strip out) && T.null (T.strip err)
                (label, isJust (silentDiagnostic out err ([] :: [()])))
                    `shouldBe` (label, expectDiag)
            )
            outcomeCases
    it "defers to guidance when the diagnose pass produced some" $
        silentDiagnostic "" "" ["guidance" :: Text] `shouldBe` Nothing
    it "types the isolation: no session bindings, own cabal line, alternative" $ do
        isolationDiagnostic `shouldSatisfy` T.isInfixOf "ISOLATED"
        isolationDiagnostic `shouldSatisfy` T.isInfixOf "cabal"
        isolationDiagnostic `shouldSatisfy` T.isInfixOf "list_bindings"
        isolationDiagnostic `shouldSatisfy` T.isInfixOf "insert a cell"
