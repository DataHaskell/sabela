{-# LANGUAGE OverloadedStrings #-}

{- | A failed import makes every use of its names a second error. Only the
import failure is the candidate's defect; the knock-ons send a reader (or a
repair loop) chasing a scope problem that does not exist.
-}
module Test.KnockOnSpec (knockOnSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Diagnose.KnockOn (dropImportKnockOns)

splitSrc :: Text
splitSrc =
    "import Data.List.Split (splitOn)\n\
    \rows = map (splitOn \",\") ls"

notFoundBlob :: Text
notFoundBlob =
    "<no location info>: error:\n\
    \    Could not find module \8216Data.List.Split\8217\n\
    \    It is not a module in the current program, or in any known package.\n\
    \\n\
    \<interactive>:792:19: error: [GHC-88464]\n\
    \    Variable not in scope: splitOn :: t0 -> String -> [a0]\n\
    \    Suggested fix: Perhaps use \8216splitAt\8217 (imported from Prelude)"

knockOnSpec :: Spec
knockOnSpec = describe "knock-ons behind a failed import are dropped" $ do
    it "keeps the module error and drops the not-in-scope it caused" $ do
        let scrubbed = dropImportKnockOns splitSrc notFoundBlob
        scrubbed `shouldSatisfy` T.isInfixOf "Could not find module"
        scrubbed `shouldSatisfy` (not . T.isInfixOf "Variable not in scope")

    it "keeps a not-in-scope no failed import provides" $ do
        let blob =
                notFoundBlob
                    <> "\n\n<interactive>:800:1: error: [GHC-88464]\n\
                       \    Variable not in scope: unrelatedName"
            scrubbed = dropImportKnockOns splitSrc blob
        scrubbed `shouldSatisfy` T.isInfixOf "unrelatedName"
        scrubbed `shouldSatisfy` (not . T.isInfixOf "splitOn ::")

    it "touches nothing when no import failed" $ do
        let blob =
                "<interactive>:5:1: error: [GHC-88464]\n\
                \    Variable not in scope: zoop"
        dropImportKnockOns splitSrc blob `shouldBe` blob

    it "covers a hidden-package load failure and an operator import" $ do
        let src =
                "import System.FilePath ((</>))\n\
                \p = \"a\" </> \"b\""
            blob =
                "<no location info>: error:\n\
                \    Could not load module \8216System.FilePath\8217\n\
                \    It is a member of the hidden package \8216filepath-1.4\8217.\n\
                \\n\
                \<interactive>:794:24: error: [GHC-88464]\n\
                \    Variable not in scope: (</>) :: FilePath -> t0 -> FilePath"
            scrubbed = dropImportKnockOns src blob
        scrubbed `shouldSatisfy` T.isInfixOf "hidden package"
        scrubbed `shouldSatisfy` (not . T.isInfixOf "not in scope")

    it "covers a qualified alias of the failed module" $ do
        let src = "import qualified Data.List.Split as S\nrows = S.splitOn \",\""
            blob =
                "<no location info>: error:\n\
                \    Could not find module \8216Data.List.Split\8217\n\
                \\n\
                \<interactive>:10:8: error: [GHC-88464]\n\
                \    Not in scope: \8216S.splitOn\8217"
            scrubbed = dropImportKnockOns src blob
        scrubbed `shouldSatisfy` (not . T.isInfixOf "S.splitOn")

    it "leaves a bare import's unrelated scope errors alone" $ do
        let src = "import Data.List.Split\nrows = someTypo ls"
            blob =
                "<no location info>: error:\n\
                \    Could not find module \8216Data.List.Split\8217\n\
                \\n\
                \<interactive>:10:8: error: [GHC-88464]\n\
                \    Variable not in scope: someTypo"
            scrubbed = dropImportKnockOns src blob
        scrubbed `shouldSatisfy` T.isInfixOf "someTypo"
