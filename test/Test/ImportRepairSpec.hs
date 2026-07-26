{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of the server-side wrong-module repair: read GHC's
"Perhaps you meant Y" correction and rename the module as a whole dotted token.
The IO loop that re-runs the cell is verified live; these pin the decisions it makes.
-}
module Test.ImportRepairSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.ImportRepair (
    addQualifiedImport,
    addScopedImport,
    moduleRenameFix,
    qualifiedAliases,
    renameModule,
    unboundAliasUses,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Test.Hspec

-- | The real GHC wrong-module wall (multiple version suggestions, same module).
moduleErr :: Text
moduleErr =
    T.unlines
        [ "Could not find module `Data.Frame'."
        , "Perhaps you meant"
        , "  DataFrame (needs flag -package-id dataframe-0.7.0.0)"
        , "  DataFrame (needs flag -package-id dataframe-0.3.3.7)"
        ]

raising :: Text -> Either Text ExecutionResult
raising msg = Right (ExecutionResult [] (Just msg) [] [])

spec :: Spec
spec = describe "Sabela.AI.ImportRepair" $ do
    describe "renameModule" $ do
        it "renames a module as a whole dotted token, leaving qualified uses intact" $
            renameModule
                "Data.Frame"
                "DataFrame"
                "import qualified Data.Frame as DF\nx = DF.readCsv"
                `shouldBe` "import qualified DataFrame as DF\nx = DF.readCsv"

        it "leaves a longer module sharing the prefix alone" $
            renameModule "Data.Frame" "DataFrame" "import Data.Frame.TH.Records"
                `shouldBe` "import Data.Frame.TH.Records"

    describe "moduleRenameFix" $ do
        it "renames from GHC's perhaps-you-meant suggestion" $
            moduleRenameFix (raising moduleErr) "import Data.Frame\nx = 1"
                `shouldBe` Just "import DataFrame\nx = 1"

        it "is Nothing when the failure names no correctable module" $
            moduleRenameFix (raising "Couldn't match Int with Bool") "import Data.Frame"
                `shouldBe` Nothing

        it "is Nothing on an abort (Left)" $
            moduleRenameFix (Left "Cancelled") "import Data.Frame" `shouldBe` Nothing

    describe "addScopedImport" $ do
        it "inserts a scoped import naming just the symbol" $
            addScopedImport "Conduit" "runConduit" "import Data.Text\nx = 1"
                `shouldBe` "import Data.Text\nimport Conduit (runConduit)\nx = 1"

        it "wraps an operator name in parens" $
            addScopedImport "Text.Regex.TDFA" "=~" "x = 1"
                `shouldBe` "import Text.Regex.TDFA ((=~))\nx = 1"

        it "is idempotent across repeated calls" $
            addScopedImport
                "Conduit"
                "runConduit"
                (addScopedImport "Conduit" "runConduit" "import Data.Text\nx = 1")
                `shouldBe` addScopedImport "Conduit" "runConduit" "import Data.Text\nx = 1"

        it "inserts after the -- cabal: line when the cell has no imports" $
            addScopedImport "Conduit" "runConduit" "-- cabal: build-depends: conduit\nx = 1"
                `shouldBe` "-- cabal: build-depends: conduit\nimport Conduit (runConduit)\nx = 1"

    {- live_test35_wine, GHC-76037: the cell used `T.lines` and `TE.decodeUtf8`
    having never bound either alias. GHC names the unimported alias itself, so
    the repair never has to guess that T means Data.Text. -}
    describe "unboundAliasUses" $ do
        it "pairs the alias with the bare name from GHC's two-line form" $
            unboundAliasUses ghcAliasError
                `shouldBe` [("T", "lines"), ("TE", "decodeUtf8")]

        it "ignores a qualified name whose alias GHC does not call unimported" $
            unboundAliasUses
                "Not in scope: \8216M.foo\8217"
                `shouldBe` []

        it "ignores an unqualified not-in-scope name" $
            unboundAliasUses
                "Not in scope: \8216foo\8217\nNote: No module named \8216T\8217 is imported."
                `shouldBe` []

        it "is empty for an unrelated diagnostic" $
            unboundAliasUses "Couldn't match type \8216Int\8217 with \8216Bool\8217"
                `shouldBe` []

    describe "addQualifiedImport" $ do
        it "binds the alias GHC said was missing" $
            addQualifiedImport "Data.Text" "T" "x = 1"
                `shouldBe` "import qualified Data.Text as T\nx = 1"

        it "is a no-op when the alias is already bound" $
            addQualifiedImport
                "Data.Text"
                "T"
                "import qualified Data.Text as T\nx = 1"
                `shouldBe` "import qualified Data.Text as T\nx = 1"

        it "reads the aliases a source already binds" $
            qualifiedAliases
                "import qualified Data.Text as T\nimport qualified Data.Map as M\nx = 1"
                `shouldBe` ["T", "M"]

-- | The diagnostic exactly as live_test35_wine received it.
ghcAliasError :: Text
ghcAliasError =
    "<interactive>:365:20: error: [GHC-76037]\n\
    \    Not in scope: \8216T.lines\8217\n\
    \    Note: No module named \8216T\8217 is imported.\n\
    \<interactive>:365:27: error: [GHC-76037]\n\
    \    Not in scope: \8216TE.decodeUtf8\8217\n\
    \    Note: No module named \8216TE\8217 is imported."
