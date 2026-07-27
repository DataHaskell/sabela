{-# LANGUAGE OverloadedStrings #-}

module Test.GateRepairSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.GateRepair (
    aliasImportCandidates,
    proofCap,
    repairCandidates,
 )

twoRenames :: Text
twoRenames =
    "<interactive>:271:28: error: [GHC-88464]\n\
    \    Variable not in scope: filtered\n\
    \    Suggested fix: Perhaps use \8216filter\8217 (imported from Prelude)\n\
    \<interactive>:271:49: error: [GHC-88464]\n\
    \    Variable not in scope: location\n\
    \    Suggested fix:\n\
    \      Perhaps use record field of Datacenter \8216_location\8217 (line 248)"

multiCandidate :: Text
multiCandidate =
    "<interactive>:271:14: error: [GHC-88464]\n\
    \    Variable not in scope: datacenters\n\
    \    Suggested fix:\n\
    \      Perhaps use one of these:\n\
    \        record field of Organization \8216_datacenters\8217 (line 256),\n\
    \        data constructor \8216Datacenter\8217 (line 248)"

extensionDiag :: Text
extensionDiag =
    "<interactive>:1:1: error: [GHC-91510]\n\
    \    Illegal polymorphic type\n\
    \    Suggested fix:\n\
    \      Perhaps you intended to use the \8216RankNTypes\8217 extension"

mixedQuality :: Text
mixedQuality =
    "<interactive>:241:16: error: [GHC-91510]\n\
    \    Illegal polymorphic type:\n\
    \    Suggested fix:\n\
    \      Perhaps you intended to use the \8216RankNTypes\8217 extension\n\
    \<interactive>:253:24: error: [GHC-76037]\n\
    \    Not in scope: type constructor or class \8216Server\8217\n\
    \    Suggested fix:\n\
    \      Perhaps use one of these:\n\
    \        \8216Setter\8217 (imported from Control.Lens)"

definesServer :: Text
definesServer =
    "import Control.Lens\n\
    \data Server = Server { _name :: String }\n\
    \declareLens :: (s -> a) -> Lens s t a b\n"

qualifiedRename :: Text
qualifiedRename =
    "<interactive>:245:13: error: [GHC-76037]\n\
    \    Not in scope: \8216T.putStrLn\8217\n\
    \    Suggested fix:\n\
    \      Perhaps use \8216LBS.putStrLn\8217 (imported from Data.ByteString.Lazy.Char8)"

dataFrameClash :: Text
dataFrameClash =
    "<interactive>:211:164: error: [GHC-87543]\n\
    \    Ambiguous occurrence \8216null\8217.\n\
    \    It could refer to\n\
    \       either \8216DataFrame.null\8217,\n\
    \              imported from \8216DataFrame\8217\n\
    \           or \8216Prelude.null\8217,\n\
    \              imported from \8216Prelude\8217\n\
    \<interactive>:222:16: error: [GHC-87543]\n\
    \    Ambiguous occurrence \8216filter\8217.\n\
    \    It could refer to\n\
    \       either \8216DataFrame.filter\8217,\n\
    \              imported from \8216DataFrame\8217\n\
    \           or \8216Prelude.filter\8217,\n\
    \              imported from \8216Prelude\8217"

dataFrameSrc :: Text
dataFrameSrc =
    "import DataFrame\n\
    \go df = if null df then 0 else length (filter id df)\n"

spec :: Spec
spec = describe "gate-side repair candidates" $ do
    describe "a diagnostic with more than one ambiguous occurrence" $ do
        it "resolves every clash in one composite, not just the first" $ do
            let (c, _) : _ = repairCandidates dataFrameClash dataFrameSrc
            c `shouldSatisfy` T.isInfixOf "DataFrame.null df"
            c `shouldSatisfy` T.isInfixOf "DataFrame.filter id df"

        it "discloses both qualifications, named as ordinary substitutions" $ do
            let (_, fixes) : _ = repairCandidates dataFrameClash dataFrameSrc
            fixes `shouldBe` ["null -> DataFrame.null", "filter -> DataFrame.filter"]

        it "never substitutes a clash's own candidate for another clash's name" $ do
            let composites = T.concat (map fst (repairCandidates dataFrameClash dataFrameSrc))
            -- "null df" only ever becomes a *.null qualification, never a
            -- \*.filter one bleeding in from the second clash's candidate list.
            composites `shouldNotSatisfy` T.isInfixOf "DataFrame.filter df"
            composites `shouldNotSatisfy` T.isInfixOf "Prelude.filter df"

    describe "an alias bound to the wrong module is repaired by import" $ do
        it "binds the resolved module under the alias the cell already uses" $ do
            let src = "import qualified Data.Text as T\nmain = T.putStrLn (toCsv df)"
                cs = aliasImportCandidates "T" ["Data.Text.IO"] src
            map fst cs
                `shouldBe` [ "import qualified Data.Text as T\n\
                             \import qualified Data.Text.IO as T\n\
                             \main = T.putStrLn (toCsv df)"
                           ]
            map snd cs `shouldBe` [["imported Data.Text.IO as T"]]

        it "leaves the call site alone — the alias is already what the model wrote" $ do
            let src = "import qualified Data.Text as T\nmain = T.putStrLn x"
                (c : _) = map fst (aliasImportCandidates "T" ["Data.Text.IO"] src)
            c `shouldSatisfy` T.isInfixOf "T.putStrLn x"

        it "yields nothing when the module is already bound to that alias" $
            aliasImportCandidates "T" ["Data.Text"] "import qualified Data.Text as T\nx = 1"
                `shouldBe` []

    it "declares every hidden package GHC named, not just the first" $ do
        let diag =
                "<no location info>: error: [GHC-87110]\n\
                \    It is a member of the hidden package \8216bytestring-0.12.2.0\8217.\n\
                \<no location info>: error: [GHC-87110]\n\
                \    It is a member of the hidden package \8216text-2.1.2\8217."
            src = "import qualified Data.Text as T\nmain = T.putStrLn \"x\""
            ((c, fixes) : _) = repairCandidates diag src
        c `shouldSatisfy` T.isInfixOf "bytestring"
        c `shouldSatisfy` T.isInfixOf "text"
        fixes
            `shouldBe` [ "declared build-depends: bytestring"
                       , "declared build-depends: text"
                       ]

    it "applies a rename whose names are module-qualified" $ do
        let src = "import qualified Data.Text as T\nmain = T.putStrLn (toCsv df)"
            cs = repairCandidates qualifiedRename src
        map fst cs
            `shouldSatisfy` any (T.isInfixOf "LBS.putStrLn (toCsv df)")

    describe "one doubtful hint sinks only the candidates carrying it" $ do
        it "proves the extension alone first, dropping the knock-on rename" $ do
            let (_, fixes) : _ = repairCandidates mixedQuality definesServer
            fixes `shouldBe` ["enabled RankNTypes"]

        it "still offers the rename as a fallback rather than ruling it out" $ do
            let cs = repairCandidates mixedQuality definesServer
            concatMap snd cs `shouldSatisfy` any (T.isInfixOf "Server -> Setter")

        it "keeps a rename whose subject the cell does not define" $ do
            let (_, fixes) : _ = repairCandidates mixedQuality "x = Server 1"
            fixes `shouldSatisfy` any (T.isInfixOf "Server -> Setter")

    it "fixes every rename hint in ONE composite, not one at a time" $ do
        let src = "xs = filtered odd [1,2,3]\nloc = location dc"
            ((c, fixes) : _) = repairCandidates twoRenames src
        c `shouldSatisfy` T.isInfixOf "filter odd"
        c `shouldSatisfy` T.isInfixOf "_location dc"
        length fixes `shouldBe` 2

    it "names each applied fix with its provenance, for the disclosure" $ do
        let ((_, fixes) : _) = repairCandidates twoRenames "xs = filtered odd xs2\nl = location d"
        fixes
            `shouldBe` [ "filtered -> filter (imported from Prelude)"
                       , "location -> _location (line 248)"
                       ]

    it "a multi-candidate hint yields a composite per candidate, GHC's order first" $ do
        let src = "d = datacenters org"
            cs = map fst (repairCandidates multiCandidate src)
        take 2 cs
            `shouldBe` [ "d = _datacenters org"
                       , "d = Datacenter org"
                       ]

    it "applies an extension hint as a pragma" $ do
        let ((c, fixes) : _) = repairCandidates extensionDiag "f :: (forall a. a) -> Int\nf _ = 1"
        c `shouldSatisfy` T.isInfixOf "RankNTypes"
        fixes `shouldBe` ["enabled RankNTypes"]

    it "never touches an import line" $ do
        let src = "import Data.List (filtered)\nxs = filtered odd ys"
            ((c, _) : _) = repairCandidates twoRenames src
        c `shouldSatisfy` T.isInfixOf "import Data.List (filtered)"
        c `shouldSatisfy` T.isInfixOf "filter odd ys"

    it "no hints, no candidates" $
        repairCandidates "Couldn't match type \8216Int\8217 with \8216Bool\8217" "x = 1"
            `shouldBe` []

    it "a hint whose substitution is a no-op yields nothing rather than a fake fix" $
        repairCandidates twoRenames "y = somethingElse" `shouldBe` []

    it "is bounded by proofCap" $
        length (repairCandidates multiCandidate "d = datacenters org")
            `shouldSatisfy` (<= proofCap)
