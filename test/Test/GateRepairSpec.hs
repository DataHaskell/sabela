{-# LANGUAGE OverloadedStrings #-}

module Test.GateRepairSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.GateRepair (proofCap, repairCandidates)

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

spec :: Spec
spec = describe "gate-side repair candidates" $ do
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
