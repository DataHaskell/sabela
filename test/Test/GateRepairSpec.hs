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

{- | live_test58's lens cell, verbatim: ONE good hint (RankNTypes) beside a
knock-on rename GHC only offered because the first error broke the block, so
the cell's own `data Server` fell out of scope and `Setter` was the nearest
name in the room. -}
poisonedComposite :: Text
poisonedComposite =
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

spec :: Spec
spec = describe "gate-side repair candidates" $ do
    describe "a knock-on rename never poisons the composite" $ do
        it "does not rename a name the cell itself defines" $ do
            let cs = repairCandidates poisonedComposite definesServer
            concatMap snd cs `shouldNotSatisfy` any (T.isInfixOf "Server -> Setter")

        it "still offers the extension fix that was beside it" $ do
            let cs = repairCandidates poisonedComposite definesServer
            concatMap snd cs `shouldSatisfy` elem "enabled RankNTypes"

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
