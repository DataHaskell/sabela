{-# LANGUAGE OverloadedStrings #-}

module Test.HintsSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Hints (
    Hint (..),
    RenameCandidate (..),
    expectedTypeOf,
    parseHints,
 )

multiKind :: Text
multiKind =
    "<interactive>:271:14: error: [GHC-88464]\n\
    \    Variable not in scope: datacenters :: b4 -> c0\n\
    \    Suggested fix:\n\
    \      Perhaps use one of these:\n\
    \        record field of Organization \8216_datacenters\8217 (line 256),\n\
    \        data constructor \8216Datacenter\8217 (line 248)"

singleImport :: Text
singleImport =
    "<interactive>:212:164: error: [GHC-88464]\n\
    \    Variable not in scope: length'\n\
    \    Suggested fix:\n\
    \      Perhaps use \8216V.length\8217 (imported from Data.Vector.Unboxed)"

operators :: Text
operators =
    "<interactive>:271:47: error: [GHC-88464]\n\
    \    Variable not in scope: (^.) :: t0 -> Bool -> t1\n\
    \    Suggested fix:\n\
    \      Perhaps use one of these:\n\
    \        \8216.\8217 (imported from Prelude), \8216^\8217 (imported from Prelude),\n\
    \        \8216^^\8217 (imported from Prelude)"

extension :: Text
extension =
    "<interactive>:1:1: error: [GHC-91510]\n\
    \    \8226 Illegal polymorphic type\n\
    \    Suggested fix:\n\
    \      Perhaps you intended to use the \8216RankNTypes\8217 extension (implied by \8216ImpredicativeTypes\8217)\n\
    \      You may enable this language extension in GHCi with:"

declPlacement :: Text
declPlacement =
    "<interactive>:550:1: error: [GHC-44432]\n\
    \    The type signature for \8216orgName\8217 lacks an accompanying binding\n\
    \    Suggested fix:\n\
    \      Move the type signature to the declaration site of \8216orgName\8217."

mismatch :: Text
mismatch =
    "<interactive>:270:28: error: [GHC-83865]\n\
    \    \8226 Couldn't match type \8216Datacenter\8217 with \8216[Datacenter]\8217\n\
    \      Expected: (Datacenter -> Identity Datacenter)\n\
    \                -> [Datacenter] -> Identity [Datacenter]\n\
    \        Actual: Optic' (->) Identity Datacenter Datacenter\n\
    \    \8226 In the first argument of \8216(.)\8217"

inlineHint :: Text
inlineHint = "Perhaps use `filter' (imported from Prelude)"

spec :: Spec
spec = describe "Sabela.AI.Hints" $ do
    describe "rename hints" $ do
        it "pairs the wrong name with GHC's candidates, in GHC's order" $
            parseHints multiKind
                `shouldBe` [ HintRename
                                "datacenters"
                                [ RenameCandidate "_datacenters" "record-field" "line 256"
                                , RenameCandidate "Datacenter" "data-constructor" "line 248"
                                ]
                           ]

        it "keeps the kind, so a fix knows a field is not a function" $ do
            let [HintRename _ cs] = parseHints multiKind
            map rcKind cs `shouldBe` ["record-field", "data-constructor"]

        it "keeps import provenance, which carries the import a fix may need" $
            parseHints singleImport
                `shouldBe` [ HintRename
                                "length'"
                                [RenameCandidate "V.length" "value" "imported from Data.Vector.Unboxed"]
                           ]

        it "reads an operator wrong-name and its operator candidates" $ do
            let [HintRename w cs] = parseHints operators
            w `shouldBe` "(^.)"
            map rcName cs `shouldBe` [".", "^", "^^"]

        it
            "the block scan stops at the block: a later diagnostic's tokens never leak in"
            $ do
                let two = multiKind <> "\n\n" <> mismatch
                    [HintRename _ cs] = [h | h@HintRename{} <- parseHints two]
                map rcName cs `shouldBe` ["_datacenters", "Datacenter"]

    describe "extension hints" $ do
        it "names the extension past the (implied by ...) parenthetical" $
            parseHints extension `shouldBe` [HintExtension "RankNTypes"]

        it "an unknown extension name falls through rather than risking a pragma" $
            parseHints
                "Suggested fix:\n  Perhaps you intended to use the \8216ZzzBogus\8217 extension"
                `shouldNotSatisfy` any isExtension

    describe "other structured forms" $ do
        it "reads declaration placement" $
            parseHints declPlacement `shouldBe` [HintDeclPlacement "orgName"]

        it "the inline (JSON hints) phrasing parses like the block form" $ do
            let [HintRename _ cs] = parseHints inlineHint
            map rcName cs `shouldBe` ["filter"]

        it "no fix block, no hints" $
            parseHints "Couldn't match type \8216Int\8217 with \8216Bool\8217" `shouldBe` []

    describe "expectedTypeOf" $ do
        it "joins the Expected: block across its continuation lines" $
            expectedTypeOf mismatch
                `shouldBe` Just
                    "(Datacenter -> Identity Datacenter) -> [Datacenter] -> Identity [Datacenter]"

        it "stops before Actual:" $
            expectedTypeOf mismatch
                `shouldNotSatisfy` maybe False ("Optic'" `T.isInfixOf`)

        it "is Nothing when no mismatch block exists" $
            expectedTypeOf multiKind `shouldBe` Nothing
  where
    isExtension h = case h of HintExtension _ -> True; _ -> False
