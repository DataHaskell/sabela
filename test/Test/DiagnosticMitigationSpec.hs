{-# LANGUAGE OverloadedStrings #-}

{- | G6's pure core: root-cause fold (task 2), the fractional-int generator
(task 1), and the honest disclosure note (task 7). Live cascade behaviour is
"Test.DiagnosticMitigationLiveSpec" and its Compound sibling.
-}
module Test.DiagnosticMitigationSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.Repair.Mitigate (
    MitigationRow (..),
    fractionalIntCandidates,
    mitigationTable,
    rootErrors,
 )
import Sabela.AI.Capabilities.Edit.Repair.Mitigate.Loop (
    MitigationFix (..),
    mitigationDisclosure,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..), bareCellError)

-- | live_test5's four-diagnostic rejection, verbatim in shape: one root
-- cause (the sine cell's @w :: Int@ forcing Fractional\/Floating Int), and
-- two knock-on not-in-scope echoes for names the SAME cell defines.
liveTest5Errors :: [CellError]
liveTest5Errors =
    [ bareCellError (Just 254) (Just 65) "No instance for \8216Fractional Int\8217 arising from a use of \8216/\8217"
    , bareCellError (Just 254) (Just 69) "No instance for \8216Floating Int\8217 arising from a use of \8216pi\8217"
    , bareCellError (Just 258) (Just 24) "Variable not in scope: pathData :: [Char]"
    , bareCellError (Just 262) (Just 34) "Variable not in scope: svg :: String"
    ]

liveTest5Src :: Text
liveTest5Src =
    "w = 400 :: Int\n\
    \h = 200 :: Int\n\
    \pathData = show w\n\
    \svg = pathData"

liveTest5Result :: Either Text ExecutionResult
liveTest5Result = Right (ExecutionResult [] Nothing liveTest5Errors [])

-- | The field-lookup helper for asserting on 'mitigationDisclosure's Value.
field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = describe "G6 diagnostic-class mitigation (pure core)" $ do
    describe "rootErrors — root-cause fold (task 2)" $ do
        it "keeps the Fractional/Floating root cause and drops the pathData/svg knock-ons" $ do
            let roots = map ceMessage (rootErrors liveTest5Src liveTest5Result)
            length roots `shouldBe` 2
            all ("No instance for" `T.isInfixOf`) roots `shouldBe` True
        it "excludes not-in-scope diagnostics naming a name the cell itself defines" $ do
            let roots = rootErrors liveTest5Src liveTest5Result
            any (("pathData" `T.isInfixOf`) . ceMessage) roots `shouldBe` False
            any (("svg" `T.isInfixOf`) . ceMessage) roots `shouldBe` False
        it "a not-in-scope diagnostic for a name the cell does NOT define is a root, not a knock-on" $ do
            let ces = [bareCellError (Just 1) (Just 1) "Variable not in scope: frobnicate"]
                roots = rootErrors "x = 1 :: Int" (Right (ExecutionResult [] Nothing ces []))
            length roots `shouldBe` 1

    describe "fractionalIntCandidates — the fresh generator (task 1)" $ do
        let sineErrs =
                [ bareCellError
                    (Just 2)
                    (Just 1)
                    "No instance for \8216Fractional Int\8217 arising from a use of \8216/\8217"
                ]
            sineSrc = "w = 400 :: Int\nsineY = pi / w"
        it "proposes re-annotating the offending Int binding to Double" $
            fractionalIntCandidates sineSrc sineErrs
                `shouldContain` ["w = 400 :: Double\nsineY = pi / w"]
        it "never proposes the annotation-dropped variant when re-annotation already works" $
            fractionalIntCandidates sineSrc sineErrs
                `shouldNotContain` ["w = 400\nsineY = pi / w"]
        it "is empty when the offending line names no Int/Integer-annotated binding" $
            fractionalIntCandidates
                "sineY = pi / 2"
                [ bareCellError
                    (Just 1)
                    (Just 1)
                    "No instance for \8216Fractional Int\8217 arising from a use of \8216/\8217"
                ]
                `shouldBe` []
        it "ignores a diagnostic that is not a Fractional/Floating no-instance error" $
            fractionalIntCandidates
                sineSrc
                [bareCellError (Just 2) (Just 1) "Variable not in scope: sinX"]
                `shouldBe` []

    describe "the did-you-mean row never fires on live_test4's bare not-in-scope (negative case)" $
        it "detects nothing for a not-in-scope diagnostic GHC gave no suggestion for" $ do
            let pointErr = bareCellError (Just 1) (Just 1) "Variable not in scope: Point"
                mRow = find ((== "did-you-mean") . mitClass) mitigationTable
            fmap (`mitDetect` pointErr) mRow `shouldBe` Just False

    describe "mitigationDisclosure — the honest chain (task 7)" $ do
        let fix1 = MitigationFix "missing-extension" [] ["{-# LANGUAGE LambdaCase #-}"]
            fix2 = MitigationFix "fractional-int" ["w :: Int"] ["w :: Double"]
        it "is Nothing when nothing was ever attempted" $
            mitigationDisclosure [] [] [] `shouldBe` Nothing
        it "reports full success with no remainder when the notebook ends clean" $
            case mitigationDisclosure [fix1, fix2] [] [] of
                Nothing -> expectationFailure "expected a disclosure"
                Just v -> do
                    field "status" v `shouldBe` Just (String "complete")
                    field "resolved" v `shouldBe` Just (Number 2)
        it "names the next diagnostic honestly when the loop stops short" $
            case mitigationDisclosure [fix1] ["Couldn't match type Int with Bool"] [] of
                Nothing -> expectationFailure "expected a disclosure"
                Just v -> do
                    field "status" v `shouldBe` Just (String "partial")
                    case field "note" v of
                        Just (String note) ->
                            note `shouldSatisfy` T.isInfixOf "resolved 1 of 2 diagnostics; 2 remains"
                        _ -> expectationFailure "note field missing"
        it "still discloses a fact list even when nothing was applied" $
            case mitigationDisclosure [] ["still ambiguous"] [String "fact"] of
                Nothing -> expectationFailure "expected a disclosure"
                Just v -> field "resolved" v `shouldBe` Just (Number 0)
