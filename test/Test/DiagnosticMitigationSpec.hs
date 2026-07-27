{-# LANGUAGE OverloadedStrings #-}

module Test.DiagnosticMitigationSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.Repair.Mitigate (
    Discharge (..),
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

liveTest5Errors :: [CellError]
liveTest5Errors =
    [ bareCellError
        (Just 254)
        (Just 65)
        "No instance for \8216Fractional Int\8217 arising from a use of \8216/\8217"
    , bareCellError
        (Just 254)
        (Just 69)
        "No instance for \8216Floating Int\8217 arising from a use of \8216pi\8217"
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

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

liveTest8Errors :: [CellError]
liveTest8Errors =
    [ bareCellError
        (Just 3)
        (Just 1)
        "Could not load module \8216Data.Text\8217. \
        \It is a member of the hidden package \8216text-2.1.2\8217."
    , bareCellError (Just 7) (Just 12) "Variable not in scope: T.pack"
    , bareCellError (Just 9) (Just 20) "Variable not in scope: T.unpack"
    ]

liveTest8Src :: Text
liveTest8Src = "import Data.Text as T\n\nsineWaveSvg = T.pack \"<svg/>\""

spec :: Spec
spec = describe "G6 diagnostic-class mitigation (pure core)" $ do
    describe "unshowable-display (live_test28)" $ do
        let showErr =
                bareCellError
                    (Just 2)
                    (Just 1)
                    "No instance for `Show Picture' arising from a use of `print'"
            src = "import Sabela.Notebook\nplot [(x, sin x) | x <- [0,0.1..2*pi]]"
            row = find ((== "unshowable-display") . mitClass) mitigationTable

        it "is a table row that APPLIES its fix" $
            fmap mitDischarge row `shouldBe` Just Apply

        it "detects an unshowable displayable, and nothing else" $ do
            fmap (`mitDetect` showErr) row `shouldBe` Just True
            fmap
                (`mitDetect` bareCellError (Just 1) (Just 1) "No instance for `Show Foo'")
                row
                `shouldBe` Just False

        it "wraps the final expression in its display function" $ case row of
            Nothing -> expectationFailure "no unshowable-display row"
            Just r -> do
                cands <-
                    mitGenerate
                        r
                        (error "generator must not touch App")
                        (error "generator must not touch AIStore")
                        (Right (ExecutionResult [] Nothing [showErr] []))
                        src
                case cands of
                    (c : _) -> do
                        c `shouldSatisfy` T.isInfixOf "displayPicture ("
                        c `shouldSatisfy` T.isInfixOf "import Sabela.Notebook"
                    [] -> expectationFailure "expected a wrapped candidate"

        it "comprehension-not-bind: a comprehension arrow is not a bind" $ case row of
            Nothing -> expectationFailure "no unshowable-display row"
            Just r -> do
                cands <-
                    mitGenerate
                        r
                        (error "unused")
                        (error "unused")
                        (Right (ExecutionResult [] Nothing [showErr] []))
                        "plot [(x, sin x) | x <- [0,0.1..2*pi]]"
                cands `shouldSatisfy` (not . null)

        it "never wraps a binding or an import" $ case row of
            Nothing -> expectationFailure "no unshowable-display row"
            Just r -> do
                cands <-
                    mitGenerate
                        r
                        (error "unused")
                        (error "unused")
                        (Right (ExecutionResult [] Nothing [showErr] []))
                        "pic = plot []"
                cands `shouldBe` []

    describe "hidden-package-text (task 1's sixth row, live_test8)" $ do
        it "is a table row, so the class is covered at all" $
            fmap mitClass (find ((== "installed-not-loaded") . mitClass) mitigationTable)
                `shouldBe` Just "installed-not-loaded"

        it "serves the fix as an artifact and NEVER applies it" $
            fmap mitDischarge (find ((== "installed-not-loaded") . mitClass) mitigationTable)
                `shouldBe` Just ServeAsArtifact

        it "detects GHC's hidden-package wording, and nothing else" $ do
            let row = find ((== "installed-not-loaded") . mitClass) mitigationTable
            fmap (\r -> map (mitDetect r) liveTest8Errors) row
                `shouldBe` Just [True, False, False]

        it "generates the committable source, cabal line first" $ do
            let row = find ((== "installed-not-loaded") . mitClass) mitigationTable
            case row of
                Nothing -> expectationFailure "no hidden-package row"
                Just r -> do
                    cands <-
                        mitGenerate
                            r
                            (error "generator must not touch App")
                            (error "generator must not touch AIStore")
                            (Right (ExecutionResult [] Nothing liveTest8Errors []))
                            liveTest8Src
                    case cands of
                        (c : _) -> do
                            head (T.lines c)
                                `shouldBe` "-- cabal: build-depends: text"
                            c `shouldSatisfy` T.isInfixOf "sineWaveSvg"
                        [] -> expectationFailure "expected a committable candidate"
    describe "rootErrors — root-cause fold (task 2)" $ do
        it
            "keeps the Fractional/Floating root cause and drops the pathData/svg knock-ons"
            $ do
                let roots = map ceMessage (rootErrors liveTest5Src liveTest5Result)
                length roots `shouldBe` 2
                all ("No instance for" `T.isInfixOf`) roots `shouldBe` True
        it "excludes not-in-scope diagnostics naming a name the cell itself defines" $ do
            let roots = rootErrors liveTest5Src liveTest5Result
            any (("pathData" `T.isInfixOf`) . ceMessage) roots `shouldBe` False
            any (("svg" `T.isInfixOf`) . ceMessage) roots `shouldBe` False
        it
            "a not-in-scope diagnostic for a name the cell does NOT define is a root, not a knock-on"
            $ do
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
        it
            "never proposes the annotation-dropped variant when re-annotation already works"
            $ fractionalIntCandidates sineSrc sineErrs
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

    describe
        "the did-you-mean row never fires on live_test4's bare not-in-scope (negative case)"
        $ it "detects nothing for a not-in-scope diagnostic GHC gave no suggestion for"
        $ do
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
