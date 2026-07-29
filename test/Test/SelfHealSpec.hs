{-# LANGUAGE OverloadedStrings #-}

module Test.SelfHealSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.SelfHeal (
    attachSelfHeal,
    attachSelfHealSuggestions,
    contrastLine,
    dependencySuggestionNote,
    plausibleRename,
    selfHealNote,
    sourceDelta,
 )

gemmaErr :: Text
gemmaErr =
    T.unlines
        [ "cell 0, line 24: Variable not in scope:"
        , "  takeWhile1"
        , "    :: (Char -> Bool)"
        , "       -> ParsecT"
        , "            Void"
        , "            String"
        , "            ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity"
        , "            String"
        , "Perhaps use one of these:"
        , "  `takeWhile' (imported from Prelude),"
        , "  `takeWhileP' (imported from Text.Megaparsec),"
        , "  `takeWhile_' (imported from Text.Megaparsec)"
        ]

unshowableErr :: Text
unshowableErr =
    "cell 7, line 15: No instance for `Show\n\
    \                   sabela-notebook-0.2.0.0:Sabela.Notebook.Picture.Internal.Picture'\n\
    \  arising from a use of `print'"

spec :: Spec
spec = describe "loud + conservative self-heal (intention)" $ do
    describe "plausibleRename — decline a lexically distant substitution" $ do
        it "accepts a near rename (takeWhile1 -> takeWhileP)" $
            plausibleRename "takeWhile1" "takeWhileP" `shouldBe` True
        it "accepts a same-stem rename (foldl -> foldl')" $
            plausibleRename "foldl" "foldl'" `shouldBe` True
        it "declines a distant constant (customers -> mempty)" $
            plausibleRename "customers" "mempty" `shouldBe` False
        it "declines a distant constant (orders -> minBound)" $
            plausibleRename "orders" "minBound" `shouldBe` False

    describe "contrastLine — wrong-vs-real on the re-enter rail" $ do
        it "names the wrong name and the real candidates from the real error" $
            case contrastLine gemmaErr of
                Nothing -> expectationFailure "no contrast produced"
                Just line -> do
                    line `shouldSatisfy` T.isInfixOf "takeWhile1"
                    line `shouldSatisfy` T.isInfixOf "takeWhileP"
                    line `shouldSatisfy` T.isInfixOf "check_type"
        it "is Nothing when the error carries no did-you-mean" $
            contrastLine "cell 0, line 2: Variable not in scope: frobnicate"
                `shouldBe` Nothing
        it "unescapes a JSON-escaped diagnostic (no literal \\n in the name)" $
            case contrastLine (T.replace "\n" "\\n" gemmaErr) of
                Nothing -> expectationFailure "no contrast from escaped blob"
                Just line -> do
                    line `shouldSatisfy` T.isInfixOf "`takeWhile1`"
                    line `shouldSatisfy` (not . T.isInfixOf "takeWhile1\\n")
        it "falls through to the unshowable contrast for a Show-at-print red" $
            case contrastLine unshowableErr of
                Nothing -> expectationFailure "no unshowable contrast"
                Just line -> do
                    line `shouldSatisfy` T.isInfixOf "displayPicture"
                    line `shouldSatisfy` T.isInfixOf "Sabela.Notebook"
        it "stays Nothing for an unrelated runtime error" $
            contrastLine "cell 3: divide by zero" `shouldBe` Nothing
        it "unescapes before the unshowable contrast too" $
            case contrastLine (T.replace "\n" "\\n" unshowableErr) of
                Nothing -> expectationFailure "no contrast from escaped blob"
                Just line -> line `shouldSatisfy` T.isInfixOf "displayPicture"
        it "is Nothing on a module-load cascade (the names are not phantoms)" $ do
            let cascade =
                    T.unlines
                        [ "cell 0: Could not load module `Text.Megaparsec'."
                        , "It is a member of the hidden package `megaparsec-9.8.1'."
                        , "cell 0, line 70: Variable not in scope:"
                        , "  parse :: f0 a1 -> t0 -> String -> Either a2 Double"
                        , "Perhaps use `Parser' (line 10) (defined in cell 0)"
                        ]
            contrastLine cascade `shouldBe` Nothing

    describe "selfHealNote — a kept repair names its rewrite" $ do
        let before = "digits = takeWhile1 (\\c -> isDigit c)"
            after = "import Data.Char (isDigit)\ndigits = takeWhileP (\\c -> isDigit c)"
        it "is Nothing when the source is unchanged" $
            selfHealNote before before `shouldBe` Nothing
        it "carries the removed and added lines" $
            case selfHealNote before after of
                Nothing -> expectationFailure "no note for a changed source"
                Just v -> do
                    lookupKey "removed" v
                        `shouldBe` Just (toStrings ["digits = takeWhile1 (\\c -> isDigit c)"])
                    lookupKey "added" v
                        `shouldBe` Just
                            ( toStrings
                                [ "import Data.Char (isDigit)"
                                , "digits = takeWhileP (\\c -> isDigit c)"
                                ]
                            )
        it "tells the model not to re-submit the old code" $
            case selfHealNote before after of
                Just v
                    | Just (String n) <- lookupKey "note" v ->
                        n `shouldSatisfy` T.isInfixOf "do not re-submit"
                _ -> expectationFailure "no note text"
        it "always inlines the post-heal source (R7.1: 'CURRENT source' is carried)" $
            case selfHealNote before after of
                Nothing -> expectationFailure "no note for a changed source"
                Just v -> lookupKey "source" v `shouldBe` Just (String after)

    describe "R7.1/R7.2 heal-note-iff-diff (generated source pairs)" $ do
        let srcs =
                [ ""
                , "x = 1"
                , "x = 2"
                , "import Data.Time\nx = 1"
                , "-- cabal: build-depends: time\nimport Data.Time\nx = 1"
                , "x = 1\ny = 2"
                , "y = 2\nx = 1"
                ] ::
                    [Text]
            pairs = [(b, a) | b <- srcs, a <- srcs]
            deltaEmpty (r, ad) = null r && null ad
        it "a note exists iff sourceDelta is nonempty" $
            mapM_
                ( \(b, a) ->
                    ((b, a), isNothing (selfHealNote b a))
                        `shouldBe` ((b, a), deltaEmpty (sourceDelta b a))
                )
                pairs
        it "every emitted note carries the exact post-heal source and true diff" $
            mapM_
                ( \(b, a) -> case selfHealNote b a of
                    Nothing -> pure ()
                    Just v -> do
                        lookupKey "source" v `shouldBe` Just (String a)
                        let (removed, added) = sourceDelta b a
                        lookupKey "removed" v `shouldBe` Just (toStrings removed)
                        lookupKey "added" v `shouldBe` Just (toStrings added)
                )
                pairs

    describe "sourceDelta — line-level diff, order preserved" $
        it "reports only the changed lines" $
            sourceDelta "a\nb\nc" "a\nB\nc" `shouldBe` (["b"], ["B"])

    describe "attachSelfHeal — the wire seam into the cell result" $ do
        let cr = object ["ok" .= False]
        it "passes the result through when no repair happened" $
            attachSelfHeal Nothing cr `shouldBe` cr
        it "adds the self_heal field when a repair happened" $ do
            let note = object ["note" .= ("x" :: Text)]
            lookupKey "self_heal" (attachSelfHeal (Just note) cr)
                `shouldBe` Just note

    describe "dependencySuggestionNote (G2 hard rule 1 — disclosed, never applied)" $ do
        it "names the package and the candidate source, and points at the manual fix" $ do
            let v =
                    dependencySuggestionNote
                        ["http-conduit"]
                        "-- cabal: build-depends: http-conduit\nx = 1"
            lookupKey "packages" v `shouldBe` Just (toStrings ["http-conduit"])
            lookupKey "source" v
                `shouldBe` Just (String "-- cabal: build-depends: http-conduit\nx = 1")
            case lookupKey "note" v of
                Just (String n) -> do
                    n `shouldSatisfy` T.isInfixOf "http-conduit"
                    n `shouldSatisfy` T.isInfixOf "never adds a dependency automatically"
                _ -> expectationFailure "no note text"

    describe "attachSelfHealSuggestions" $ do
        let cr = object ["ok" .= False]
        it "passes the result through unchanged when there are no suggestions" $
            attachSelfHealSuggestions [] cr `shouldBe` cr
        it "adds every suggestion as self_heal_suggestions" $ do
            let sug = dependencySuggestionNote ["time"] "x = 1"
            lookupKey "self_heal_suggestions" (attachSelfHealSuggestions [sug] cr)
                `shouldBe` Just (toJSON [sug])

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup (K.fromText k) o
lookupKey _ _ = Nothing

toStrings :: [Text] -> Value
toStrings = toJSON
