{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverInvariantSpec (discoverInvariantSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.LeakShape (lexicalEntity)
import Siza.Agent.Discover.Classify (sessionAnswer)
import Siza.Agent.Discover.Interpret (interpret, stripDecoration, stripVersion)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    SourceAnswer (..),
    mkHit,
    okAnswer,
    seededBuiltins,
    unavailableAnswer,
 )
import Test.DiscoverFixtures

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

nameInterp :: Text -> Interpreted
nameInterp = interpret env0

hk0 :: HackageInfo
hk0 = HackageInfo True [] [] []

discoverInvariantSpec :: Spec
discoverInvariantSpec =
    beforeAll_ installNamesFile $
        describe "discover general invariants (R3)" $ do
            describe "R3.1 findability: every catalogue export is top-3 for its own name" $
                it "holds over the whole catalogue" $ do
                    misses <- concat <$> mapM findMiss catalogueExports
                    misses `shouldBe` []

            describe "R3.2 exact-name-first: no weak hit precedes an exact hit" $ do
                it "holds over the whole catalogue" $ do
                    bad <- concat <$> mapM exactFirstViolation catalogueExports
                    bad `shouldBe` []
                it "holds in the pure merge regardless of input order" $ do
                    let weak =
                            (mkHit "gusty" "Syn.Anim" "junk")
                                { dhKind = MkSynonym
                                }
                        exact = mkHit "gust" "Zephyr.Core" "zephyr"
                        v =
                            discoverEnvelope
                                env0
                                (nameInterp "gust")
                                8
                                [okAnswer "hoogle" [weak, exact]]
                                hk0
                    map (hitText "name") (hitsOf v)
                        `shouldBe` ["gust", "gusty"]

            describe "R3.3 conservation: no source's non-empty answer is dropped" $ do
                it "unions a strong source with a weak one (no ladder discard)" $ do
                    let strong = okAnswer "session" [mkHit "alpha" "A.B" "a"]
                        weak =
                            okAnswer
                                "hoogle"
                                [ (mkHit "alphaLike" "C.D" "c")
                                    { dhKind = MkSubstring
                                    }
                                , (mkHit "alphaIsh" "E.F" "e")
                                    { dhKind = MkSynonym
                                    }
                                ]
                        v =
                            discoverEnvelope
                                env0
                                (nameInterp "alpha")
                                8
                                [strong, weak]
                                hk0
                    map (hitText "name") (hitsOf v)
                        `shouldBe` ["alpha", "alphaLike", "alphaIsh"]
                    intField "total" v `shouldBe` 3
                it "counts capped hits in omitted; shown+omitted == total (R3.4)" $ do
                    let many =
                            okAnswer
                                "hoogle"
                                [ mkHit ("n" <> T.pack (show i)) "M.N" "p"
                                | i <- [1 .. 12 :: Int]
                                ]
                        v =
                            discoverEnvelope
                                env0
                                (nameInterp "n1")
                                5
                                [many]
                                hk0
                    intField "shown" v `shouldBe` 5
                    intField "shown" v + intField "omitted" v
                        `shouldBe` intField "total" v
                    intField "total" v `shouldBe` 12
                it "reconciles shown+omitted == total on every catalogue query" $ do
                    bad <- concat <$> mapM reconcileViolation discoverables
                    bad `shouldBe` []

            describe "R3.5 provenance is computed, never a placeholder" $
                it "install is from the vocabulary; no field says \"unknown\"" $ do
                    bad <- concat <$> mapM provenanceViolation discoverables
                    bad `shouldBe` []

            describe "R2.1 decorated names reduce to the bare name" $ do
                it "type applications strip" $
                    stripDecoration "col @T.Text \"month\"" `shouldBe` "col"
                it "applied literal arguments strip" $
                    stripDecoration "col \"revenue\"" `shouldBe` "col"
                it "backticks and quotes strip" $
                    stripDecoration "`divvy`" `shouldBe` "divvy"
                it "operators in parens survive verbatim (R2.3)" $
                    stripDecoration "(<>)" `shouldBe` "(<>)"
                it "prose is untouched" $
                    stripDecoration "edit distance between strings"
                        `shouldBe` "edit distance between strings"

            describe "R2.5 package-with-version strings resolve to the package" $
                it "strips a trailing version" $ do
                    stripVersion "dataframe-0.7.0.0" `shouldBe` "dataframe"
                    stripVersion "granite" `shouldBe` "granite"

            describe "envelope states" $ do
                it "a blank query is bad_request, not a miss (R2.6)" $ do
                    v <- runCat "   "
                    stateOf v `shouldBe` "bad_request"
                it "session-unavailable incompleteness is disclosed (R1.1)" $ do
                    let v =
                            discoverEnvelope
                                env0
                                (nameInterp "someName")
                                8
                                [unavailableAnswer "session" "no live kernel"]
                                hk0
                    stateOf v `shouldBe` "not_found"
                    textField "next" v `shouldSatisfy` T.isInfixOf "session"
                    textField "next" v `shouldSatisfy` T.isInfixOf "unavailable"
                it "a near-miss suggests the nearest held name (R5.1)" $ do
                    let v =
                            discoverEnvelope
                                env0
                                (nameInterp "displayHtlm")
                                8
                                [okAnswer "session" []]
                                hk0
                    textField "next" v `shouldSatisfy` T.isInfixOf "displayHtml"
                it "is deterministic (R3.7)" $ do
                    a <- runCat "gust"
                    b <- runCat "gust"
                    a `shouldBe` b

            callableHitSpec

findMiss :: Text -> IO [Text]
findMiss n = do
    v <- runCat n
    let top3 = map (hitText "name") (take 3 (hitsOf v))
    pure [n | n `notElem` top3]

exactFirstViolation :: Text -> IO [Text]
exactFirstViolation n = do
    v <- runCat n
    let kinds = [(hitText "name" h, hitText "matchKind" h) | h <- hitsOf v]
        afterFirstWeak = dropWhile (\(nm, k) -> nm == n && k == "exact") kinds
    pure [n | any (\(nm, k) -> nm == n && k == "exact") afterFirstWeak]

reconcileViolation :: Text -> IO [Text]
reconcileViolation q = do
    v <- runCat q
    let ok =
            intField "shown" v == length (hitsOf v)
                && intField "shown" v + intField "omitted" v
                    == intField "total" v
    pure [q | not ok]

provenanceViolation :: Text -> IO [Text]
provenanceViolation q = do
    v <- runCat q
    let installVocab =
            [ "builtin"
            , "notebook"
            , "installed"
            , "installed-not-loaded"
            , "absent-known"
            , "absent-unknown"
            ]
        bad h =
            hitText "install" h
                `notElem` installVocab
                || elem "unknown" (fieldValues h)
                || any T.null (fieldValues h)
                || null (provenanceOf h)
    pure [q | any bad (hitsOf v)]

{- | C2-10a: provenance is stated when computed and omitted when not. The old
form required `package`/`version`/`module` to be present, which the literal
placeholder "unknown" satisfied without carrying a fact.
-}
fieldValues :: Value -> [Text]
fieldValues h =
    [t | k <- ["package", "version", "module"], Just (String t) <- [field k h]]

{- | Omitting a field is only honest while something still says where the hit
came from: a hit that names neither its module nor its package is unusable,
and absence must not become the way to satisfy 'fieldValues'.
-}
provenanceOf :: Value -> [Text]
provenanceOf h =
    [ t
    | k <- ["package", "module"]
    , Just (String t) <- [field k h]
    , not (T.null t)
    ]

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number n) -> round n
    _ -> -1

-- C2-5b: a card row only becomes a hit when it names a callable entity ----

{- | Includes the rows a decomposed listing carries: a class and a record head
alongside the members they declare, which the card emits as exports of their
own.
-}
wellFormedRows :: [Text]
wellFormedRows =
    [ "insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a"
    , "splitOn :: Text -> Text -> [Text]"
    , "(.:?) :: FromJSON a => Object -> Key -> Parser (Maybe a)"
    , "data Value"
    , "newtype Parser"
    , "runReaderT :: ReaderT r m a -> r -> m a"
    , "class Renderable a where renderOne :: a -> Int"
    , "renderOne :: a -> Int"
    , "data Speed = Speed {knots :: Int}"
    , "knots :: Speed -> Int"
    ]

fragmentRows :: [Text]
fragmentRows =
    [ "a b),"
    , "~ Bool) =>"
    , "-> Expr b"
    , "(Ord k,"
    , "forall {k}."
    , "= Constr {conrep :: ConstrRep,"
    , "a b)"
    ]

cardOf :: [Text] -> Value
cardOf rows =
    object
        [ "module" .= ("Syn.Frame" :: Text)
        , "status" .= ("ok" :: Text)
        , "exports" .= rows
        , "total" .= length rows
        ]

hitsFromCard :: [Text] -> [DHit]
hitsFromCard rows =
    saHits (sessionAnswer (nameInterp "insertWith") (Just (cardOf rows)))

callable :: Text -> Bool
callable t = not (T.null t) && not (T.any (== ' ') t) && lexicalEntity t

callableHitSpec :: Spec
callableHitSpec =
    describe "R3.8 a card row becomes a hit only when it is callable (C2-5b)" $ do
        prop "no hit names a signature fragment" $
            forAll (shuffle (wellFormedRows ++ fragmentRows)) $ \rows ->
                conjoin
                    [ counterexample (T.unpack ("not callable: " <> dhName h)) $
                        property (callable (dhName h))
                    | h <- hitsFromCard rows
                    ]
        prop "every well-formed row becomes exactly one hit" $
            forAll (shuffle (wellFormedRows ++ fragmentRows)) $ \rows ->
                length (hitsFromCard rows) === length wellFormedRows
        it "a card of nothing but fragments yields no hits, and says so" $ do
            let a = sessionAnswer (nameInterp "insertWith") (Just (cardOf fragmentRows))
            saHits a `shouldBe` []
            saNote a `shouldSatisfy` T.isInfixOf "7"
