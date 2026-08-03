{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverScopeConserveSpec (discoverScopeConserveSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Siza.Agent.Discover.Facts (factPackages)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (
    discoverEnvelopeRecent,
    discoverEnvelopeScoped,
 )
import Siza.Agent.Discover.Request (scopeActive)
import Siza.Agent.Discover.ScopeFilter (scopeRemovedNote)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
    emptyScope,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitsOf, stateOf, textField)

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

{- | One entity two sources describe: the same package, and a record whose
module the harness did not compute. The filter must speak for that record.
-}
attributedHits :: [DHit]
attributedHits =
    [ (mkHit "colList" "" "frameio"){dhOrigin = "session"}
    , (mkHit "colList" "Frame" "frameio"){dhOrigin = "hoogle"}
    ]

-- | Two entities that share nothing but a name.
namesakeHits :: [DHit]
namesakeHits =
    [ (mkHit "colList" "Frame" ""){dhOrigin = "session"}
    , (mkHit "colList" "Ops.Internal" "frameio"){dhOrigin = "hoogle"}
    ]

scoped :: Scope -> [DHit] -> Value
scoped scope hits =
    discoverEnvelopeScoped
        env0
        (interpret env0 "colList")
        scope
        8
        [okAnswer "hoogle" hits]
        hk0

discoverScopeConserveSpec :: Spec
discoverScopeConserveSpec = do
    refinementSpec
    factPackagesSpec
    scopedCardSpec
    narrowNoteSpec
    describe "post-union scope predicate (section 3.3)" $ do
        it "keeps an exact hit whose attributed sibling module satisfies the filter" $ do
            let v = scoped (Scope (Just "Frame") Nothing) attributedHits
            stateOf v `shouldBe` "found"
            map (textField "module") (hitsOf v)
                `shouldMatchList` ["", "Frame"]

        it "conserves totals: the filtered total equals the attributed-kept count" $ do
            let v = scoped (Scope (Just "Frame") Nothing) attributedHits
            length (hitsOf v) `shouldBe` 2

        it "drops a namesake whose only claim on the module is the name (A1)" $ do
            let v = scoped (Scope (Just "Frame") Nothing) namesakeHits
            stateOf v `shouldBe` "found"
            map (textField "module") (hitsOf v) `shouldBe` ["Frame"]
            textField "narrow" v `shouldSatisfy` ("Ops.Internal" `T.isInfixOf`)

        it "a filter that excludes everything names the removed candidates" $ do
            let v = scoped (Scope (Just "Granite") Nothing) namesakeHits
            stateOf v `shouldBe` "not_found"
            let narrow = textField "narrow" v
            narrow `shouldSatisfy` ("removed" `T.isInfixOf`)
            narrow `shouldSatisfy` ("colList" `T.isInfixOf`)
            narrow `shouldSatisfy` ("Frame" `T.isInfixOf`)
            narrow `shouldSatisfy` ("Ops.Internal" `T.isInfixOf`)

        it
            "package filters honour module attribution (a session hit missing its package)"
            $ do
                let hits =
                        [ (mkHit "colList" "Frame" ""){dhOrigin = "session"}
                        , (mkHit "colList" "Frame.Ops" "frameio"){dhOrigin = "hoogle"}
                        ]
                    answer =
                        (okAnswer "hoogle" hits)
                            { saPkgModules = [("frameio", ["Frame", "Frame.Ops"])]
                            }
                    v =
                        discoverEnvelopeScoped
                            env0
                            (interpret env0 "colList")
                            (Scope Nothing (Just "frameio"))
                            8
                            [answer]
                            hk0
                stateOf v `shouldBe` "found"
                length (hitsOf v) `shouldBe` 2

scopedCardSpec :: Spec
scopedCardSpec = describe "a scoped request never carries a foreign card" $ do
    let cardFor m =
            object
                [ "module" .= (m :: T.Text)
                , "status" .= ("ok" :: T.Text)
                , "exports" .= (["x :: Int"] :: [T.Text])
                ]
        pkgCardFor p =
            object
                [ "package" .= (p :: T.Text)
                , "status" .= ("installed-not-loaded" :: T.Text)
                , "modules" .= (["Some.Module"] :: [T.Text])
                ]
        envWith scope c =
            discoverEnvelopeScoped
                env0
                (interpret env0 "!?")
                scope
                8
                [(okAnswer "session" []){saCard = Just c}]
                hk0
    it "drops a card whose module is not the scoped one" $
        field
            "card"
            (envWith (Scope (Just "Data.Csv") Nothing) (cardFor "Data.ByteString"))
            `shouldBe` Nothing
    it "keeps the card when it IS the scoped module" $
        fmap
            (textField "module")
            (field "card" (envWith (Scope (Just "Data.Csv") Nothing) (cardFor "Data.Csv")))
            `shouldBe` Just "Data.Csv"
    it "leaves an unscoped request's card alone" $
        fmap
            (textField "module")
            (field "card" (envWith (Scope Nothing Nothing) (cardFor "Data.ByteString")))
            `shouldBe` Just "Data.ByteString"
    it "drops a card whose package is not the scoped one" $
        field
            "card"
            (envWith (Scope Nothing (Just "cassava")) (pkgCardFor "bytestring"))
            `shouldBe` Nothing
    it "keeps the card when it IS the scoped package" $
        fmap
            (textField "package")
            ( field
                "card"
                (envWith (Scope Nothing (Just "cassava")) (pkgCardFor "cassava"))
            )
            `shouldBe` Just "cassava"
    it "a scope that drops the only card, with no hits, is not_found" $
        stateOf
            (envWith (Scope (Just "Data.Csv") Nothing) (cardFor "Data.ByteString"))
            `shouldBe` "not_found"
    it "a package scope that drops the only card, with no hits, is not_found" $
        stateOf
            (envWith (Scope Nothing (Just "cassava")) (pkgCardFor "bytestring"))
            `shouldBe` "not_found"
    it "keeps a card that does not state the scoped axis at all" $
        fmap
            (textField "module")
            ( field
                "card"
                (envWith (Scope Nothing (Just "cassava")) (cardFor "Data.Csv"))
            )
            `shouldBe` Just "Data.Csv"

refinementSpec :: Spec
refinementSpec = describe "a search refines what the session established" $ do
    let hit n p =
            (mkHit n "M" p)
                { dhType = "X -> Y"
                , dhInstall = InstHidden
                }
        answers = [okAnswer "hoogle" [hit "colAaa" "strange", hit "colZzz" "session"]]
        rankedWith recent =
            map
                (textField "name")
                ( hitsOf
                    ( discoverEnvelopeRecent
                        recent
                        env0
                        (interpret env0 "col")
                        emptyScope
                        8
                        answers
                        hk0
                    )
                )
    it "a session-established package leads its stratum" $
        head (rankedWith ["session"]) `shouldBe` "colZzz"
    it "with no session footprint the order is the static one" $
        head (rankedWith []) `shouldBe` "colAaa"
    it "refinement never drops the stranger" $
        length (rankedWith ["session"]) `shouldBe` 2

{- | C2-smaller-narrow: a scope disclosure must name the candidates it removed
and must never leave a dangling clause behind an empty field.
-}
narrowNoteSpec :: Spec
narrowNoteSpec = describe "the narrow disclosure is well formed (C2-smaller-narrow)" $ do
    prop "no note has an empty segment, whatever fields the hits lack" $
        forAll genRemovalCase $ \(hits, scope) ->
            let narrow = textField "narrow" (scoped scope hits)
             in counterexample (T.unpack narrow) (property (wellFormed narrow))
    prop "an excluded candidate's declaration is stated as itself" $
        forAll ((,) <$> genEntity <*> genEntity) $ \(n, a) ->
            let decl = "type " <> n <> " = " <> a
                scope = Scope (Just "Ctx.One") Nothing
                h = (mkHit n "Far.Two" "pfar"){dhType = decl}
                note = fromMaybe "" (scopeRemovedNote scope [h])
             in counterexample (T.unpack note) $
                    conjoin
                        [ property (decl `T.isInfixOf` note)
                        , property (not ((":: " <> decl) `T.isInfixOf` note))
                        ]
    prop "a removal note names an excluded candidate, or says nothing at all" $
        forAll ((,) <$> listOf1 genHit <*> genScope) $ \(removed, scope) ->
            case scopeRemovedNote scope removed of
                Nothing -> property (not (scopeActive scope))
                Just note ->
                    counterexample (T.unpack note) . property $
                        wellFormed note
                            && any (\h -> dhName h `T.isInfixOf` note) removed

wellFormed :: T.Text -> Bool
wellFormed narrow =
    T.null narrow
        || ( not (": ;" `T.isInfixOf` narrow)
                && not (": ," `T.isInfixOf` narrow)
                && not (": " `T.isSuffixOf` narrow)
                && not (any (T.null . T.strip) (T.splitOn "; " narrow))
           )

-- | An entity a removed candidate can be about, with nothing of a shape in it.
genEntity :: Gen T.Text
genEntity = do
    c <- elements ['A' .. 'Z']
    n <- choose (2, 7)
    T.pack . (c :) <$> vectorOf n (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

genRemovalCase :: Gen ([DHit], Scope)
genRemovalCase = (,) <$> listOf1 genHit <*> genScope

genScope :: Gen Scope
genScope =
    Scope
        <$> elements [Nothing, Just "Ctx.One", Just "Absent.Module"]
        <*> elements [Nothing, Just "pctx", Just "absentpkg"]

genHit :: Gen DHit
genHit = do
    n <- elements ["colList", "insertWith", "splitOn", "encode", "pack"]
    m <- elements ["", "Ctx.One", "Far.Two", "Ops.Internal"]
    p <- elements ["", "pctx", "pfar"]
    ty <- elements ["", "Int -> Int", "Ord k => k -> Map k a -> Maybe a"]
    pure (mkHit n m p){dhType = ty, dhOrigin = "hoogle"}

factPackagesSpec :: Spec
factPackagesSpec = describe "the session footprint from held facts" $ do
    it "reads an install fact's package" $
        factPackages
            [ "dataframe (installed-not-loaded): -- cabal: build-depends: dataframe — provides `readCsv`"
            ]
            `shouldBe` ["dataframe"]
    it "reads a signature fact's provenance" $
        factPackages
            ["`bars` :: [(Text, Double)] -> Plot -> Text — found in Cumulus.Plot (cumulus)"]
            `shouldBe` ["cumulus"]
    it "holds nothing for a compiler fact with no provenance" $
        factPackages
            ["`defaultReadOptions` :: ReadOptions — confirmed by the compiler (check_type)"]
            `shouldBe` []
