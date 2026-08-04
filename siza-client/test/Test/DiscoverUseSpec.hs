{-# LANGUAGE OverloadedStrings #-}

{- | C2-7a/b/c: every hit says how to bring it into scope, same-named hits
from different modules say they clash, and a module card names the exports
that collide with names already in scope.
-}
module Test.DiscoverUseSpec (discoverUseSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Either (isRight)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Discover.Affordance (clashBudget, moduleShaped)
import Siza.Agent.Discover.Envelope (envelopeViolations)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope, envelopeFrom)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    emptyScope,
    okAnswer,
 )
import Siza.Lang.Haskell (parseHaskell)
import Test.DiscoverFixtures (field, hitText, hitsOf)
import Test.DiscoverGen (
    genClashGroup,
    genEnvOver,
    genHitOver,
    genModulePair,
    genPkgPair,
    genQualName,
    genValueName,
    seedModules,
 )

discoverUseSpec :: Spec
discoverUseSpec = describe "every hit says how to call it (C2-7)" $ do
    useStampSpec
    useParsesSpec
    clashSpec
    cardClashSpec

hkT :: HackageInfo
hkT = HackageInfo True [] []

envelopeOf :: NotebookEnv -> Text -> [DHit] -> Value
envelopeOf env q hs =
    discoverEnvelope env (interpret env q) 40 [okAnswer "session" hs] hkT

-- | A module name a hit can actually be imported from.
realModule :: Text -> Bool
realModule m =
    not (T.null m)
        && maybe False ((`elem` ['A' .. 'Z']) . fst) (T.uncons m)
        && not (T.any (== ' ') m)

useStampSpec :: Spec
useStampSpec = describe "C2-7a the use note is stamped at the chokepoint" $ do
    it "every hit from a real module carries a non-empty use" $
        property $
            forAll ((,,) <$> genQualName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (genHitOver n m p) $ \h ->
                        forAll (genEnvOver (m : seedModules)) $ \env ->
                            let v = envelopeOf env n [h]
                                bad =
                                    [ hitText "name" hj
                                    | hj <- hitsOf v
                                    , realModule (hitText "module" hj)
                                    , T.null (hitText "use" hj)
                                    ]
                             in counterexample (show (v, bad)) (null bad)
    it "a module the notebook has not imported yields an import naming it" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, other), (p, _)) ->
                    let env = NotebookEnv [] [other] [] [] [] []
                        h =
                            DHit
                                n
                                "Int -> Int"
                                m
                                p
                                "1.0"
                                InstInstalled
                                MkExact
                                "session"
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                        v = envelopeOf env n [h]
                     in case hitsOf v of
                            [] -> counterexample "no hits" False
                            (hj : _) ->
                                let u = hitText "use" hj
                                 in counterexample (T.unpack u) $
                                        conjoin
                                            [ property (m `T.isInfixOf` u)
                                            , property (n `T.isInfixOf` u)
                                            ]

{- | The general defence: whatever the merge puts in `use`, it is Haskell. The
generator must reach the colliding case, where a hit whose own module the
harness did not compute is offered a qualified import of that non-module.
-}
useParsesSpec :: Spec
useParsesSpec = describe "C2-7a the harness never emits illegal Haskell" $ do
    it "every emitted import line parses as a Haskell import" $
        property $
            forAll ((,,) <$> genQualName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (genHitOver n m p) $ \h ->
                        forAll (genEnvOver (m : seedModules)) $ \env ->
                            legalImports (envelopeOf env n [h])
    it "a group sharing one name, module unknown for some, still parses" $
        property $
            forAll genClashGroup $ \(n, hs) ->
                forAll (genEnvOver seedModules) $ \env ->
                    legalImports (envelopeOf env n hs)
    it "a hit that does not name an importable module claims no clash" $
        property $
            forAll genClashGroup $ \(n, hs) ->
                forAll (genEnvOver seedModules) $ \env ->
                    let v = envelopeOf env n hs
                        bad =
                            [ (hitText "module" hj, hitText "ambiguousWith" hj)
                            | hj <- hitsOf v
                            , not (moduleShaped (hitText "module" hj))
                            , not (T.null (hitText "ambiguousWith" hj))
                            ]
                     in counterexample (show (v, bad)) (null bad)

legalImports :: Value -> Property
legalImports v =
    counterexample (show (v, bad)) (null bad)
  where
    bad =
        [ u
        | hj <- hitsOf v
        , let u = hitText "use" hj
        , "import " `T.isPrefixOf` u
        , not (parses u)
        ]

parses :: Text -> Bool
parses u = isRight (parseHaskell (u <> "\n"))

clashSpec :: Spec
clashSpec = describe "C2-7b two hits sharing a bare name say they clash" $ do
    it "the collision is stated once, counted, and one sibling is named" $
        property $
            forAll genCollision $ \(shared, v) ->
                let claims =
                        [ (hitText "name" h, hitText "module" h, clashOf h)
                        | h <- hitsOf v
                        ]
                    present = nub [m | (n, m, _) <- claims, n == shared]
                    marked = [c | c@(_, _, raw) <- claims, not (T.null raw)]
                    unqualified =
                        [ m
                        | (n, m, _) <- claims
                        , n == shared
                        , length present > 1
                        , not (qualifies (useFor m v))
                        ]
                 in counterexample (show (v, claims, unqualified)) $
                        conjoin
                            ( property (length marked <= 1)
                                : property (null unqualified)
                                : map (wellMarked shared present) claims
                            )
    it "the whole answer spends at most the clash budget on saying so" $
        property $
            forAll genCollision $ \(_, v) ->
                let spent = sum [T.length (clashOf h) | h <- hitsOf v]
                 in counterexample (show (spent, v)) (spent <= clashBudget)

clashOf :: Value -> Text
clashOf = hitText "ambiguousWith"

qualifies :: Text -> Bool
qualifies = T.isPrefixOf "import qualified "

useFor :: Text -> Value -> Text
useFor m v =
    T.concat [hitText "use" h | h <- hitsOf v, hitText "module" h == m]

{- | Colliding hits, over arbitrary names and modules, with one hit that
shares nothing so a marker that fires on everything is caught.
-}
genCollision :: Gen (Text, Value)
genCollision = do
    shared <- genValueName
    solo <- genValueName `suchThat` (/= shared)
    k <- choose (1, 6)
    mods0 <- vectorOf k (fst <$> genModulePair)
    soloMod <- (fst <$> genModulePair) `suchThat` (`notElem` mods0)
    let hs =
            [synHit shared m pk | (m, pk) <- zip (nub mods0) (cycle pkgPool)]
                ++ [synHit solo soloMod "bytestring"]
    pure (shared, envelopeOf (NotebookEnv [] [] [] [] [] []) shared hs)

{- | The surgical clash contract: the marker counts the siblings and names one
of them, and a hit with no sibling is never marked.
-}
wellMarked :: Text -> [Text] -> (Text, Text, Text) -> Property
wellMarked shared present (nm, md, raw)
    | nm /= shared = counterexample "singleton marked" (T.null raw)
    | null others = counterexample "lone module marked" (T.null raw)
    | T.null raw = property True
    | otherwise =
        counterexample (T.unpack raw) $
            conjoin
                [ property (all (`elem` others) named)
                , property (length named <= 1)
                , property (T.pack (show (length others)) `T.isPrefixOf` raw)
                ]
  where
    others = [m | m <- present, m /= md]
    named = [m | m <- present, m `T.isInfixOf` raw, m /= md]

pkgPool :: [Text]
pkgPool = ["containers", "text", "aeson", "mtl", "bytestring", "vector"]

synHit :: Text -> Text -> Text -> DHit
synHit n m p =
    DHit
        n
        ("Int -> Int " <> m)
        m
        p
        "1.0"
        InstInstalled
        MkExact
        "session"
        Nothing
        Nothing
        Nothing
        Nothing

cardClashSpec :: Spec
cardClashSpec = describe "C2-7c a card names the exports already in scope" $
    it "the clash list equals the intersection exactly, and is absent when empty" $
        property $
            forAll (vectorOf 6 genValueName) $ \names ->
                forAll (choose (0, 6)) $ \k ->
                    let inScope = nub (take 6 names)
                        shared = take k inScope
                        extra = [n <> "Zz" | n <- inScope]
                        exports = [n <> " :: Int" | n <- shared ++ extra]
                        env = NotebookEnv [] [] [] inScope [] []
                        card =
                            object
                                [ "module" .= ("Some.Module" :: Text)
                                , "status" .= ("ok" :: Text)
                                , "exports" .= exports
                                ]
                        interp = Interpreted "q" "q" Nothing "name" "" []
                        v =
                            envelopeFrom
                                env
                                interp
                                emptyScope
                                10
                                []
                                hkT
                                (Just card)
                                []
                        got =
                            sort
                                . splitList
                                . maybe "" clashText
                                $ field "card" v
                     in counterexample (show v) $
                            conjoin
                                [ got === sort (nub shared)
                                , envelopeViolations v === []
                                ]

splitList :: Text -> [Text]
splitList t
    | T.null t = []
    | otherwise = map T.strip (T.splitOn "," t)

clashText :: Value -> Text
clashText c = case field "clashesInScope" c of
    Just (String s) -> s
    _ -> ""
