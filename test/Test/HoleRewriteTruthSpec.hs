{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Fix H, the reading half: what a rejection may say about the harness's hole.
Every fact published must come from the block GHC wrote about that hole and no
other, and nothing the caller already had may be displaced.
-}
module Test.HoleRewriteTruthSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Char (isDigit)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Sabela.AI.Capabilities.Edit.HoleNudge (attachPairs)
import Sabela.AI.Capabilities.Edit.HoleRewrite (
    HoleRewrite (..),
    Placement (..),
    Trigger (..),
    holeName,
    holeRewritePairs,
    holeRewriteSource,
    runHoleRewrite,
 )
import Sabela.AI.Capabilities.Edit.HoleRewrite.Section (
    errorBlocks,
    typeVariables,
 )
import Sabela.AI.TypedHole (containsTypedHole)
import Test.HoleRewriteGen (
    HeadSource (..),
    applySrc,
    countingProbe,
    field,
    genDistinct,
    genHeadSource,
    genIdent,
    holeBlob,
    namedHoleBlob,
    scopeDiag,
    twoHoleDiag,
    weakHoleDiag,
    wrappedHoleDiag,
    wrappedType,
 )

spec :: Spec
spec = describe "what a hole-rewritten rejection may say" $ do
    provenanceSpec
    selectionSpec
    typeSpec
    payloadSpec

provenanceSpec :: Spec
provenanceSpec = describe "provenance" $ do
    prop "attributes the substitution to the harness, naming the head" $
        forAll (genDistinct 2) $ \[headName, fitName] ->
            let prov = provenanceOf headName (fitBlob fitName)
             in conjoin
                    [ (prov >>= textAt "substituted") === Just headName
                    , (prov >>= textAt "with") === Just holeName
                    , (prov >>= textAt "trigger") === Just "GHC-88464"
                    , counterexample "no disclosure" $
                        maybe False (T.isInfixOf "the harness") (prov >>= textAt "by")
                    ]

    prop "never publishes the rewritten source" $
        forAll genHeadSource $ \hs ->
            forAll genIdent $ \fitName -> monadicIO $ do
                (probe, _) <- run (countingProbe (fitBlob fitName))
                out <-
                    run
                        ( runHoleRewrite
                            probe
                            (scopeDiag 88464 (hsHead hs))
                            (hsSrc hs)
                        )
                let rendered = renderPairs (maybe [] holeRewritePairs out)
                    holed = maybe [] holedLines (rewrittenOf hs)
                assert (not (any (`T.isInfixOf` rendered) holed))

    prop "hands back no source, so nothing committed can carry the hole" $
        forAll genHeadSource $ \hs ->
            forAll genIdent $ \fitName -> monadicIO $ do
                (probe, _) <- run (countingProbe (fitBlob fitName))
                out <-
                    run
                        ( runHoleRewrite
                            probe
                            (scopeDiag 88464 (hsHead hs))
                            (hsSrc hs)
                        )
                let rendered = renderPairs (maybe [] holeRewritePairs out)
                assert
                    ( not (containsTypedHole (hsSrc hs))
                        && T.count holeName rendered == 1
                    )

selectionSpec :: Spec
selectionSpec = describe "selecting the harness's own hole" $ do
    prop "still asks about an unresolved head when the caller's hole was answered" $
        forAll (genDistinct 2) $ \[headName, ownHole] ->
            let diagnostic =
                    namedHoleBlob ownHole "Int" [("maxBound", "Int")]
                        <> "\n\n"
                        <> scopeDiag 88464 headName
                src = "x = _\ny = " <> headName <> " 1\n"
             in fmap (\(nm, _, _) -> nm) (holeRewriteSource diagnostic src)
                    === Just headName

    prop "reads the hole it wrote, not one the candidate already carried" $
        forAll (genDistinct 4) $ \[headName, ownBinder, theirs, ours] ->
            let prov = provenanceOf headName (twoHoleDiag ownBinder theirs ours)
             in conjoin
                    [ (prov >>= textAt "holeType") === Just "[Int] -> Int"
                    , fitWrites prov === [ours]
                    ]

    prop "reads its own hole on a candidate that already carried one" $
        forAll (genDistinct 5) $
            \[binder, headName, ownBinder, theirs, ours] -> monadicIO $ do
                (probe, count) <-
                    run (countingProbe (twoHoleDiag ownBinder theirs ours))
                out <-
                    run
                        ( runHoleRewrite
                            probe
                            (scopeDiag 88464 headName)
                            (ownBinder <> " = _\n" <> binder <> " = " <> headName <> " 1\n")
                        )
                n <- run count
                let prov = field "holeRewrite" (object (maybe [] holeRewritePairs out))
                assert
                    ( n == 1
                        && (prov >>= textAt "holeType") == Just "[Int] -> Int"
                        && fitWrites prov == [ours]
                    )

    prop "publishes nothing the compiler did not offer as a fit" $
        forAll (genDistinct 4) $ \[headName, ownBinder, theirs, ours] ->
            let writes = fitWrites (provenanceOf headName (twoHoleDiag ownBinder theirs ours))
             in counterexample (show writes) $
                    all (\w -> T.all identChar w && not (T.null w)) writes

    prop "a probe that found no hole of the harness's is not a result" $
        forAll (genDistinct 2) $ \[headName, binder] ->
            forAll
                (elements ["error: something else entirely", weakOther binder])
                $ \blob ->
                    holeRewritePairs (rewriteOf headName blob) === []

    prop "keeps GHC's echoed source out of the block it reads" $
        forAll (genDistinct 4) $ \[_, ownBinder, theirs, ours] ->
            let ls = concatMap (T.splitOn "\n") (errorBlocks (twoHoleDiag ownBinder theirs ours))
             in counterexample (show ls) (not (any echoed ls))

    prop "emits nothing the fit rule refuses to admit" $
        forAll (genDistinct 2) $ \[headName, nm] ->
            fitWrites (provenanceOf headName (holeBlob "aaa -> aaa" [(nm, "aaa -> aaa")]))
                === []
  where
    weakOther binder = T.replace holeName (binder <> "z") (weakHoleDiag binder)

typeSpec :: Spec
typeSpec = describe "the type GHC printed" $ do
    prop "reads a type GHC wrapped onto continuation lines" $
        forAll (genDistinct 2) $ \[headName, fitName] ->
            let prov = provenanceOf headName (wrappedHoleDiag fitName)
             in conjoin
                    [ (prov >>= textAt "holeType") === Just wrappedType
                    , fitWrites prov === [fitName]
                    ]

    prop "stops the type at GHC's prose, and never quotes it" $
        forAll (genDistinct 2) $ \[headName, binder] ->
            let prov = provenanceOf headName (weakHoleDiag binder)
             in conjoin
                    [ (prov >>= textAt "holeType") === Just "t1 -> t2 -> t3"
                    , (prov >>= field "typeVariables")
                        === Just (toJSON (["t1", "t2", "t3"] :: [Text]))
                    ]

    it "names the type variables a printed type mentions" $ do
        typeVariables "Int -> Maybe b" `shouldBe` ["b"]
        typeVariables "[Int] -> Int" `shouldBe` []

payloadSpec :: Spec
payloadSpec = describe "what the rejection carries" $ do
    prop "does not offer the binder the hole itself sits in" $
        forAll (genDistinct 3) $ \[binder, headName, fitName] -> monadicIO $ do
            let blob =
                    holeBlob
                        "[Int] -> Int"
                        [(binder, "[Int] -> Int"), (fitName, "[Int] -> Int")]
            (probe, _) <- run (countingProbe blob)
            out <-
                run
                    ( runHoleRewrite
                        probe
                        (scopeDiag 88464 headName)
                        (applySrc binder headName ["1"])
                    )
            let prov = field "holeRewrite" (object (maybe [] holeRewritePairs out))
            assert (fitWrites prov == [fitName])

    prop "displaces nothing the rejection already answered" $
        forAll (genDistinct 3) $ \[headName, fitName, theirFit] ->
            let base = crowdedBase theirFit
                merged = attachPairs (holeRewritePairs (rewriteOf headName (fitBlob fitName))) base
             in conjoin
                    [ keysOf merged === sort (KM.keys (asMap base) <> ["holeRewrite"])
                    , counterexample "base value changed" $
                        all
                            (\k -> KM.lookup k (asMap merged) == KM.lookup k (asMap base))
                            (KM.keys (asMap base))
                    , fitWrites (field "holeRewrite" merged) === [fitName]
                    ]

    prop "leaves the rejection payload byte-identical when it does not fire" $
        forAll (genDistinct 3) $ \[binder, outer, inner] -> monadicIO $ do
            (probe, _) <- run (countingProbe (fitBlob "beta"))
            out <-
                run
                    ( runHoleRewrite
                        probe
                        (scopeDiag 88464 inner)
                        (binder <> " = " <> outer <> " " <> inner <> "\n")
                    )
            let base = crowdedBase "gamma"
            assert (attachPairs (maybe [] holeRewritePairs out) base == base)
  where
    crowdedBase :: Text -> Value
    crowdedBase theirFit =
        Object
            ( KM.fromList
                [ ("diagnostic", String "d")
                , ("notCommitted", String "compile-gate")
                , ("holeFits", toJSON [object ["write" .= theirFit]])
                , ("expectedType", String "Int")
                ]
            )
    asMap = \case
        Object o -> o
        _ -> KM.empty
    keysOf = sort . KM.keys . asMap

-- Helpers ---------------------------------------------------------------

rewriteOf :: Text -> Text -> HoleRewrite
rewriteOf nm = HoleRewrite (AtHead nm) (TriggerCode 88464) Nothing [] []

-- | The rewritten candidate, computed independently of what the rewrite kept.
rewrittenOf :: HeadSource -> Maybe Text
rewrittenOf hs =
    thd <$> holeRewriteSource (scopeDiag 88464 (hsHead hs)) (hsSrc hs)
  where
    thd (_, _, c) = c

provenanceOf :: Text -> Text -> Maybe Value
provenanceOf headName blob =
    field "holeRewrite" (object (holeRewritePairs (rewriteOf headName blob)))

fitBlob :: Text -> Text
fitBlob nm = holeBlob "[Int] -> Int" [(nm, "[Int] -> Int")]

fitWrites :: Maybe Value -> [Text]
fitWrites prov = case prov >>= field "holeFits" of
    Just (Array xs) -> [w | v <- foldr (:) [] xs, Just (String w) <- [field "write" v]]
    _ -> []

textAt :: Text -> Value -> Maybe Text
textAt k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

-- | The lines of the rewritten candidate that carry the harness's hole.
holedLines :: Text -> [Text]
holedLines src = [l | l <- T.splitOn "\n" src, holeName `T.isInfixOf` l]

-- | A line of the source excerpt GHC prints beneath a diagnostic.
echoed :: Text -> Bool
echoed l = "|" `T.isPrefixOf` T.stripStart (T.dropWhile isDigit (T.stripStart l))

identChar :: Char -> Bool
identChar c = c `notElem` (" |\8226:" :: String)

renderPairs :: [Pair] -> Text
renderPairs = T.pack . show . object
