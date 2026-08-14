{-# LANGUAGE OverloadedStrings #-}

{- | C2-10a: a hit field is either computed or absent. A mandated field whose
value is the literal placeholder @unknown@ spends the envelope's budget on a
non-answer, and @dropLastHit@ then sheds a real hit to pay for it.
-}
module Test.DiscoverHitFieldSpec (discoverHitFieldSpec) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Discover.Affordance (moduleShaped, writableName)
import Siza.Agent.Discover.Classify (capabilityAnswer)
import Siza.Agent.Discover.Envelope (
    envelopeViolations,
    hitKeys,
    requiredHitKeys,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    NotebookEnv (..),
    SourceAnswer (..),
    hitJson,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitsOf)
import Test.DiscoverGen (
    genClashGroup,
    genHitOver,
    genModulePair,
    genPkgPair,
    genQualName,
 )

discoverHitFieldSpec :: Spec
discoverHitFieldSpec = describe "a hit field is computed or absent (C2-10a)" $ do
    noPlaceholderSpec
    monotoneSpec
    catalogueOnlySpec
    schemaPromiseSpec
    indexDeclarationSpec

{- | An identifier the index could carry in a declaration: a type's name, or
the expansion it stands for.
-}
genIdent :: Gen Text
genIdent = do
    c <- elements ['A' .. 'Z']
    n <- choose (2, 7)
    cs <- vectorOf n (elements (['a' .. 'z'] ++ ['A' .. 'Z']))
    pure (T.pack (c : cs))

{- | The index rows the capability channel publishes for one entity, carrying
whatever type text the index stated for it.
-}
indexHitsOver :: Text -> Text -> [Value]
indexHitsOver n ty = map hitJson (saHits (capabilityAnswer (interpret envT n) (Just payload)))
  where
    payload = object ["hits" .= [object bucket]]
    bucket =
        [ "package" .= ("idx" :: Text)
        , "modules" .= (["M.Idx"] :: [Text])
        , "api" .= [object ["name" .= n, "module" .= ("M.Idx" :: Text), "type" .= ty]]
        ]

-- | The row an index bucket states for the entity itself, not for its package.
apiRowOf :: Text -> Text -> [Value]
apiRowOf n ty = [h | h <- indexHitsOver n ty, textAt "name" h == n]

{- | The type text a row carries, and the keys it carries it under: a hit that
states nothing carries no @type@ key at all.
-}
typeOf :: Text -> Text -> ([Text], [Text])
typeOf n ty =
    ( map (textAt "type") (apiRowOf n ty)
    , [k | h <- apiRowOf n ty, (k, _) <- fieldsOf h, k == "type"]
    )

{- | Wave-5 item 1: an index-origin hit carries the declaration its source
expanded, or no type at all. A declaration head expands to nothing, so
restating it would dress the hit's own name as its expansion.
-}
indexDeclarationSpec :: Spec
indexDeclarationSpec =
    describe "an index hit states an expansion its source gave, or nothing" $ do
        it "the type key is present exactly when the source stated one" $
            property $
                forAll ((,,,) <$> genIdent <*> genIdent <*> genIdent <*> elements declHeads) $
                    \(n, a, b, kw) ->
                        let decl = "type " <> n <> " = (" <> a <> ", " <> b <> ")"
                            sig = a <> " -> " <> b
                         in conjoin
                                [ counterexample "expansion" (typeOf n decl === ([decl], ["type"]))
                                , counterexample "head" (snd (typeOf n (kw <> " " <> n <> " a")) === [])
                                , counterexample "signature" (typeOf n sig === ([sig], ["type"]))
                                ]
        it "a head that ascribes a kind to a binder states no type" $
            property $
                forAll ((,,) <$> elements declHeads <*> genIdent <*> genKind) $
                    \(kw, n, k) ->
                        let head_ = kw <> " " <> n <> " (f :: " <> k <> ")"
                         in counterexample (T.unpack head_) $
                                snd (typeOf n head_) === []

declHeads :: [Text]
declHeads = ["type", "data", "newtype", "class", "pattern"]

-- | A kind an index item can ascribe to one of its head's binders.
genKind :: Gen Text
genKind = elements ["Type", "Type -> Type", "k", "[Type]"]

{- | The stand-ins a hit must never carry: a field the harness did not compute
is absent, so neither of these can be read as a fact.
-}
placeholders :: [Text]
placeholders = ["unknown", "(not installed)"]

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

{- | The live path C2-10a's placeholder actually survived on: a package the
Hackage catalogue knows and no source returned a hit for.
-}
catalogueOnlySpec :: Spec
catalogueOnlySpec = describe "the catalogue-only hit is built from what is known" $
    it "a Hackage-known package invents no module and no version" $
        property $
            forAll (fst <$> genPkgPair) $ \pkg ->
                let v =
                        discoverEnvelope
                            envT
                            (interpret envT pkg)
                            10
                            [okAnswer "session" []]
                            (HackageInfo True [pkg] [] [])
                    hs = hitsOf v
                    bad =
                        [ (k, s)
                        | h <- hs
                        , (k, String s) <- fieldsOf h
                        , s `elem` placeholders
                        ]
                 in counterexample (show v) $
                        conjoin
                            [ counterexample "no catalogue hit" (not (null hs))
                            , bad === []
                            , envelopeViolations v === []
                            ]

{- | The promise the envelope prints, checked against the envelope it
describes: a hit naming both an importable module and a writable name says how
to import it.
-}
schemaPromiseSpec :: Spec
schemaPromiseSpec = describe "the schema promise is one the merge can keep" $
    it "every hit naming an importable module and a writable name has a use" $
        property $
            forAll (fst <$> genPkgPair) $ \pkg ->
                forAll genClashGroup $ \(n, ghs) ->
                    let vs =
                            [ discoverEnvelope
                                envT
                                (interpret envT pkg)
                                10
                                [okAnswer "session" []]
                                (HackageInfo True [pkg] [] [])
                            , discoverEnvelope
                                envT
                                (interpret envT n)
                                40
                                [okAnswer "session" ghs]
                                (HackageInfo True [] [] [])
                            ]
                        bad =
                            [ h
                            | v <- vs
                            , h <- hitsOf v
                            , moduleShaped (textAt "module" h)
                            , writableName (textAt "name" h)
                            , T.null (textAt "use" h)
                            ]
                     in counterexample (show (vs, bad)) (null bad)

textAt :: Text -> Value -> Text
textAt k h = case field k h of
    Just (String s) -> s
    _ -> ""

genHit :: Gen DHit
genHit = do
    n <- genQualName
    (m, _) <- genModulePair
    (p, _) <- genPkgPair
    h <- genHitOver n m p
    blankM <- arbitrary
    blankP <- arbitrary
    blankV <- arbitrary
    pure
        h
            { dhModule = if blankM then "" else dhModule h
            , dhPackage = if blankP then "" else dhPackage h
            , dhVersion = if blankV then "" else dhVersion h
            }

fieldsOf :: Value -> [(Text, Value)]
fieldsOf (Object o) = [(K.toText k, v) | (k, v) <- KM.toList o]
fieldsOf _ = []

noPlaceholderSpec :: Spec
noPlaceholderSpec = describe "no key carries a not-computed placeholder" $ do
    it "no hit field is ever the literal \"unknown\"" $
        property $
            forAll genHit $ \h ->
                let bad =
                        [ k
                        | (k, String s) <- fieldsOf (hitJson h)
                        , s == "unknown"
                        ]
                 in counterexample (show (hitJson h)) (null bad)
    it "every emitted key is in the declared schema" $
        property $
            forAll genHit $ \h ->
                let bad = [k | (k, _) <- fieldsOf (hitJson h), k `notElem` hitKeys]
                 in counterexample (show bad) (null bad)
    it "every required key is present for any hit the harness can build" $
        property $
            forAll genHit $ \h ->
                let missing =
                        [ k
                        | k <- requiredHitKeys
                        , k `notElem` map fst (fieldsOf (hitJson h))
                        ]
                 in counterexample (show (hitJson h, missing)) (null missing)

sizeOf :: DHit -> Int
sizeOf = fromIntegral . LBS.length . encode . hitJson

{- | Adding no information must not add bytes: blanking a field can only make
the rendering smaller or leave it alone.
-}
monotoneSpec :: Spec
monotoneSpec = describe "the rendering is monotone in what was computed" $
    it "blanking a field never grows the hit" $
        property $
            forAll genHit $ \h ->
                let blanked =
                        [ h{dhModule = ""}
                        , h{dhPackage = ""}
                        , h{dhVersion = ""}
                        , h{dhType = ""}
                        ]
                    bad = [b | b <- blanked, sizeOf b > sizeOf h]
                 in counterexample
                        (show (map (T.pack . show . hitJson) bad))
                        (null bad)
