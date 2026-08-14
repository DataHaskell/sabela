{-# LANGUAGE OverloadedStrings #-}

{- | LIVE-TRIAGE 1-2: what the harness says about the world is what it computed.
An import line is emitted only for a name Haskell can write in one, an import
list names only an export a source returned, and an install state is the state
of the session's own evidence for the package the hit names.
-}
module Test.DiscoverWorldTruthSpec (discoverWorldTruthSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Discover.Classify (capabilityAnswer)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    MatchKind (..),
    SourceAnswer,
    mkHit,
    okAnswer,
 )
import Siza.Lang.Haskell (parseHaskell)
import Test.DiscoverFixtures (hitText, hitsOf)
import Test.DiscoverGen (genModulePair, genPkgName, genValueName)
import Test.LadderFixtures (encodeT, envT)

discoverWorldTruthSpec :: Spec
discoverWorldTruthSpec =
    describe "the harness states only what it computed about the world" $ do
        useLegalSpec
        importListSpec
        installEvidenceSpec
        computedFieldSpec

{- | A package answer as the index returns it: the package, the modules it
exposes, and the exports it lists inside one of them.
-}
indexAnswer :: Text -> Text -> [Text] -> SourceAnswer
indexAnswer pkg m ns = capabilityAnswer (interpret envT pkg) (Just payload)
  where
    payload =
        object
            [ "hits"
                .= [ object
                        [ "package" .= pkg
                        , "version" .= ("1.2" :: Text)
                        , "cabal" .= ("-- cabal: build-depends: " <> pkg)
                        , "modules" .= [m]
                        , "api" .= map apiRow ns
                        ]
                   ]
            ]
    apiRow n =
        object
            [ "name" .= n
            , "module" .= m
            , "type" .= ("FilePath -> IO ()" :: Text)
            ]

-- | The envelope a query over an indexed package produces.
indexEnvelope :: Text -> Text -> [Text] -> [SourceAnswer] -> Value
indexEnvelope pkg m ns extra =
    discoverEnvelope
        envT
        (interpret envT pkg)
        20
        (extra ++ [indexAnswer pkg m ns])
        (HackageInfo True [pkg] [] [])

{- | Names an index carries that Haskell cannot write where an import expects
one: a row can hold a package, a qualified spelling or a phrase.
-}
genUnwritableName :: Gen Text
genUnwritableName =
    elements ["read all", "to-list", "Some.Thing.get", "a b c", "x/y"]

{- | An index row that names no package: the module is all it states, so
nothing attributes the module to a package on this path.
-}
flatAnswer :: Text -> Text -> SourceAnswer
flatAnswer m n =
    capabilityAnswer (interpret envT n) (Just payload)
  where
    payload =
        object
            [ "hits"
                .= [ object
                        [ "name" .= n
                        , "module" .= m
                        , "type" .= ("FilePath -> IO ()" :: Text)
                        ]
                   ]
            ]

{- | The session's answer for a module it reached only through a package that
is present but not loaded: the shape a hidden-package card produces.
-}
sessionHidden :: Text -> Text -> SourceAnswer
sessionHidden m pkg =
    okAnswer
        "session"
        [ (mkHit pkg m pkg)
            { dhInstall = InstHidden
            , dhOrigin = "session"
            , dhKind = MkModule
            }
        ]

useLegalSpec :: Spec
useLegalSpec = describe "an emitted import line is Haskell (LIVE-TRIAGE 1)" $ do
    it "a package the index names is never spelled into an import list" $
        property $
            forAll ((,,) <$> genPkgName <*> genModulePair <*> genValueName) $
                \(pkg, (m, _), n) -> allImportsParse (indexEnvelope pkg m [n] [])
    it "a row whose name Haskell cannot write yields no import line" $
        property $
            forAll ((,,) <$> genPkgName <*> genModulePair <*> genValueName) $
                \(pkg, (m, _), n) ->
                    forAll genUnwritableName $ \odd' ->
                        allImportsParse (indexEnvelope pkg m [n, odd'] [])

allImportsParse :: Value -> Property
allImportsParse v = counterexample (show (encodeT v, bad)) (null bad)
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

{- | The import list is a claim that the module exports that name. A hit that
stands for a package carries the package's own name, which is not an export.
-}
importListSpec :: Spec
importListSpec = describe "an import list names an export, not a package" $
    it "a hit whose name is its package's name lists nothing" $
        property $
            forAll ((,,) <$> genPkgName <*> genModulePair <*> genValueName) $
                \(pkg, (m, _), n) ->
                    let v = indexEnvelope pkg m [n] []
                        bad =
                            [ (hitText "name" hj, u)
                            | hj <- hitsOf v
                            , hitText "name" hj == hitText "package" hj
                            , T.null (hitText "type" hj)
                            , let u = hitText "use" hj
                            , "(" `T.isInfixOf` u
                            ]
                     in counterexample (show (encodeT v, bad)) (null bad)

{- | LIVE-TRIAGE 2: installation is a fact about this session. Evidence the
session recorded for one package establishes nothing about another, and
evidence that a module is present but not loaded does not become "installed".
-}
installEvidenceSpec :: Spec
installEvidenceSpec = describe "install is the session's own evidence" $ do
    it "a package the session never reached is not called installed" $
        property $
            forAll ((,,,) <$> genPkgName <*> genPkgName <*> genModulePair <*> genValueName) $
                \(pkg, other, (m, _), n) ->
                    pkg
                        /= other
                        ==> let v = indexEnvelope pkg m [n] [sessionHidden m other]
                                bad =
                                    [ (hitText "package" hj, hitText "install" hj)
                                    | hj <- hitsOf v
                                    , hitText "package" hj == pkg
                                    , hitText "install" hj == "installed"
                                    ]
                             in counterexample (show (encodeT v, bad)) (null bad)
    it "a module reached only through a package that is not loaded" $
        property $
            forAll ((,,) <$> genPkgName <*> genModulePair <*> genValueName) $
                \(pkg, (m, _), n) ->
                    let v =
                            discoverEnvelope
                                envT
                                (interpret envT n)
                                20
                                [sessionHidden m pkg, flatAnswer m n]
                                (HackageInfo True [] [] [])
                        bad =
                            [ (hitText "name" hj, hitText "install" hj)
                            | hj <- hitsOf v
                            , hitText "module" hj == m
                            , hitText "install" hj == "installed"
                            ]
                     in counterexample (show (encodeT v, bad)) (null bad)

{- | The other half of "computed or absent": a module a source did state is
carried through to the payload, not dropped with the placeholders.
-}
computedFieldSpec :: Spec
computedFieldSpec = describe "a computed field is stated" $
    it "the module and package a source returned are the ones stated" $
        property $
            forAll ((,,) <$> genPkgName <*> genModulePair <*> genValueName) $
                \(pkg, (m, _), n) ->
                    let v = indexEnvelope pkg m [n] []
                        mods = [hitText "module" hj | hj <- hitsOf v]
                        pkgs = [hitText "package" hj | hj <- hitsOf v]
                     in counterexample (show (encodeT v)) $
                            conjoin
                                [ property (not (null mods))
                                , property (all (== m) mods)
                                , property (all (== pkg) pkgs)
                                ]
