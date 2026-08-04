{-# LANGUAGE OverloadedStrings #-}

{- | A1: a blank package is unknown, never a wildcard. Colliding names across
packages are the normal condition in Haskell, so a scope keyed on the name
alone admits every namesake and lends it the module the caller asked for.
Stated over generated names, modules and packages.
-}
module Test.DiscoverHomonymSpec (discoverHomonymSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Siza.Agent.Discover.Classify (sessionAnswer)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelopeScoped)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
    emptyScope,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitsOf, textField)
import Test.DiscoverGen (genModulePair, genPkgPair, genQualName)

discoverHomonymSpec :: Spec
discoverHomonymSpec = describe "attribution is evidence, not a namesake (A1)" $ do
    scopeSpec
    evidenceSpec
    sessionPackageSpec

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True [] []

envelope :: Text -> Scope -> Int -> [SourceAnswer] -> HackageInfo -> Value
envelope q = discoverEnvelopeScoped envT (interpret envT q)

sessionHit :: Text -> Text -> DHit
sessionHit n m =
    (mkHit n m ""){dhInstall = InstInstalled, dhOrigin = "session"}

foreignHit :: Text -> Text -> Text -> DHit
foreignHit n m p =
    (mkHit n m p){dhType = "Int -> Int", dhOrigin = "hoogle"}

modulesOf :: Value -> [Text]
modulesOf v = map (textField "module") (hitsOf v)

packagesOf :: Value -> [Text]
packagesOf v = map (textField "package") (hitsOf v)

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number n) -> round n
    _ -> -1

{- | The episode's shape: one hit the notebook holds in the scoped module, and
namesakes elsewhere whose only relation to it is the bare name.
-}
data Homonyms = Homonyms
    { hoName :: Text
    , hoHere :: Text
    , hoThere :: Text
    , hoPackage :: Text
    , hoImpostors :: Int
    }
    deriving (Show)

genHomonyms :: Gen Homonyms
genHomonyms = do
    n <- genQualName
    (here, there) <- genModulePair
    (_, p) <- genPkgPair
    k <- choose (1, 4)
    pure (Homonyms n here there p k)

impostorHits :: Homonyms -> [DHit]
impostorHits h =
    [ foreignHit (hoName h) (hoThere h <> ".M" <> T.pack (show i)) (hoPackage h)
    | i <- [1 .. hoImpostors h]
    ]

scopedEnvelope :: Homonyms -> Int -> Value
scopedEnvelope h limit =
    envelope
        (hoName h)
        (Scope (Just (hoHere h)) Nothing)
        limit
        [ okAnswer "session" [sessionHit (hoName h) (hoHere h)]
        , okAnswer "hoogle" (impostorHits h)
        ]
        hkT

scopeSpec :: Spec
scopeSpec = describe "a module scope keeps what the module holds" $ do
    prop "no hit survives on a namesake in another module" $
        forAll genHomonyms $ \h ->
            let v = scopedEnvelope h 20
             in counterexample (show (modulesOf v)) $
                    modulesOf v === [hoHere h]
    prop "the scoped hit survives the limit its impostors used to spend" $
        forAll genHomonyms $ \h ->
            let v = scopedEnvelope h 1
             in counterexample (show (modulesOf v, intField "total" v)) $
                    conjoin
                        [ modulesOf v === [hoHere h]
                        , intField "total" v === 1
                        , intField "omitted" v === 0
                        ]
    prop "a package scope keeps no hit whose package is unknown" $
        forAll genHomonyms $ \h ->
            let v =
                    envelope
                        (hoName h)
                        (Scope Nothing (Just (hoPackage h)))
                        20
                        [ okAnswer "session" [sessionHit (hoName h) (hoHere h)]
                        , okAnswer "hoogle" (impostorHits h)
                        ]
                        hkT
             in counterexample (show (packagesOf v)) $
                    packagesOf v === replicate (hoImpostors h) (hoPackage h)
    prop "unscoped, every candidate is still returned" $
        forAll genHomonyms $ \h ->
            let v =
                    envelope
                        (hoName h)
                        emptyScope
                        20
                        [ okAnswer "session" [sessionHit (hoName h) (hoHere h)]
                        , okAnswer "hoogle" (impostorHits h)
                        ]
                        hkT
             in intField "total" v === 1 + hoImpostors h

{- | The same law where the session's own evidence is read: a fact recorded
under no package says nothing about the package a hit names.
-}
evidenceSpec :: Spec
evidenceSpec = describe "install state is evidence about a package" $ do
    prop "package-less evidence about a contested module proves nothing" $
        forAll genEvidenceCase $ \c ->
            counterexample (show (installStates c [owner c, rival c])) $
                property ("installed" `notElem` installStates c [owner c, rival c])
    prop "package-less evidence about a module one package owns still holds" $
        forAll genEvidenceCase $ \c ->
            counterexample (show (installStates c [owner c])) $
                installStates c [owner c] === ["installed"]
    prop "a module two packages claim leaves the package unattributed" $
        forAll ((,,) <$> genQualName <*> genModulePair <*> genPkgPair) $
            \(n, (m, _), (p1, p2)) ->
                let answers =
                        [ (okAnswer "session" [sessionHit n m])
                            { saPkgModules = [(p1, [m]), (p2, [m])]
                            }
                        ]
                    v = envelope n emptyScope 20 answers hkT
                 in counterexample (show (packagesOf v, field "narrow" v)) $
                        conjoin
                            [ packagesOf v === [""]
                            , property (m `T.isInfixOf` textField "narrow" v)
                            , property (p1 `T.isInfixOf` textField "narrow" v)
                            , property (p2 `T.isInfixOf` textField "narrow" v)
                            ]
    prop "an owner the sources agree on still attributes" $
        forAll ((,,) <$> genQualName <*> genModulePair <*> genPkgPair) $
            \(n, (m, _), (p, _)) ->
                let answers =
                        [ (okAnswer "session" [sessionHit n m])
                            { saPkgModules = [(p, [m])]
                            }
                        ]
                 in packagesOf (envelope n emptyScope 20 answers hkT) === [p]

{- | The session reached a module; an index says which packages hold a module
of that name. Whether that names the hit's package is the question.
-}
data Evidence = Evidence
    { evSeen :: Text
    , evWanted :: Text
    , evModule :: Text
    , owner :: Text
    , rival :: Text
    }
    deriving (Show)

genEvidenceCase :: Gen Evidence
genEvidenceCase = do
    seen <- genQualName
    wanted <- genQualName `suchThat` (/= seen)
    (m, _) <- genModulePair
    (p1, p2) <- genPkgPair
    pure (Evidence seen wanted m p1 p2)

{- | What the envelope says about the wanted name, when the session reached the
module without naming a package and the listed packages claim it.
-}
installStates :: Evidence -> [Text] -> [Text]
installStates c claimants =
    [ textField "install" hj
    | hj <- hitsOf v
    , textField "name" hj == evWanted c
    ]
  where
    v =
        envelope
            (evWanted c)
            emptyScope
            20
            [ okAnswer "session" [sessionHit (evSeen c) (evModule c)]
            , (okAnswer "hoogle" [foreignHit (evWanted c) (evModule c) (owner c)])
                { saPkgModules = [(p, [evModule c]) | p <- claimants]
                }
            ]
            (HackageInfo True [owner c] [])

{- | A3 client line and the wire half of A1: what a session match states is
what the hit carries, and the record-update syntax describes a field rather
than a way to reach the name.
-}
sessionPackageSpec :: Spec
sessionPackageSpec = describe "a session match is read, not overwritten" $ do
    prop "the package a match states is the package the hit carries" $
        forAll ((,,) <$> genQualName <*> genModulePair <*> genPkgPair) $
            \(n, (m, _), (p, _)) ->
                map dhPackage (matchHits n [("package", String p), ("module", String m)])
                    === [p]
    prop "a match that states no package leaves it unknown" $
        forAll ((,) <$> genQualName <*> genModulePair) $
            \(n, (m, _)) ->
                map dhPackage (matchHits n [("module", String m)]) === [""]
    prop "record-update syntax never stands as the whole use line" $
        forAll ((,,) <$> genQualName <*> genModulePair <*> arbitrary) $
            \(n, (m, _), withImport) ->
                let fld = "Opts { " <> n <> " = ... }"
                    imp = "import " <> m <> " (" <> n <> ")"
                    extra =
                        ("field", String fld)
                            : [("import", String imp) | withImport]
                    uses = map dhUse (matchHits n extra)
                 in counterexample (show uses) $
                        conjoin
                            [ property (Just fld `notElem` uses)
                            , property (not withImport || uses == [Just (imp <> "; record update: " <> fld)])
                            ]

matchHits :: Text -> [(Text, Value)] -> [DHit]
matchHits n extra =
    saHits (sessionAnswer (nameInterp n) (Just (object ["matches" .= [match]])))
  where
    match =
        object
            ( [ "name" .= n
              , "type" .= ("Int -> Int" :: Text)
              , "via" .= ("name" :: Text)
              ]
                <> [K.fromText k .= v | (k, v) <- extra]
            )

nameInterp :: Text -> Interpreted
nameInterp n = Interpreted n n Nothing "name" "" []
