{-# LANGUAGE OverloadedStrings #-}

{- | C1-8: the notebook/session context a hit sits in decides before the shape
of its signature does. Stated over generated hit pairs, so it says nothing
about any particular library.
-}
module Test.DiscoverContextBandSpec (
    contextBandSpec,
    sigWith,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Rank (RankKey, rankKeyRecent)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    SourceAnswer (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (hitText, hitsOf)

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

sigWith :: Int -> Int -> Text
sigWith c t = constraints <> "Text -> Expr a" <> tyArgs
  where
    constraints
        | c <= 0 = ""
        | otherwise =
            "("
                <> T.intercalate ", " ["C" <> tShow i <> " a" | i <- [1 .. c]]
                <> ") => "
    tyArgs = T.concat [" @S" <> tShow i | i <- [1 .. t]]
    tShow = T.pack . show

contextBandSpec :: Spec
contextBandSpec = describe "context outranks signature shape (C1-8)" $ do
    prop "an implicated hit leads, whether or not its package resolved" $
        forAll genContextCase $ \c ->
            counterexample (show c) $
                property (rankOf c (ctxHit c) < rankOf c (farHit c))
    prop "a session hit outranks a package nothing implicates" $
        forAll genUnresolvedCase $ \c ->
            counterexample (show c) $
                property (rankOf c (ctxHit c) < rankOf c (farHit c))
    prop "with nothing implicated, plainness still decides" $
        forAll genPlainnessCase $ \c ->
            counterexample (show c) $
                property (rankOf c (farHit c) < rankOf c (ctxHit c))
    prop "between two package-less hits, the implicated module's owner leads" $
        forAll genOwnedCase $ \c ->
            counterexample (show c) $
                attributedOrder c === [ccCtxModule c, ccFarModule c]

data Implication = ByImport | ByPackage | ByRecent | NotImplicated
    deriving (Eq, Show)

data ContextCase = ContextCase
    { ccName :: Text
    , ccCtxModule :: Text
    , ccFarModule :: Text
    , ccCtxPackage :: Text
    , ccFarPackage :: Text
    , ccConstraints :: Int
    , ccImplication :: Implication
    }
    deriving (Show)

{- | A hit the session context implicates, against a namesake with a plainer
signature. The far hit's package may be blank too — the shape a card answer
produces — and only an import implicates a hit whose package never resolved.
-}
genContextCase :: Gen ContextCase
genContextCase = do
    impl <- elements [ByImport, ByPackage, ByRecent]
    ctxPkg <- if impl == ByImport then elements ["", "pctx"] else pure "pctx"
    farPkg <- elements ["pfar", "otherfar", ""]
    baseCase impl ctxPkg farPkg

{- | The episode's shape: a package-less session hit against a resolved package
that nothing implicates, where the session hit carries the constraints.
-}
genUnresolvedCase :: Gen ContextCase
genUnresolvedCase = do
    farPkg <- elements ["pfar", "otherfar"]
    baseCase NotImplicated "" farPkg

-- | Both packages resolved and neither implicated: the tie plainness owns.
genPlainnessCase :: Gen ContextCase
genPlainnessCase = do
    farPkg <- elements ["pfar", "otherfar"]
    baseCase NotImplicated "pctx" farPkg

{- | Neither hit resolved a package of its own — the shape every session hit
has — so only the answers' module attribution says which one the notebook
implicates.
-}
genOwnedCase :: Gen ContextCase
genOwnedCase = baseCase ByPackage "" ""

baseCase :: Implication -> Text -> Text -> Gen ContextCase
baseCase impl ctxPkg farPkg =
    ContextCase
        <$> elements ["col", "insert", "empty", "null", "filter", "lookup", "pack"]
        <*> elements ["Ctx.One", "Ctx.Two", "Ctx.Three"]
        <*> elements ["Far.One", "Far.Two", "Far.Three"]
        <*> pure ctxPkg
        <*> pure farPkg
        <*> choose (1, 3)
        <*> pure impl

ctxHit :: ContextCase -> DHit
ctxHit c =
    (mkHit (ccName c) (ccCtxModule c) (ccCtxPackage c))
        { dhType = sigWith (ccConstraints c) 0
        , dhInstall = InstInstalled
        , dhOrigin = originOf (ccCtxPackage c)
        }

farHit :: ContextCase -> DHit
farHit c =
    (mkHit (ccName c) (ccFarModule c) (ccFarPackage c))
        { dhType = sigWith 0 0
        , dhInstall = InstInstalled
        , dhOrigin = originOf (ccFarPackage c)
        }

-- | A hit with no package came from the session; only a search resolves one.
originOf :: Text -> Text
originOf pkg = if T.null pkg then "session" else "hoogle"

rankOf :: ContextCase -> DHit -> RankKey
rankOf c = rankKeyRecent recent imported env interp
  where
    interp = interpret env (ccName c)
    env = case ccImplication c of
        ByImport -> seededBuiltins (NotebookEnv [] [ccCtxModule c] [] [] [] [])
        _ -> env0
    imported = case ccImplication c of
        ByPackage -> ["pctx"]
        _ -> []
    recent = case ccImplication c of
        ByRecent -> ["pctx"]
        _ -> []

{- | The modules the envelope ranks, for two package-less hits whose packages
only the answers' module attribution resolves. The notebook imports a sibling
module of the context hit's package and nothing of the far one's.
-}
attributedOrder :: ContextCase -> [Text]
attributedOrder c =
    map (hitText "module") (hitsOf (discoverEnvelope env interp 8 [answer] hk0))
  where
    interp = interpret env (ccName c)
    env = seededBuiltins (NotebookEnv [] [sibling] [] [] [] [])
    sibling = "Ctx.Sibling"
    answer =
        ( okAnswer
            "session"
            [sessionHit (ccFarModule c) 0, sessionHit (ccCtxModule c) (ccConstraints c)]
        )
            { saPkgModules =
                [ ("pctx", [ccCtxModule c, sibling])
                , ("pfar", [ccFarModule c])
                ]
            }
    sessionHit m k =
        (mkHit (ccName c) m "")
            { dhType = sigWith k 0
            , dhInstall = InstInstalled
            , dhOrigin = "session"
            }
