{-# LANGUAGE OverloadedStrings #-}

{- | Generators for the discover-surface properties: arbitrary identifiers,
module names, packages and hits, so no property depends on the library or the
function any one episode happened to use.
-}
module Test.DiscoverGen (
    ScopeArgs (..),
    genClashGroup,
    genEnvOver,
    genGoalFreeRow,
    genHitOver,
    genMatchedStampCard,
    genModulePair,
    genOperator,
    genPkgName,
    genPkgPair,
    genQualName,
    genScopeArgs,
    genSessionHits,
    genSharedModuleAnswers,
    genValueName,
    seedModules,
    seedPackages,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    MatchKind (..),
    NotebookEnv (..),
    SourceAnswer (..),
    okAnswer,
 )
import Test.TruthGen (genIdent, genModuleName, genPackage)

-- | The seeded half of the module universe: real, unrelated libraries.
seedModules :: [Text]
seedModules =
    [ "Data.Map"
    , "Data.Map.Strict"
    , "Data.IntMap"
    , "Data.Text"
    , "Data.ByteString"
    , "Data.Aeson"
    , "Data.Aeson.Encoding"
    , "Control.Monad.State"
    ]

seedPackages :: [Text]
seedPackages = ["containers", "text", "bytestring", "aeson", "mtl"]

genValueName :: Gen Text
genValueName =
    oneof
        [ elements ["member", "null", "encode", "insertWith", "splitOn", "get"]
        , genIdent
        ]

genOperator :: Gen Text
genOperator = elements [".=", "<>", "!?", "|>", "<+>"]

-- | A name a hit can carry: a plain value, an operator, or a type-level name.
genQualName :: Gen Text
genQualName = frequency [(6, genValueName), (2, genOperator), (2, genTypeName)]

genTypeName :: Gen Text
genTypeName = do
    c <- elements ['A' .. 'Z']
    rest <- listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z']))
    pure (T.pack (c : take 6 rest))

genModule :: Gen Text
genModule = oneof [elements seedModules, genModuleName]

genPkg :: Gen Text
genPkg = oneof [elements seedPackages, genPackage]

{- | Package names as Hackage spells them: hyphenated and suffixed as well as
plain. The package universe is wider than the identifier universe, and a
property drawn only from identifiers cannot see an import list that names one.
-}
genPkgName :: Gen Text
genPkgName = do
    n <- choose (1, 3)
    parts <- vectorOf n word
    suffix <- elements ["", "-th", "-io", "2"]
    pure (T.intercalate "-" parts <> suffix)
  where
    word = do
        c <- elements ['a' .. 'z']
        k <- choose (2, 5)
        rest <- vectorOf k (elements ['a' .. 'z'])
        pure (T.pack (c : rest))

{- | Two modules neither of which is a substring of the other, so a property
that asserts one never appears cannot be satisfied by the other's spelling.
-}
genModulePair :: Gen (Text, Text)
genModulePair = do
    a <- genModule
    b <- genModule `suchThat` disjointText a
    pure (a, b)

-- | Two packages, disjoint the same way.
genPkgPair :: Gen (Text, Text)
genPkgPair = do
    a <- genPkg
    b <- genPkg `suchThat` disjointText a
    pure (a, b)

disjointText :: Text -> Text -> Bool
disjointText a b = a /= b && not (a `T.isInfixOf` b) && not (b `T.isInfixOf` a)

{- | A hit over a caller-supplied (name, module, package), with every other
field drawn across its whole domain and the optional fields independently
empty, so a property sees the uncomputed cases too.
-}
genHitOver :: Text -> Text -> Text -> Gen DHit
genHitOver n m p = do
    inst <- arbitraryBoundedEnum
    kind <- arbitraryBoundedEnum
    ver <- elements ["", "0.1.0", "2.3"]
    ty <- elements ["", "Int -> Int", "Map k v -> Bool"]
    origin <- elements ["session", "hoogle", "hackage"]
    pure (DHit n ty m p ver inst kind origin Nothing Nothing Nothing)

{- | Hits sharing one bare name across several modules, one of which the
harness did not compute — the shape a source answer with a missing or
stand-in @module@ field has, and the input a clash marker must not turn into
an import line.
-}
genClashGroup :: Gen (Text, [DHit])
genClashGroup = do
    n <- genQualName
    k <- choose (1, 4)
    mods <- nub <$> vectorOf k genModule
    blank <- elements ["", "(not installed)"]
    pkgs <- vectorOf (length mods + 1) genPkg
    pure (n, [clashHit n m p | (m, p) <- zip (blank : mods) pkgs])

clashHit :: Text -> Text -> Text -> DHit
clashHit n m p =
    DHit
        n
        "Int -> Int"
        m
        p
        ""
        InstInstalled
        MkExact
        "session"
        Nothing
        Nothing
        Nothing

{- | Session hits exactly as Classify.matchHit shapes them: origin "session",
blank package and version, installed, no cabal line — the attribution shape a
scope filter must treat as unknown, never as a wildcard.
-}
genSessionHits :: Gen [DHit]
genSessionHits = do
    k <- choose (1, 4)
    vectorOf k genSessionHit

genSessionHit :: Gen DHit
genSessionHit = do
    n <- genQualName
    m <- genModule
    ty <- elements ["", "Int -> Int", "Map k v -> Bool"]
    kind <- arbitraryBoundedEnum
    owner <- genTypeName
    use <- elements [Nothing, Just (owner <> " { " <> n <> " = ... }")]
    pure (DHit n ty m "" "" InstInstalled kind "session" Nothing use Nothing)

{- | Two source answers whose package listings both claim one module: distinct
packages, one shared module name, each answer's hit attributed to its own
package — the input an unambiguous attribution rule must refuse.
-}
genSharedModuleAnswers :: Gen (Text, SourceAnswer, SourceAnswer)
genSharedModuleAnswers = do
    m <- genModule
    (p1, p2) <- genPkgPair
    n1 <- genQualName
    n2 <- genQualName
    h1 <- genHitOver n1 m p1
    h2 <- genHitOver n2 m p2
    extra1 <- sublistOf seedModules
    extra2 <- sublistOf seedModules
    let a1 = (okAnswer "hoogle" [h1]){saPkgModules = [(p1, m : extra1)]}
        a2 = (okAnswer "hackage" [h2]){saPkgModules = [(p2, m : extra2)]}
    pure (m, a1, a2)

{- | A module card stamped @matched: no export matched '<q>'@ whose exports are
drawn to mention the query or not; the returned flag says which, so a property
can hold the stamp to what the exports actually contain.
-}
genMatchedStampCard :: Gen (Text, Value, Bool)
genMatchedStampCard = do
    q <- genValueName
    m <- genModule
    mentions <- arbitrary
    plain <- listOf1 (exportRow `suchThat` (not . T.isInfixOf q))
    rows <-
        if mentions
            then do
                ty <- elements exportTypes
                pos <- choose (0, length plain)
                let (before, after) = splitAt pos plain
                pure (before ++ [q <> " :: " <> ty] ++ after)
            else pure plain
    let card =
            object
                [ "module" .= m
                , "matched" .= ("no export matched '" <> q <> "'")
                , "exports" .= rows
                ]
    pure (q, card, mentions)
  where
    exportRow = do
        n <- genValueName
        ty <- elements exportTypes
        pure (n <> " :: " <> ty)

exportTypes :: [Text]
exportTypes = ["Int -> Int", "Map k v -> Bool", "Text"]

{- | Rows against a generated nominal goal type: genuine producers beside
axiom-shaped (bare type-variable result) and projection-shaped (consume the
goal) junk. Junk is junk by shape alone; every name is an ordinary identifier.
-}
genGoalFreeRow :: Gen (Text, [(Text, Text)], [(Text, Text)])
genGoalFreeRow = do
    goal <- genTypeName `suchThat` (`notElem` ["Int", "Text", "Map"])
    producers <- listOf1 (producerRow goal)
    junk <- listOf1 (junkRow goal)
    pure (goal, producers, junk)
  where
    producerRow g = do
        n <- genIdent
        ty <-
            elements
                [ g
                , "Int -> " <> g
                , "[Text] -> " <> g
                , "Int -> Int -> " <> g
                ]
        pure (n, ty)
    junkRow g = do
        n <- genIdent
        ty <-
            elements
                [ "a"
                , "a -> b"
                , "forall a. a"
                , g <> " -> a"
                , g <> " -> Int"
                ]
        pure (n, ty)

-- | A notebook environment whose imports are a random subset of the modules.
genEnvOver :: [Text] -> Gen NotebookEnv
genEnvOver mods = do
    imports <- sublistOf mods
    binds <- listOf genIdent
    pure (NotebookEnv [] imports [] (take 4 binds) [] [])

-- | One point of the {none, module=, package=, both} scope grid.
data ScopeArgs = ScopeArgs {saLabel :: String, saArgs :: Value}
    deriving (Show)

genScopeArgs :: Text -> Text -> Gen ScopeArgs
genScopeArgs m p =
    elements
        [ ScopeArgs "none" (args [])
        , ScopeArgs "module" (args [("module", String m)])
        , ScopeArgs "package" (args [("package", String p)])
        , ScopeArgs
            "module+package"
            (args [("module", String m), ("package", String p)])
        ]
  where
    args kvs = object [(K.fromText k, v) | (k, v) <- kvs]
