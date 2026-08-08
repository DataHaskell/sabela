{-# LANGUAGE OverloadedStrings #-}

{- | Wave-6 W6-B: the retrieval rank key reads the measured popularity table
and the module path, and nothing else — not a package's name, not its shape,
not its length.
-}
module Test.HoogleRankSpec (spec) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.HoogleResolve (
    HoogleHit (..),
    rankHitsInScopeWith,
    rankResolveTopKWith,
 )
import Sabela.AI.ModuleResolve (isOutOfScopePackage)
import Sabela.AI.Popularity (
    PackagePrior (..),
    emptyPopularity,
    packagePrior,
    packageRankKey,
    popularityFromList,
    popularityRank,
 )

{- | A package name of one to four hyphen-separated segments. The spread of
segment counts and lengths is the point: it straddles every threshold a
name-shape heuristic could read, so a key that consults the name diverges here.
-}
genPkg :: Gen Text
genPkg = do
    n <- choose (1, 4)
    T.intercalate "-" <$> vectorOf n genSeg

-- | Long enough that a segment never collides with a toolchain package name.
genSeg :: Gen Text
genSeg = do
    k <- choose (5, 8)
    T.pack <$> vectorOf k (elements ['a' .. 'z'])

genIdent :: Gen Text
genIdent = do
    k <- choose (3, 9)
    T.pack <$> vectorOf k (elements ['a' .. 'z'])

genModule :: Gen Text
genModule = do
    n <- choose (1, 3)
    T.intercalate "." <$> vectorOf n cap
  where
    cap = do
        c <- elements ['A' .. 'Z']
        k <- choose (2, 6)
        cs <- vectorOf k (elements ['a' .. 'z'])
        pure (T.pack (c : cs))

{- | Two package names differing in both of the things a name-shape heuristic
reads: how many hyphens split them, and how long they are.
-}
genShapePair :: Gen (Text, Text)
genShapePair = do
    a <- genSeg
    b <- genSeg
    c <- genSeg
    pure (a, T.intercalate "-" [a, b, c])

-- | Two module names of equal length, differing in their final character.
genModulePair :: Gen (Text, Text)
genModulePair = do
    m <- genModule
    c <- elements ['a' .. 'z']
    let swapped = T.init m <> T.singleton c
    if swapped == m then genModulePair else pure (m, swapped)

-- | Downloads and reverse dependencies, the two columns the table carries.
genCounts :: Gen (Int, Int)
genCounts = (,) <$> choose (0, 500000) <*> choose (0, 20000)

genTable :: Gen [(Text, (Int, Int))]
genTable = do
    n <- choose (0, 12)
    vectorOf n ((,) <$> genPkg <*> genCounts)

hit :: Text -> Text -> Text -> HoogleHit
hit n pkg modu = HoogleHit n pkg modu "" "" ""

-- | Two package names and the counts a table would carry for each.
genRankedPair :: Gen (Text, Text, (Int, Int), (Int, Int))
genRankedPair = do
    p <- genPkg
    q <- genPkg
    cp <- genCounts
    cq <- genCounts
    pure (p, q, cp, cq)

{- | Every package name Hackage is known to carry, from the same data directory
the ranker reads its table from. Real names, written down by nobody here.
-}
packageUniverse :: IO [Text]
packageUniverse = do
    dir <- fromMaybe "data" <$> lookupEnv "SABELA_DATA_DIR"
    txt <- TIO.readFile (dir </> "hackage-packages.txt")
    pure (filter (not . T.null) (map T.strip (T.lines txt)))

spec :: Spec
spec = describe "Sabela.AI.HoogleRank (measured prior)" $ do
    priorSpec
    resolveSpec
    universeSpec
    scopeSpec

priorSpec :: Spec
priorSpec = describe "the prior a package is ranked by" $ do
    prop "a package the table measures higher outranks one it measures lower" $
        forAll genRankedPair $ \(p, q, cp, cq) ->
            let pop = popularityFromList [(p, cp), (q, cq)]
             in p
                    /= q
                    && popularityRank pop p > popularityRank pop q
                    ==> packageRankKey pop p < packageRankKey pop q

    prop "two packages measured alike rank alike, whatever their names" $
        forAll ((,,) <$> genPkg <*> genPkg <*> genCounts) $ \(p, q, c) ->
            let pop = popularityFromList [(p, c), (q, c)]
             in p /= q ==> packageRankKey pop p === packageRankKey pop q

    prop "a package the table never mentions is unmeasured, not unpopular" $
        forAll ((,,,,) <$> genPkg <*> genPkg <*> genPkg <*> genCounts <*> genCounts) $
            \(hi, lo, absent, chi, clo) ->
                let pop = popularityFromList [(hi, chi), (lo, clo)]
                    distinct = length (Set.fromList [hi, lo, absent]) == 3
                 in distinct && popularityRank pop hi > popularityRank pop lo ==>
                        counterexample (show (map (packagePrior pop) [hi, lo, absent])) $
                            conjoin
                                [ packagePrior pop absent === Unmeasured
                                , packagePrior pop hi === WellUsed (popularityRank pop hi)
                                , packagePrior pop lo === LittleUsed (popularityRank pop lo)
                                , property (packageRankKey pop hi < packageRankKey pop absent)
                                , property (packageRankKey pop absent < packageRankKey pop lo)
                                ]

    prop "a table with no spread cannot rank absence below its members" $
        forAll ((,,,) <$> genPkg <*> genPkg <*> genPkg <*> genCounts) $
            \(p, q, absent, c) ->
                let pop = popularityFromList [(p, c), (q, c)]
                 in length (Set.fromList [p, q, absent])
                        == 3
                        ==> conjoin
                            [ packagePrior pop p === Unmeasured
                            , packageRankKey pop absent === packageRankKey pop p
                            ]

    prop "with no table at all every package gets one and the same key" $
        forAll ((,) <$> genPkg <*> genPkg) $ \(p, q) ->
            conjoin
                [ packageRankKey emptyPopularity p
                    === packageRankKey emptyPopularity q
                , packagePrior emptyPopularity p === Unmeasured
                ]

resolveSpec :: Spec
resolveSpec = describe "exact-name resolution reads the prior" $ do
    prop "the better-measured package's module leads, either input order" $
        forAll ((,,) <$> genRankedPair <*> genIdent <*> genModule) $
            \((p, q, cp, cq), name, modu) ->
                let pop = popularityFromList [(p, cp), (q, cq)]
                    other = modu <> "x"
                    hp = hit name p modu
                    hq = hit name q other
                    winner hs = take 1 (map fst (rankResolveTopKWith pop 2 name Nothing hs))
                 in p
                        /= q
                        && popularityRank pop p > popularityRank pop q
                        ==> conjoin
                            [ winner [hp, hq] === [p]
                            , winner [hq, hp] === [p]
                            ]

    prop "measured alike, the module decides and the package cannot" $
        forAll ((,,,) <$> genShapePair <*> genCounts <*> genIdent <*> genModulePair) $
            \((p, q), c, name, (m1, m2)) ->
                let pop = popularityFromList [(p, c), (q, c)]
                    won hs = map snd (rankResolveTopKWith pop 2 name Nothing hs)
                    inOrder = [min m1 m2, max m1 m2]
                 in conjoin
                        [ won [hit name p m1, hit name q m2] === inOrder
                        , won [hit name q m1, hit name p m2] === inOrder
                        ]

    prop "a lone exact hit resolves to its own package and module" $
        forAll ((,,,) <$> genIdent <*> genPkg <*> genModule <*> genTable) $
            \(name, pkg, modu, tbl) ->
                rankResolveTopKWith
                    (popularityFromList tbl)
                    1
                    name
                    Nothing
                    [hit name pkg modu]
                    === [(pkg, modu)]

    prop "a table that measures neither package leaves the order alone"
        $ forAll
            ((,,,,) <$> genIdent <*> genPkg <*> genPkg <*> genModulePair <*> genTable)
        $ \(name, p, q, (m1, m2), tbl) ->
            let hs = [hit name p m1, hit name q m2]
                go pop = rankResolveTopKWith pop 2 name Nothing hs
                unrelated = filter ((`notElem` [p, q]) . fst) tbl
             in p
                    /= q
                    ==> go emptyPopularity
                        === go (popularityFromList unrelated)

{- | The generated names above can never rediscover a hard-coded allowlist, so
this one asks the same question of every name Hackage has.
-}
universeSpec :: Spec
universeSpec = describe "the whole package universe, not a sample" $
    it "orders by module for every real name but the excluded toolchain" $ do
        universe <- packageUniverse
        length universe `shouldSatisfy` (> 10000)
        let name = "wqjxv"
            lo = "Zaa.Bba"
            hi = "Zaa.Bbb"
            control = "qqzzwwxx-vvttrr"
            order p =
                map snd $
                    rankResolveTopKWith
                        emptyPopularity
                        2
                        name
                        Nothing
                        [hit name p hi, hit name control lo]
            deviating = filter (\p -> order p /= [lo, hi]) universe
            excluded = filter isOutOfScopePackage universe
        (length deviating, take 6 deviating)
            `shouldBe` (length excluded, take 6 excluded)

scopeSpec :: Spec
scopeSpec = describe "scope still dominates the prior" $ do
    prop "with nothing in scope the better-measured package's hit leads" $
        forAll ((,,,) <$> genRankedPair <*> genIdent <*> genIdent <*> genModulePair) $
            \((p, q, cp, cq), n1, n2, (m1, m2)) ->
                let pop = popularityFromList [(p, cp), (q, cq)]
                    ranked =
                        rankHitsInScopeWith pop Set.empty [hit n1 q m1, hit n2 p m2]
                 in p
                        /= q
                        && popularityRank pop p > popularityRank pop q
                        ==> map hhPackage ranked === [p, q]

    prop "an in-scope hit leads a better-measured out-of-scope one" $
        forAll ((,,,) <$> genRankedPair <*> genIdent <*> genIdent <*> genModule) $
            \((p, q, cp, cq), n1, n2, modu) ->
                let pop = popularityFromList [(p, cp), (q, cq)]
                    ranked =
                        rankHitsInScopeWith
                            pop
                            (Set.singleton q)
                            [hit n1 p modu, hit n2 q (modu <> "z")]
                 in p
                        /= q
                        && popularityRank pop p > popularityRank pop q
                        ==> map hhPackage ranked === [q, p]
