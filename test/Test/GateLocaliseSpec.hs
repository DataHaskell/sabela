{-# LANGUAGE OverloadedStrings #-}

{- | C1-18: a rejected multi-binding candidate should come back saying which
component stopped compiling and which prefix was measured green, so nothing has
to bisect the cell by committing probes.
-}
module Test.GateLocaliseSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Sabela.AI.Capabilities.Edit.GateFrontier.Localise (
    Component (..),
    Localisation (..),
    Split (..),
    localiseCandidate,
    prefixSource,
    splitCandidate,
 )
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
 )

goodBodies :: [Text]
goodBodies =
    [ "Data.Map.fromList [(1 :: Int, 'x')]"
    , "Data.Text.pack \"forty-two\""
    , "Data.Aeson.encode (1 :: Int)"
    , "length [1 .. 10 :: Int]"
    , "Zubu.Artefact.describe Zubu.Artefact.empty"
    ]

badBodies :: [Text]
badBodies =
    [ "notInScopeAnywhere"
    , "length 1 2 3"
    , "(1 :: Int) + \"text\""
    , "Data.Map.fromList 7 8"
    ]

preambleGrid :: [[Text]]
preambleGrid =
    [ []
    , ["import Data.Map"]
    , ["-- cabal: build-depends: containers", "import qualified Data.Map as M"]
    , ["{-# LANGUAGE ScopedTypeVariables #-}", "import Data.Aeson"]
    ]

genIdent :: Gen Text
genIdent = do
    n <- choose (2, 6) :: Gen Int
    c <- elements ['a' .. 'z']
    cs <- vectorOf (n - 1) (elements ['a' .. 'z'])
    pure (T.pack (c : cs))

data Case = Case
    { caseSource :: Text
    , caseNames :: [Text]
    , caseBadIndex :: Int
    }

instance Show Case where
    show c =
        "bad="
            <> T.unpack (caseNames c !! caseBadIndex c)
            <> "\n"
            <> T.unpack (caseSource c)

genCase :: Gen Case
genCase = do
    n <- choose (2, 5) :: Gen Int
    names <- distinctIdents n
    k <- choose (0, n - 1)
    goods <- vectorOf n (elements goodBodies)
    bad <- elements badBodies
    preamble <- elements preambleGrid
    let bodies = [if i == k then bad else b | (i, b) <- zip [0 ..] goods]
        decls = [nm <> " = " <> b | (nm, b) <- zip names bodies]
        src =
            T.unlines (preamble <> ["" | not (null preamble)]) <> T.intercalate "\n\n" decls
    pure (Case src names k)

distinctIdents :: Int -> Gen [Text]
distinctIdents n = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc = do
        i <- genIdent
        if i `elem` acc then go k acc else go (k - 1) (i : acc)

green :: DisposableResult
green =
    DisposableResult
        "disposable_scratch"
        DisposableOk
        Nothing
        ""
        ""
        Nothing
        []
        []
        []

red :: Text -> DisposableResult
red diag = green{disposableVerdict = DisposableCompileError, disposableStderr = diag}

{- | A probe that fails exactly when the source defines the poisoned binder,
read at the start of a line so a longer name ending in it is not mistaken for
it. Counts its calls so the search's cost is a measured fact.
-}
countingProbe :: IORef Int -> Text -> Text -> IO DisposableResult
countingProbe counter badName src = do
    modifyIORef' counter (+ 1)
    pure $
        if any defines (T.lines src)
            then red ("error: [GHC-88464] Variable not in scope, in " <> badName)
            else green
  where
    defines l = (badName <> " =") `T.isPrefixOf` T.stripStart l

budget :: Int
budget = 6

spec :: Spec
spec = describe "staged minimisation of a rejected candidate" $ do
    prop "splitting a candidate loses none of its lines" $
        forAll genCase $ \c ->
            let split = splitCandidate (caseSource c)
                whole = prefixSource split (length (splitComponents split))
                lines' t = sort [T.strip l | l <- T.lines t, not (T.null (T.strip l))]
             in lines' whole === lines' (caseSource c)

    prop "one component per top-level binder, in source order" $
        forAll genCase $ \c ->
            map compName (splitComponents (splitCandidate (caseSource c)))
                === caseNames c

    prop "names the first failing component and the prefix that compiled" $
        forAll genCase $ \c -> monadicIO $ do
            let bad = caseNames c !! caseBadIndex c
            counter <- run (newIORef 0)
            found <-
                run
                    ( localiseCandidate
                        budget
                        (countingProbe counter bad)
                        (caseSource c)
                        (red "error: [GHC-88464] Variable not in scope")
                    )
            case found of
                Nothing -> assert False
                Just loc -> do
                    probes <- run (readIORef counter)
                    assert (locFirstFailing loc == bad)
                    assert (locProven loc == take (caseBadIndex c) (caseNames c))
                    assert (probes <= length (caseNames c))

    prop "the prefix it calls proven is a prefix it measured green" $
        forAll genCase $ \c -> monadicIO $ do
            let bad = caseNames c !! caseBadIndex c
            counter <- run (newIORef 0)
            found <-
                run
                    ( localiseCandidate
                        budget
                        (countingProbe counter bad)
                        (caseSource c)
                        (red "error")
                    )
            case found of
                Nothing -> assert False
                Just loc -> do
                    verdict <-
                        run (countingProbe counter bad (locProvenSource loc))
                    assert (disposableVerdict verdict == DisposableOk)

    it "says nothing about a candidate with a single component" $ do
        counter <- newIORef 0
        found <-
            localiseCandidate
                budget
                (countingProbe counter "a")
                "a = notInScopeAnywhere"
                (red "error")
        found `shouldBe` Nothing

    it "spends no probes on a candidate that compiled" $ do
        counter <- newIORef 0
        found <-
            localiseCandidate budget (countingProbe counter "b") "a = 1\n\nb = 2" green
        found `shouldBe` Nothing
        readIORef counter `shouldReturn` 0
