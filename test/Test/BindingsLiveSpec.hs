{-# LANGUAGE OverloadedStrings #-}

{- | R6.5 kernel truth for @list_bindings@: GHCi keeps replaced cells'
bindings alive, but only what a CURRENT cell defines (Repair.snapshot's own
source) may report live; the rest is flagged stale, never current state.
-}
module Test.BindingsLiveSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.ValueEcho (liveBindingsReport, partitionLive, staleNote)

-- | One notebook mutation in the generated histories.
data Op = InsertCell [Text] | ReplaceLast [Text] | DeleteLast
    deriving (Show)

opPool :: [Op]
opPool =
    [ InsertCell ["a"]
    , InsertCell ["b", "c"]
    , ReplaceLast ["d"]
    , ReplaceLast []
    , DeleteLast
    ]

-- | Every op sequence up to length 3 over the pool (deterministic, 156 cases).
histories :: [[Op]]
histories = concat [sequencesOf n | n <- [0 .. 3 :: Int]]
  where
    sequencesOf 0 = [[]]
    sequencesOf n = [op : rest | op <- opPool, rest <- sequencesOf (n - 1)]

{- | Interpret a history: the current cells' define-sets plus every name the
session ever saw (GHCi keeps replaced cells' bindings alive).
-}
runHistory :: [Op] -> ([[Text]], Set.Set Text)
runHistory = foldl step ([], Set.empty)
  where
    step (cells, ever) (InsertCell ns) = (cells ++ [ns], grow ever ns)
    step ([], ever) (ReplaceLast _) = ([], ever)
    step (cells, ever) (ReplaceLast ns) = (init cells ++ [ns], grow ever ns)
    step ([], ever) DeleteLast = ([], ever)
    step (cells, ever) DeleteLast = (init cells, ever)
    grow ever ns = ever <> Set.fromList ns

sessionListing :: Set.Set Text -> Text
sessionListing ever =
    T.unlines [n <> " :: Int = _" | n <- Set.toAscList ever]

listingNames :: Text -> Set.Set Text
listingNames t =
    Set.fromList
        [ T.strip (fst (T.breakOn " :: " l))
        | l <- T.lines t
        , not (T.null (T.strip l))
        ]

spec :: Spec
spec = describe "list_bindings live-vs-stale truth (R6.5)" $ do
    describe "partitionLive over generated cell-replacement histories" $
        it "reported-live == union of current defines; stale == session leftovers" $
            mapM_
                ( \ops -> do
                    let (cells, ever) = runHistory ops
                        current = Set.fromList (concat cells)
                        (live, stale) = partitionLive (concat cells) (sessionListing ever)
                    (map show ops, listingNames live)
                        `shouldBe` (map show ops, current)
                    (map show ops, Set.fromList stale)
                        `shouldBe` (map show ops, ever `Set.difference` current)
                )
                histories

    describe "the symbolicRegression fixture (resultText/resultMarkdown)" $ do
        let session =
                T.unlines
                    [ "best :: (String, Double) = _"
                    , "resultMarkdown :: String = _"
                    , "resultText :: String = _"
                    ]
            currentDefines = ["best"]
        it "a replaced cell's old bindings are never presented as live" $ do
            let (live, stale) = partitionLive currentDefines session
            listingNames live `shouldBe` Set.fromList ["best"]
            Set.fromList stale
                `shouldBe` Set.fromList ["resultMarkdown", "resultText"]
        it "the composed report lists live bindings and FLAGS the stale ones" $ do
            let report = liveBindingsReport currentDefines (const (Just "42")) session
            report `shouldSatisfy` T.isInfixOf "best :: (String, Double)"
            report `shouldSatisfy` T.isInfixOf "stale"
            report `shouldSatisfy` T.isInfixOf "resultText"
            -- Stale names never appear as a plain binding line.
            filter ("resultText ::" `T.isInfixOf`) (T.lines report) `shouldBe` []
            filter ("resultMarkdown ::" `T.isInfixOf`) (T.lines report)
                `shouldBe` []

    describe "staleNote" $ do
        it "is empty when nothing is stale" $
            staleNote [] `shouldBe` ""
        it "is one bounded line naming the stale bindings as not current" $ do
            let note = staleNote ["resultText", "resultMarkdown"]
            note `shouldSatisfy` T.isInfixOf "no current cell defines"
            T.length note `shouldSatisfy` (<= 250)
            T.count "\n" note `shouldSatisfy` (<= 1)
        it "stays bounded for many stale names" $
            T.length (staleNote [T.pack ("b" <> show i) | i <- [1 .. 200 :: Int]])
                `shouldSatisfy` (<= 250)
