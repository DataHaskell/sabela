{-# LANGUAGE OverloadedStrings #-}

module Test.EmitLedgerSpec (emitLedgerSpec) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.SelfHeal (attachSelfHeal, selfHealNote)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.EmitLedger (
    backRefLimit,
    blockFloor,
    dedupText,
    eligibleBlocks,
    emptyEmitLedger,
 )
import Siza.Agent.Tools (renderOutcome)
import Test.DiscoverFixtures (textField)

srcA :: Text
srcA =
    T.unlines
        [ "-- cabal: build-depends: base"
        , "import Data.List (minimumBy)"
        , "import Data.Ord (comparing)"
        , "points :: [(Double, Double)]"
        , "points = [(1.0,1.0),(2.0,4.0),(3.0,9.0),(4.0,16.0)]"
        , "sse :: (Double -> Double) -> Double"
        , "sse f = sum [ (f x - y) ^ 2 | (x,y) <- points ]"
        , "best = minimumBy (comparing sse) candidates"
        ]

srcB :: Text
srcB = T.replace "best = minimumBy" "best = maximumBy" srcA

longFacts :: [Text]
longFacts =
    [ "`bars` :: [(Text, Double)] -> Plot -> Text — found in Granite.Svg"
    , "granite (hidden): -- cabal: build-depends: granite, text, containers"
    , "`plot` :: Plot — defaultPlot builds one (Granite.Svg, granite)"
    ]

toolOk :: Value -> Text
toolOk = renderOutcome . Right . ToolOk

readCellEcho :: Text -> Text
readCellEcho src =
    toolOk (object ["id" .= (0 :: Int), "error" .= Null, "source" .= src])

listCellsEcho :: Text -> Text
listCellsEcho src =
    toolOk
        ( object
            [ "cells"
                .= [ object
                        [ "id" .= (0 :: Int)
                        , "defines" .= (["best", "sse"] :: [Text])
                        , "source" .= src
                        ]
                   ]
            ]
        )

bindingsEcho :: Text -> Text
bindingsEcho src = toolOk (object ["result" .= src])

healEcho :: Text -> Text -> Text
healEcho before after =
    toolOk
        ( attachSelfHeal
            (selfHealNote before after)
            (object ["cellId" .= (0 :: Int), "execution" .= object ["ok" .= True]])
        )

discoveryCard :: Text -> Text
discoveryCard name =
    toolOk
        ( object
            [ "query" .= name
            , "state" .= ("found" :: Text)
            , "hits"
                .= [ object
                        [ "name" .= name
                        , "type" .= ("[(Text, Double)] -> Plot -> Text" :: Text)
                        , "module" .= ("Granite.Svg" :: Text)
                        , "package" .= ("granite" :: Text)
                        , "install" .= ("installed" :: Text)
                        , "note" .= T.replicate 4 ("call " <> name <> " with a plot; ")
                        ]
                   ]
            ]
        )

classGrid :: [(String, [Text])]
classGrid =
    [
        ( "read_cell source echo"
        , [readCellEcho srcA, readCellEcho srcA, readCellEcho srcB]
        )
    ,
        ( "list_cells source echo"
        , [listCellsEcho srcA, listCellsEcho srcA, listCellsEcho srcB]
        )
    ,
        ( "list_bindings echo"
        , [bindingsEcho srcA, bindingsEcho srcA, bindingsEcho srcB]
        )
    ,
        ( "self_heal note echo"
        , [healEcho srcA srcB, healEcho srcA srcB, healEcho srcB srcA]
        )
    ,
        ( "discovery card"
        , [discoveryCard "bars", discoveryCard "bars", discoveryCard "columns"]
        )
    ]

esc :: Text -> Text
esc t = T.dropEnd 1 (T.drop 1 (TE.decodeUtf8 (LBS.toStrict (encode t))))

occursIn :: Text -> Text -> Bool
occursIn block emission =
    block `T.isInfixOf` emission || esc block `T.isInfixOf` emission

runSeq :: [Text] -> [Text]
runSeq = go 1 emptyEmitLedger
  where
    go _ _ [] = []
    go turn led (c : cs) =
        let (c', led') = dedupText turn c led
         in c' : go (turn + 1) led' cs

assertInvariant :: [Text] -> Expectation
assertInvariant cs = do
    let outs = runSeq cs
    forM_ (zip3 [1 :: Int ..] cs outs) $ \(i, orig, out) -> do
        let before = concatMap eligibleBlocks (take (i - 1) cs)
            blocks = eligibleBlocks orig
            isLeaf b =
                not
                    ( any
                        (\b' -> b' /= b && (b' `T.isInfixOf` b || esc b' `T.isInfixOf` b))
                        blocks
                    )
        forM_ blocks $ \b ->
            if b `elem` before
                then do
                    when (occursIn b out) $
                        expectationFailure
                            ("turn " <> show i <> ": repeated block re-transmitted verbatim")
                    unless
                        ( "as established turn " `T.isInfixOf` out
                            || "changed since turn " `T.isInfixOf` out
                        )
                        $ expectationFailure
                            ("turn " <> show i <> ": repeat carries no back-reference or diff")
                else when (isLeaf b) $ do
                    let priorLines = concatMap T.lines before
                        newLines =
                            [ l
                            | l <- T.lines b
                            , l `notElem` priorLines
                            , not (T.null (T.strip l))
                            ]
                    unless (occursIn b out) $ do
                        unless ("changed since turn " `T.isInfixOf` out) $
                            expectationFailure
                                ("turn " <> show i <> ": first occurrence suppressed")
                        forM_ newLines $ \l ->
                            unless (occursIn l out) $
                                expectationFailure
                                    ( "turn "
                                        <> show i
                                        <> ": diff lost a changed line: "
                                        <> T.unpack l
                                    )

emitLedgerSpec :: Spec
emitLedgerSpec = describe "cross-turn content ledger (R3.8/R3.9/R5.5)" $ do
    describe "general dedup invariant over the whole artifact-class grid" $
        forM_ classGrid $ \(label, seqs) ->
            it (label <> ": no eligible block transmitted twice") $
                assertInvariant seqs

    it "the invariant holds across interleaved classes in one episode" $
        assertInvariant (concatMap snd classGrid)

    it "a cross-surface echo (read_cell then list_cells) back-references" $ do
        let outs = runSeq [readCellEcho srcA, listCellsEcho srcA]
        case outs of
            [_, second] -> do
                occursIn srcA second `shouldBe` False
                second `shouldSatisfy` T.isInfixOf "as established turn 1"
            _ -> expectationFailure "expected two emissions"

    it "changed content re-emits as a line diff carrying the changed lines" $ do
        let outs = runSeq [readCellEcho srcA, listCellsEcho srcB]
        case outs of
            [_, second] -> do
                occursIn srcB second `shouldBe` False
                second `shouldSatisfy` T.isInfixOf "changed since turn 1"
                second
                    `shouldSatisfy` occursIn "best = maximumBy (comparing sse) candidates"
            _ -> expectationFailure "expected two emissions"

    it "a back-reference stays within backRefLimit chars" $ do
        let block = T.replicate 40 "abcdefgh "
        case runSeq [block, block] of
            [_, second] -> do
                T.length second `shouldSatisfy` (<= backRefLimit)
                second `shouldSatisfy` T.isInfixOf "as established turn 1"
            _ -> expectationFailure "expected two emissions"

    it "sub-floor content always passes through byte-identical" $ do
        let short = T.take (blockFloor - 1) (T.replicate 40 "tiny block ")
        runSeq [short, short] `shouldBe` [short, short]

    it "a within-message repeat dedups after its first occurrence" $ do
        let block = T.replicate 30 "block bytes " <> "end"
            both = block <> "\n\n" <> block
        case runSeq [both] of
            [out] -> do
                length (T.breakOnAll block out) `shouldBe` 1
                out `shouldSatisfy` T.isInfixOf "as established turn 1"
            _ -> expectationFailure "expected one emission"
