{-# LANGUAGE OverloadedStrings #-}

{- | The payload-truth probe: every discover answer over a fixed grid, checked
against witnesses that share no code with it. Every check reports what it
examined, and one that reached nothing fails: verifying nothing is not clean.
-}
module Main (main) where

import Control.Monad (forM, unless)
import Data.Aeson (Value (..), eitherDecodeFileStrict, encodeFile, object)
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)

import Probe.Check (cardClaims, checkEnvelope)
import Probe.Claim (
    Claim (..),
    Outcome (..),
    Tally (..),
    checkNames,
    idleWitnesses,
    tallyOf,
 )
import Probe.Oracle (Oracle (..), browseExports, defaultPackageDbs, loadOracle)
import Probe.Record (recordFixture)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Envelope (envelopeChars)
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Siza.Retro.Distribution (Spread (..), spreadOf)
import Siza.Transport (callTool, newConn)

data Opts = Opts
    { opUrl :: Text
    , opQueries :: FilePath
    , opRecord :: Maybe FilePath
    , opCorpus :: FilePath
    , opDump :: Maybe FilePath
    , opMedianCap :: Int
    }

defaultOpts :: Opts
defaultOpts =
    Opts
        { opUrl = "http://localhost:3000"
        , opQueries = "data/eval/probe-queries.json"
        , opRecord = Nothing
        , opCorpus = "data/eval/discovery-corpus.json"
        , opDump = Nothing
        , opMedianCap = 1600
        }

parseOpts :: [String] -> Opts -> Opts
parseOpts as o = case as of
    ("--url" : v : rest) -> parseOpts rest o{opUrl = T.pack v}
    ("--queries" : v : rest) -> parseOpts rest o{opQueries = v}
    ("--record" : v : rest) -> parseOpts rest o{opRecord = Just v}
    ("--corpus" : v : rest) -> parseOpts rest o{opCorpus = v}
    ("--dump" : v : rest) -> parseOpts rest o{opDump = Just v}
    ("--median-cap" : v : rest) -> parseOpts rest o{opMedianCap = read v}
    (_ : rest) -> parseOpts rest o
    [] -> o

main :: IO ()
main = do
    opts <- parseOpts <$> getArgs <*> pure defaultOpts
    case opRecord opts of
        Just out -> recordFixture (opCorpus opts) out
        Nothing -> gate opts

gate :: Opts -> IO ()
gate opts = do
    grid <- loadGrid (opQueries opts)
    conn <- newConn
    answers <- forM grid $ \(q, args) -> do
        out <- runDiscoverCall True (callTool conn (opUrl opts)) q args
        pure (q, valueOf out)
    mapM_ (`encodeFile` object [("answers", toArray answers)]) (opDump opts)
    dbs <- defaultPackageDbs
    oracle <- loadOracle dbs
    browsed <- browseExports oracle (nub (mapMaybe (cardClaims . snd) answers))
    let claims = concat [checkEnvelope oracle browsed q v | (q, v) <- answers]
        tally = tallyOf claims
        idle = idleWitnesses tally
        spread = spreadOf (map (envelopeChars . snd) answers)
    report (length answers) oracle spread claims tally idle (opMedianCap opts)
    verdict (truthful tally idle && seen oracle) (economical spread opts)
  where
    seen o = orTrouble o == Nothing
    truthful t idle =
        null idle && all (\y -> tyBroken y == 0 && tyBlind y == 0) (M.elems t)
    economical s o = maybe False ((<= opMedianCap o) . spMedian) s

toArray :: [(Text, Value)] -> Value
toArray answers = Array (foldr cons mempty answers)
  where
    cons (q, v) acc = pure (object [("query", String q), ("answer", v)]) <> acc

{- | Truth and economy are ranked differently, so they exit differently: 1
means a claim no witness backs, or a witness that saw nothing; 2 means every
claim held and the payload set costs more than the cap allows.
-}
verdict :: Bool -> Bool -> IO ()
verdict True True = do
    TIO.putStrLn "[probe] truth PASS, economy PASS"
    exitSuccess
verdict True False = do
    TIO.putStrLn "[probe] truth PASS, economy FAIL (median over cap)"
    exitWith (ExitFailure 2)
verdict False _ = do
    TIO.putStrLn "[probe] truth FAIL"
    exitWith (ExitFailure 1)

{- | Distribution first: the budget is a cap, and a cap says nothing about
where the mass sits. Then the ledger, for every check whether or not it found
anything, so a reader can see what the run was in a position to know.
-}
report ::
    Int ->
    Oracle ->
    Maybe Spread ->
    [Claim] ->
    M.Map Text Tally ->
    [Text] ->
    Int ->
    IO ()
report n oracle spread claims tally idle cap = do
    TIO.putStrLn ("[probe] answers: " <> tShow n)
    TIO.putStrLn (widths spread cap)
    TIO.putStrLn (witnesses oracle)
    mapM_ (TIO.putStrLn . ledger tally) checkNames
    mapM_ (\c -> TIO.putStrLn ("[probe] " <> c <> " examined nothing")) idle
    unless (null noted) $ do
        TIO.putStrLn "[probe] first of each kind per check:"
        mapM_ detail (M.toList (firstOf noted))
  where
    noted = [c | c <- claims, clOutcome c /= Backed]
    detail ((c, _), f) =
        TIO.putStrLn
            ("[probe]   " <> c <> " '" <> clQuery f <> "': " <> why (clOutcome f))
    why (Broken t) = "untrue — " <> t
    why (Blind t) = "unwitnessed — " <> t
    why Backed = ""

widths :: Maybe Spread -> Int -> Text
widths Nothing _ = "[probe] payload chars — no answers to measure"
widths (Just s) cap =
    "[probe] payload chars — median "
        <> tShow (spMedian s)
        <> " (cap "
        <> tShow cap
        <> "), p90 "
        <> tShow (spP90 s)
        <> ", max "
        <> tShow (spMax s)

witnesses :: Oracle -> Text
witnesses o = case orTrouble o of
    Just t -> "[probe] oracle UNUSABLE: " <> t
    Nothing ->
        "[probe] oracle: "
            <> tShow (length (orDbs o))
            <> " package db(s) beside the compiler's own, "
            <> tShow (M.size (orExposed o))
            <> " packages"

ledger :: M.Map Text Tally -> Text -> Text
ledger t c =
    "[probe] "
        <> c
        <> ": "
        <> tShow (tyBacked y)
        <> " backed, "
        <> tShow (tyBlind y)
        <> " unwitnessed, "
        <> tShow (tyBroken y)
        <> " untrue"
  where
    y = M.findWithDefault (Tally 0 0 0) c t

{- | One example per check per kind, so an untrue claim is never hidden behind
an unwitnessed one that sorted first.
-}
firstOf :: [Claim] -> M.Map (Text, Text) Claim
firstOf cs = M.fromListWith (\_ old -> old) [((clCheck c, kind c), c) | c <- cs]
  where
    kind c = case clOutcome c of
        Broken _ -> "untrue"
        _ -> "unwitnessed"

loadGrid :: FilePath -> IO [(Text, Value)]
loadGrid fp = do
    parsed <- eitherDecodeFileStrict fp
    case parsed of
        Left e -> fail ("probe grid: " <> e)
        Right (Object o) -> pure (rows (KM.lookup "queries" o))
        Right _ -> fail "probe grid: expected an object"
  where
    rows (Just (Array qs)) =
        [ (q, argsOf r)
        | Object r <- foldr (:) [] qs
        , Just (String q) <- [KM.lookup "query" r]
        ]
    rows _ = []
    argsOf r = case KM.lookup "args" r of
        Just a@(Object _) -> a
        _ -> object []

valueOf :: ToolOutcome -> Value
valueOf (ToolOk v) = v
valueOf (ToolErr v) = v

tShow :: Int -> Text
tShow = T.pack . show
