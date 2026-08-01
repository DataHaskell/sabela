{-# LANGUAGE OverloadedStrings #-}

{- | The replay fixture the discovery benchmark is owed: every Hoogle
sub-query the search plan raises over the corpus, with the rows the real
database returned, keyed by sub-query as the benchmark decodes it.
-}
module Probe.Record (recordFixture) where

import Data.Aeson (
    Value (..),
    eitherDecodeFileStrict,
    encodeFile,
    object,
    (.=),
 )
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sabela.AI.HoogleClient (HoogleHit (..), queryAllDbs)
import Sabela.AI.Search.Gather (searchNeed)
import Sabela.AI.Search.Need (parseNeed)

{- | The depth the benchmark replays at. Recording at a different one records
sub-queries the replay never raises, and misses ones it does.
-}
replayDepth :: Int
replayDepth = 5

recordFixture :: FilePath -> FilePath -> IO ()
recordFixture corpusPath outPath = do
    queries <- loadCorpus corpusPath
    seen <- newIORef M.empty
    mapM_
        (\q -> searchNeed (capture seen) replayDepth (parseNeed S.empty q))
        queries
    rows <- readIORef seen
    encodeFile
        outPath
        (object [K.fromText q .= map hitJson hs | (q, hs) <- M.toList rows])
    TIO.putStrLn
        ( "[probe] recorded "
            <> tShow (M.size rows)
            <> " sub-queries from "
            <> tShow (length queries)
            <> " corpus queries into "
            <> T.pack outPath
        )

{- | The retriever the recording runs on: the live database, with every
sub-query and its rows kept as they arrive.
-}
capture :: IORef (M.Map Text [HoogleHit]) -> Int -> Text -> IO [HoogleHit]
capture seen k q = do
    hs <- liveQuery k q
    modifyIORef' seen (M.insert q hs)
    pure hs

{- | The one call the search plan makes of Hoogle, restated here rather than
imported, so the recording is not shaped by the code it records.
-}
liveQuery :: Int -> Text -> IO [HoogleHit]
liveQuery k q
    | T.null (T.strip q) = pure []
    | otherwise =
        queryAllDbs
            ["search", "--count=" ++ show (max 1 k * 3), "--json", T.unpack q]

hitJson :: HoogleHit -> Value
hitJson h =
    object
        [ "n" .= hhName h
        , "p" .= hhPackage h
        , "m" .= hhModule h
        , "t" .= hhType h
        , "d" .= hhDocs h
        ]

loadCorpus :: FilePath -> IO [Text]
loadCorpus fp = do
    parsed <- eitherDecodeFileStrict fp
    case parsed of
        Left e -> fail ("probe corpus: " <> e)
        Right (Array ts) ->
            pure [q | Object t <- toList ts, Just (String q) <- [KM.lookup "query" t]]
        Right _ -> fail "probe corpus: expected an array of tasks"

tShow :: Int -> Text
tShow = T.pack . show
