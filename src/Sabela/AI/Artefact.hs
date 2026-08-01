{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | One description of a file on disk, computed from its bytes. Every
surface with something to say about an artefact says it from here, so the
model is never told two different things about the same file.
-}
module Sabela.AI.Artefact (
    Artefact (..),
    ArtefactView (..),
    sampleFile,
    atRowLimit,
    artefactPairs,
    viewName,
    sampleBytes,
    decodePrefix,
) where

import Control.Exception (IOException, try)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import Data.Char (isPrint)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (IOMode (..), SeekMode (..), hFileSize, hSeek, withFile)

import Sabela.AI.PeekData (
    PeekResult (..),
    PeekVerdict (..),
    peekData,
    peekResultJSON,
 )
import Sabela.AI.Shape (binaryish, ellipsed, undecodableCount)

{- | Generous enough for source and small data files, small enough that one
read cannot swallow the model's context.
-}
sampleBytes :: Int
sampleBytes = 64 * 1024

peekRowLimit :: Int
peekRowLimit = 5

data ArtefactView = TextView | DelimitedView' | BinaryView
    deriving (Eq, Show)

viewName :: ArtefactView -> Text
viewName TextView = "text"
viewName DelimitedView' = "delimited"
viewName BinaryView = "binary"

data Artefact = Artefact
    { arBytes :: Integer
    , arText :: Text
    , arTruncated :: Bool
    , arView :: ArtefactView
    , arPeek :: PeekResult
    , arStrings :: [Text]
    , arStringsOmitted :: Int
    , arHeadBytes :: Int
    , arTailBytes :: Int
    }

{- | Reads a bounded sample rather than the file: the head always, and the
tail as well once the file is longer than the sample, because the part of a
container that names its fields is as often at the end as at the start.
-}
sampleFile :: FilePath -> IO (Maybe Artefact)
sampleFile path = do
    got <-
        try (withFile path ReadMode grab) ::
            IO (Either IOException (Integer, BS.ByteString, BS.ByteString))
    pure $ case got of
        Left _ -> Nothing
        Right (size, headBs, tailBs) -> Just (describe size headBs tailBs)
  where
    grab h = do
        size <- hFileSize h
        let headLen = fromIntegral (min size (fromIntegral sampleBytes))
        headBs <- BS.hGet h headLen
        tailBs <-
            if size > fromIntegral sampleBytes
                then do
                    hSeek
                        h
                        SeekFromEnd
                        (negate (fromIntegral (min size (fromIntegral (sampleBytes `div` 2)))))
                    BS.hGet h (sampleBytes `div` 2)
                else pure BS.empty
        pure (size, headBs, tailBs)

{- | The one description read again at the caller's row count. A caller who
wants more rows gets them here rather than as a second reading beside the
first, so the payload keeps one answer per question.
-}
atRowLimit :: Int -> Artefact -> Artefact
atRowLimit n a = a{arPeek = peekData n (arText a)}

describe :: Integer -> BS.ByteString -> BS.ByteString -> Artefact
describe size headBs tailBs =
    Artefact
        { arBytes = size
        , arText = body
        , arTruncated = truncated
        , arView = view
        , arPeek = peeked
        , arStrings = if view == BinaryView then shown else []
        , arStringsOmitted = if view == BinaryView then length runs - length shown else 0
        , arHeadBytes = BS.length headBs
        , arTailBytes = BS.length tailBs
        }
  where
    truncated = size > fromIntegral (BS.length headBs)
    body = decodePrefix truncated headBs
    peeked = peekData peekRowLimit body
    view
        | binaryish body = BinaryView
        | Delimited _ <- peekVerdict peeked = DelimitedView'
        | otherwise = TextView
    runs = sampleRuns headBs tailBs
    shown = take maxRuns runs

{- | Printable runs of each sample on its own. Extracting across the join
would splice the last run of the head onto the first run of the tail and
report a string that is nowhere in the file.
-}
sampleRuns :: BS.ByteString -> BS.ByteString -> [Text]
sampleRuns headBs tailBs =
    distinct
        (map (ellipsed runCharBound) (printableRuns headBs <> printableRuns tailBs))

{- | Distinct runs in the order they were found. A set rather than 'nub'
because the omitted count reads the whole list, and a sample can hold
thousands of runs.
-}
distinct :: [Text] -> [Text]
distinct = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert x seen) xs

{- | Decodes the sampled bytes so the result is always a prefix of the whole
file's decoding: a byte sequence cut mid-character is dropped rather than
answered with a replacement character the file does not contain.
-}
decodePrefix :: Bool -> BS.ByteString -> Text
decodePrefix cut bs = TE.decodeUtf8With lenientDecode (if cut then trimPartial bs else bs)

trimPartial :: BS.ByteString -> BS.ByteString
trimPartial bs = go (min 3 (BS.length bs)) bs
  where
    go 0 acc = acc
    go n acc = case BS.unsnoc acc of
        Just (rest, w)
            | continuation w -> go (n - 1) rest
            | leading w -> rest
        _ -> acc
    continuation w = w >= 0x80 && w < 0xC0
    leading w = w >= 0xC0

{- | Runs of printable ASCII within one sample, in order. For a container the
reader cannot decode these are the only names it has actually observed.
-}
printableRuns :: BS.ByteString -> [Text]
printableRuns bs = [r | r <- T.split (not . keep) decoded, T.length r >= minRun]
  where
    decoded = T.pack (map (toEnum . fromIntegral) (BS.unpack bs) :: String)
    keep :: Char -> Bool
    keep c = isPrint c && c < '\128'

minRun :: Int
minRun = 4

maxRuns :: Int
maxRuns = 40

runCharBound :: Int
runCharBound = 60

{- | What is known about the artefact, without its contents: the same facts
whichever surface is answering.
-}
artefactPairs :: Artefact -> [Pair]
artefactPairs a =
    [ "bytes" .= arBytes a
    , "view" .= viewName (arView a)
    , "truncated" .= arTruncated a
    , "shape" .= shapeJSON a
    ]
        <> ["sampled" .= sampledJSON a | arTruncated a]
        <> ["strings" .= toJSON (arStrings a) | not (null (arStrings a))]
        <> ["stringsOmitted" .= arStringsOmitted a | arStringsOmitted a > 0]

{- | Which bytes were read, so a count taken over a sample is never mistaken
for a count over the file.
-}
sampledJSON :: Artefact -> Value
sampledJSON a =
    object
        [ "headBytes" .= arHeadBytes a
        , "tailBytes" .= arTailBytes a
        , "unreadBytes" .= unreadBytes a
        ]

-- | Bytes covered by neither sample; zero when the two samples meet.
unreadBytes :: Artefact -> Integer
unreadBytes a =
    max 0 (arBytes a - fromIntegral (arHeadBytes a) - fromIntegral (arTailBytes a))

shapeJSON :: Artefact -> Value
shapeJSON a = case arView a of
    BinaryView ->
        object
            ( ["verdict" .= ("not-delimited" :: Text), "reason" .= binaryReason a]
                <> countPairs a
            )
    _ -> addPairs (countPairs a) (peekResultJSON (arPeek a))

{- | Every count taken over the sampled text sits here, beside the one field
that says which bytes were counted, so no count of the file is stated where
its coverage is not.
-}
countPairs :: Artefact -> [Pair]
countPairs a =
    ["countsCover" .= ("head-sample" :: Text) | arTruncated a]
        <> ["undecodable" .= n | let n = undecodableCount (arText a), n > 0]

addPairs :: [Pair] -> Value -> Value
addPairs ps (Object o) = Object (foldr (uncurry KM.insert) o ps)
addPairs _ v = v

binaryReason :: Artefact -> Text
binaryReason a =
    "the sample contains bytes that do not read as text, so no text view was \
    \returned; "
        <> runProvenance a

runProvenance :: Artefact -> Text
runProvenance a
    | null (arStrings a) =
        "no run of "
            <> tShow minRun
            <> " or more printable characters was found in "
            <> readRegion a
    | otherwise =
        "the `strings` field holds printable runs found in " <> readRegion a

-- | The bytes the runs were taken from, and the bytes nothing was taken from.
readRegion :: Artefact -> Text
readRegion a
    | unreadBytes a > 0 =
        "the first "
            <> tShow (arHeadBytes a)
            <> " and the last "
            <> tShow (arTailBytes a)
            <> " bytes, each read on its own; the "
            <> T.pack (show (unreadBytes a))
            <> " bytes between them were not read"
    | otherwise = "the file's bytes"

tShow :: Int -> Text
tShow = T.pack . show
