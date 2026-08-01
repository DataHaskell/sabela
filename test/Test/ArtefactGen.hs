{-# LANGUAGE OverloadedStrings #-}

{- | Generators for the artefact-shape properties: arbitrary identifiers,
module names and packages, delimited grids, prose, JSON and byte blobs.
Nothing here names the library or the file format the fixes must work for.
-}
module Test.ArtefactGen (
    genIdent,
    genModuleName,
    genPackage,
    genCell,
    GridSpec (..),
    genGridSpec,
    genNumericGrid,
    genNamedGrid,
    renderGrid,
    gridColumnNames,
    genProse,
    genJsonText,
    genBlobBytes,
    genBlobText,
    genArtefactText,
    genOversizeGrid,
    genUndecodableBytes,
    genSizedBytes,
    genTreePaths,
    SeamBlob (..),
    genSeamBlob,
    genRun,
    delimiters,
) where

import Data.Aeson (Value (..), encode, object, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.QuickCheck

{- | Lowercase Haskell identifiers, the vocabulary a real header or a real
binding name is drawn from.
-}
genIdent :: Gen Text
genIdent = do
    c <- elements ['a' .. 'z']
    rest <- listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_'"))
    pure (T.pack (c : take 12 rest))

genModuleName :: Gen Text
genModuleName =
    elements
        [ "Data.Map"
        , "Data.Text"
        , "Data.Aeson"
        , "Control.Monad.State"
        , "Data.ByteString"
        , "Quux.Internal.Frobnicate"
        ]

genPackage :: Gen Text
genPackage =
    elements
        ["containers", "text", "aeson", "mtl", "bytestring", "zzfrob", "wibble-lib"]

-- | A cell of a delimited grid: never empty, never a delimiter, never a newline.
genCell :: Gen Text
genCell =
    oneof
        [ genIdent
        , genPackage
        , genModuleName
        , T.pack . show <$> (arbitrary :: Gen Int)
        , (\d -> T.pack (show (fromIntegral d / 100 :: Double)))
            <$> (arbitrary :: Gen Int)
        , elements ["true", "false", "True", "False"]
        ]

delimiters :: [Text]
delimiters = [",", "\t", ";", "|"]

data GridSpec = GridSpec
    { gsDelim :: Text
    , gsHeader :: Maybe [Text]
    , gsRows :: [[Text]]
    }
    deriving (Eq, Show)

genGridSpec :: Gen GridSpec
genGridSpec = do
    delim <- elements delimiters
    width <- choose (1, 6 :: Int)
    height <- choose (0, 12 :: Int)
    wantHeader <- arbitrary
    header <-
        if wantHeader
            then Just <$> vectorOf width (safeCell delim)
            else pure Nothing
    rows <- vectorOf height (vectorOf width (safeCell delim))
    pure (GridSpec delim header rows)

{- | Keeps a generated cell free of EVERY candidate delimiter and of
newlines, so only the chosen delimiter can split the rendering and the grid's
width is exactly the width the generator asked for.
-}
safeCell :: Text -> Gen Text
safeCell _ = do
    c <- genCell
    let cleaned = T.filter (`notElem` (",;|\t\n\r" :: String)) c
    pure (if T.null cleaned then "x" else cleaned)

renderGrid :: GridSpec -> Text
renderGrid gs =
    T.unlines
        [T.intercalate (gsDelim gs) r | r <- maybe [] pure (gsHeader gs) <> gsRows gs]

-- | The names a truthful reader may report for the grid: only a real header.
gridColumnNames :: GridSpec -> Maybe [Text]
gridColumnNames = gsHeader

genProse :: Gen Text
genProse = do
    n <- choose (1, 6 :: Int)
    ls <- vectorOf n line
    pure (T.unlines ls)
  where
    line = do
        ws <- listOf1 (oneof [genIdent, genModuleName, genPackage])
        punct <- elements ["", ".", "!", ",", " -- note", ": see"]
        pure (T.unwords ws <> punct)

genJsonText :: Gen Text
genJsonText = render <$> genValue 2
  where
    render = TE.decodeUtf8 . LBS.toStrict . encode
    genValue :: Int -> Gen Value
    genValue 0 = leaf
    genValue d =
        oneof
            [ leaf
            , toJSON <$> resize 4 (listOf (genValue (d - 1)))
            , object <$> resize 4 (listOf (pair (d - 1)))
            ]
    pair d = (\k v -> (K.fromText k, v)) <$> genIdent <*> genValue d
    leaf =
        oneof
            [ String <$> genIdent
            , Number . fromIntegral <$> (arbitrary :: Gen Int)
            , Bool <$> arbitrary
            , pure Null
            ]

genBlobBytes :: Gen [Word8]
genBlobBytes = listOf arbitrary

genBlobText :: Gen Text
genBlobText = TE.decodeLatin1 . BS.pack <$> genBlobBytes

-- | The three artefact families a reader must survive.
genArtefactText :: Gen Text
genArtefactText =
    oneof [renderGrid <$> genGridSpec, genProse, genJsonText, genBlobText]

{- | A grid of numbers only. No reader can find a header here, because no
cell of the first row differs in kind from the column under it.
-}
genNumericGrid :: Gen GridSpec
genNumericGrid = do
    delim <- elements delimiters
    width <- choose (3, 6 :: Int)
    height <- choose (14, 24 :: Int)
    rows <- vectorOf height (vectorOf width genNumberCell)
    pure (GridSpec delim Nothing rows)

{- | The same grid with a row of names on top: a header no reader can miss,
because every column below it is numeric and its own cell is not.
-}
genNamedGrid :: Gen GridSpec
genNamedGrid = do
    gs <- genNumericGrid
    header <- vectorOf (length (concat (take 1 (gsRows gs)))) genIdent
    pure gs{gsHeader = Just header}

genNumberCell :: Gen Text
genNumberCell = T.pack . show <$> (arbitrary :: Gen Int)

-- | A delimited text file longer than @sample@ bytes.
genOversizeGrid :: Int -> Gen Text
genOversizeGrid bound = do
    gs <- genGridSpec `suchThat` ((>= 2) . length . gsRows)
    let one = renderGrid gs
    pure (T.replicate (bound `div` max 1 (T.length one) + 2) one)

{- | Bytes no decoder can read as UTF-8, interleaved with printable ASCII so
the failure is inside the file rather than at one end. At least one byte
begins no valid sequence, so @decodeUtf8'@ is @Left@ by construction.
-}
genUndecodableBytes :: Gen BS.ByteString
genUndecodableBytes = do
    chunks <- listOf (ascii <$> genRun)
    bad <- listOf1 (elements [0xFF, 0xFE, 0xC0, 0xC1])
    stray <- listOf (choose (0x80, 0xBF))
    pure (BS.concat (interleave chunks (map BS.singleton (bad <> stray))))

interleave :: [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
interleave [] ys = ys
interleave xs [] = xs
interleave (x : xs) (y : ys) = x : y : interleave xs ys

{- | Bodies on both sides of the sample bound, decodable and not, so a
property about what a read discloses is answered by every size and every
kind of bytes a reader meets.
-}
genSizedBytes :: Int -> Gen BS.ByteString
genSizedBytes bound =
    oneof
        [ TE.encodeUtf8 <$> genArtefactText
        , TE.encodeUtf8 <$> genOversizeGrid bound
        , sbBytes <$> genSeamBlob bound
        , genUndecodableBytes
        ]

{- | A file longer than the read sample, with one printable run ending at the
head sample's last byte and another starting at the tail sample's first, NUL
bytes between. Runs taken from the joined samples splice into one false name.
-}
data SeamBlob = SeamBlob
    { sbBytes :: BS.ByteString
    , sbHeadRun :: Text
    , sbTailRun :: Text
    , sbUnread :: Int
    }
    deriving (Show)

genSeamBlob :: Int -> Gen SeamBlob
genSeamBlob bound = do
    headRun <- genRun
    tailRun <- genRun `suchThat` (/= headRun)
    gap <- choose (1, 4096)
    let tailLen = bound `div` 2
        headPad = bound - T.length headRun
        bs =
            BS.concat
                [ BS.replicate headPad 0
                , ascii headRun
                , BS.replicate gap 0
                , ascii tailRun
                , BS.replicate (tailLen - T.length tailRun) 0
                ]
    pure (SeamBlob bs headRun tailRun gap)

{- | Distinct relative file paths, at least one of them nested, so a property
about a tree meets directories as well as files. Every leaf carries an
extension and no directory segment does, so no name is both.
-}
genTreePaths :: Gen [FilePath]
genTreePaths = do
    n <- choose (0, 4)
    nested <- genTreePath 1
    rest <- vectorOf n (choose (0, 2) >>= genTreePath)
    pure (nub (nested : rest))

genTreePath :: Int -> Gen FilePath
genTreePath least = do
    depth <- max least <$> choose (0, 2 :: Int)
    segs <- vectorOf depth (T.unpack <$> genIdent)
    leaf <- T.unpack <$> genIdent
    ext <- elements [".csv", ".dat", ".md", ".bin", ".parquet"]
    pure (foldr (</>) (leaf <> ext) segs)

-- | Printable ASCII, long enough to be reported and short enough to be whole.
genRun :: Gen Text
genRun = do
    n <- choose (4, 20)
    T.pack
        <$> vectorOf n (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-."))

ascii :: Text -> BS.ByteString
ascii = BS.pack . map (fromIntegral . fromEnum) . T.unpack
