{-# LANGUAGE OverloadedStrings #-}

{- | Reads a Parquet file's own footer: the column names, their types and the
row count, from the bytes alone. No kernel, no dataframe, no decompressor —
the footer is uncompressed Thrift, so this answers before a session exists.
-}
module Sabela.Parquet (
    ParquetSchema (..),
    ParquetColumn (..),
    parquetFooter,
    parquetFooterOfTail,
    parquetMagic,
    footerLengthOfTail,
    footerSlice,
) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)

import Sabela.Parquet.Thrift (
    TStructFields,
    TValue (..),
    decodeStruct,
    fieldBinary,
    fieldInt,
    fieldList,
 )

data ParquetColumn = ParquetColumn
    { pqName :: Text
    , pqType :: Text
    }
    deriving (Eq, Show)

data ParquetSchema = ParquetSchema
    { pqColumns :: [ParquetColumn]
    , pqRowCount :: Int
    }
    deriving (Eq, Show)

parquetMagic :: BS.ByteString
parquetMagic = "PAR1"

{- | The footer's bytes: a file is @PAR1 … footer len(4) PAR1@, so the length
sits in the last eight bytes and the footer runs back from there.
-}
footerSlice :: BS.ByteString -> Either Text BS.ByteString
footerSlice b
    | BS.length b < 12 = Left tooSmall
    | BS.take 4 b /= parquetMagic = Left notParquet
    | otherwise = tailSlice b

{- | The footer out of a file's trailing bytes. Separate from 'footerSlice'
because the footer is at the end: a reader that seeks to the tail never sees
the leading marker, and should not have to fake one.
-}
tailSlice :: BS.ByteString -> Either Text BS.ByteString
tailSlice b = do
    len <- footerLengthOfTail b
    if len + 8 > BS.length b
        then Left shortTail
        else Right (BS.take len (BS.drop (BS.length b - 8 - len) b))

{- | How many bytes of footer the trailing marker claims, so a reader can ask
for exactly that many rather than guessing or reading the whole file.
-}
footerLengthOfTail :: BS.ByteString -> Either Text Int
footerLengthOfTail b
    | n < 12 = Left tooSmall
    | BS.drop (n - 4) b /= parquetMagic = Left notParquet
    | len <= 0 = Left badLength
    | otherwise = Right len
  where
    n = BS.length b
    len = leWord (BS.take 4 (BS.drop (n - 8) b))

leWord :: BS.ByteString -> Int
leWord = BS.foldr (\w acc -> acc * 256 + fromIntegral w) 0

tooSmall, notParquet, badLength, shortTail :: Text
tooSmall = "too small to be a parquet file"
notParquet = "not a parquet file: the PAR1 markers are missing"
badLength = "the parquet footer length does not fit the file"
shortTail = "the parquet footer is longer than the bytes read"

{- | The schema and row count a whole parquet file declares. Needs the whole
file only because the footer is at the end; nothing else is read.
-}
parquetFooter :: BS.ByteString -> Either Text ParquetSchema
parquetFooter b = footerSlice b >>= schemaOf

-- | The same, from a file's trailing bytes alone.
parquetFooterOfTail :: BS.ByteString -> Either Text ParquetSchema
parquetFooterOfTail b = tailSlice b >>= schemaOf

schemaOf :: BS.ByteString -> Either Text ParquetSchema
schemaOf raw = do
    fields <- decodeStruct raw
    pure
        ParquetSchema
            { pqColumns = leafColumns fields
            , pqRowCount = fromMaybe 0 (fieldInt 3 fields)
            }

leafColumns :: TStructFields -> [ParquetColumn]
leafColumns fields = mapMaybe leaf (fieldList 2 fields)
  where
    leaf (TStruct el) = do
        physical <- fieldInt 1 el
        name <- fieldBinary 4 el
        pure (ParquetColumn (decode name) (logicalName el physical))
    leaf _ = Nothing
    decode = TE.decodeUtf8With lenientDecode

{- | The name to show for a column. A converted type says what the bytes
mean, which is what a reader wants; the physical type is the fallback when
it says nothing.
-}
logicalName :: TStructFields -> Int -> Text
logicalName el physical = case fieldInt 6 el of
    Just c | Just n <- lookup c convertedTypes -> n
    _ -> physicalName physical

physicalName :: Int -> Text
physicalName 0 = "Bool"
physicalName 1 = "Int"
physicalName 2 = "Int"
physicalName 3 = "Int96"
physicalName 4 = "Double"
physicalName 5 = "Double"
physicalName 6 = "Text"
physicalName 7 = "Bytes"
physicalName n = "type" <> T.pack (show n)

-- | ConvertedType, the widely written half of parquet's logical types.
convertedTypes :: [(Int, Text)]
convertedTypes =
    [ (0, "Text")
    , (1, "Map")
    , (3, "List")
    , (4, "Enum")
    , (5, "Decimal")
    , (6, "Date")
    , (7, "Time")
    , (8, "Time")
    , (9, "Timestamp")
    , (10, "Timestamp")
    , (11, "Int")
    , (12, "Int")
    , (13, "Int")
    , (14, "Int")
    , (15, "Int")
    , (16, "Int")
    , (17, "Int")
    , (18, "Int")
    , (21, "Json")
    , (22, "Bson")
    ]
