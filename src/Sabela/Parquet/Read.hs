{-# LANGUAGE OverloadedStrings #-}

{- | Reading a parquet schema off disk. Two seeks to the tail, never the whole
file: the footer says how long it is, so a preview of a gigabyte costs the
same as a preview of a kilobyte.
-}
module Sabela.Parquet.Read (
    readParquetSchema,
    isParquetPath,
    footerReadCap,
) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeExtension)
import System.IO (IOMode (ReadMode), SeekMode (..), hFileSize, hSeek, withFile)

import Sabela.Parquet (
    ParquetSchema,
    footerLengthOfTail,
    parquetFooterOfTail,
 )

-- | A footer larger than this is refused rather than pulled into memory.
footerReadCap :: Int
footerReadCap = 16 * 1024 * 1024

{- | Parquet by name. The extension only decides whether to try; a file that
lies about it fails on the missing PAR1 marker, not on its name.
-}
isParquetPath :: FilePath -> Bool
isParquetPath p = map toLower (takeExtension p) == ".parquet"

readParquetSchema :: FilePath -> IO (Either Text ParquetSchema)
readParquetSchema path = do
    got <- try (withFile path ReadMode grab)
    pure $ case got of
        Left e -> Left (T.pack (show (e :: IOException)))
        Right r -> r
  where
    grab h = do
        size <- hFileSize h
        if size < 12
            then pure (Left "too small to be a parquet file")
            else do
                hSeek h SeekFromEnd (-8)
                trailer <- BS.hGet h 8
                case footerLengthOfTail (BS.replicate 4 0 <> trailer) of
                    Left e -> pure (Left e)
                    Right len
                        | len > footerReadCap -> pure (Left oversize)
                        | fromIntegral len + 8 > size -> pure (Left doesNotFit)
                        | otherwise -> do
                            hSeek h SeekFromEnd (negate (fromIntegral len + 8))
                            parquetFooterOfTail <$> BS.hGet h (len + 8)

oversize, doesNotFit :: Text
oversize = "the parquet footer is too large to read for a preview"
doesNotFit = "the parquet footer length does not fit the file"
