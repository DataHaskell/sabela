{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Just enough of Thrift's compact protocol to read a Parquet footer. The
decoder is structural: it returns field ids and values without knowing any
Parquet schema, so the format's meaning lives in one place, above this.
-}
module Sabela.Parquet.Thrift (
    TValue (..),
    TStructFields,
    decodeStruct,
    field,
    fieldInt,
    fieldBinary,
    fieldList,
    fieldStruct,
) where

import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Float (castWord64ToDouble)

{- | A decoded value. Thrift's byte, i16, i32 and i64 all arrive as 'TInt':
Parquet never needs to tell them apart, and one case cannot be mismatched.
-}
data TValue
    = TBool !Bool
    | TInt !Int64
    | TDouble !Double
    | TBinary !BS.ByteString
    | TList [TValue]
    | TStruct TStructFields
    deriving (Eq, Show)

type TStructFields = [(Int, TValue)]

-- | Position-passing parser. Errors name the offset so a bad file is locatable.
newtype P a = P {runP :: BS.ByteString -> Int -> Either Text (a, Int)}

instance Functor P where
    fmap f (P g) = P $ \b i -> fmap (first f) (g b i)

instance Applicative P where
    pure a = P $ \_ i -> Right (a, i)
    P f <*> P g = P $ \b i -> do
        (h, j) <- f b i
        (a, k) <- g b j
        pure (h a, k)

instance Monad P where
    P g >>= f = P $ \b i -> do
        (a, j) <- g b i
        runP (f a) b j

failAt :: Text -> P a
failAt msg = P $ \_ i -> Left (msg <> " at byte " <> T.pack (show i))

byte :: P Word8
byte = P $ \b i ->
    if i < BS.length b
        then Right (BS.index b i, i + 1)
        else Left "ran past the end of the footer"

bytes :: Int -> P BS.ByteString
bytes n = P $ \b i ->
    if n >= 0 && i + n <= BS.length b
        then Right (BS.take n (BS.drop i b), i + n)
        else Left "ran past the end of the footer"

uvarint :: P Int64
uvarint = go 0 0
  where
    go !acc !sh
        | sh > 63 = failAt "varint too long"
        | otherwise = do
            c <- byte
            let acc' = acc + (fromIntegral (c .&. 0x7f) `shiftL` sh)
            if testBit c 7 then go acc' (sh + 7) else pure acc'

zigzag :: P Int64
zigzag = fmap (\n -> (n `shiftR` 1) `xor` negate (n .&. 1)) uvarint

-- | Decode one compact-protocol struct from the front of a byte string.
decodeStruct :: BS.ByteString -> Either Text TStructFields
decodeStruct b = fst <$> runP (structBody 0) b 0

structBody :: Int -> P TStructFields
structBody lastId = do
    h <- byte
    if h == 0
        then pure []
        else do
            let tid = fromIntegral (h .&. 0x0f)
                delta = fromIntegral (h `shiftR` 4)
            fid <-
                if delta == (0 :: Int)
                    then fromIntegral <$> zigzag
                    else pure (lastId + delta)
            v <- value tid
            ((fid, v) :) <$> structBody fid

value :: Int -> P TValue
value 1 = pure (TBool True)
value 2 = pure (TBool False)
value 3 = TInt . fromIntegral <$> byte
value 4 = TInt <$> zigzag
value 5 = TInt <$> zigzag
value 6 = TInt <$> zigzag
value 7 = TDouble <$> double
value 8 = TBinary <$> (uvarint >>= bytes . fromIntegral)
value 9 = list
value 10 = list
value 12 = TStruct <$> structBody 0
value t = failAt ("unsupported thrift type " <> T.pack (show t))

{- | Thrift writes a compact double as eight little-endian bytes. Parquet's
footer only uses it for statistics, which this decoder passes through.
-}
double :: P Double
double = do
    raw <- bytes 8
    pure
        ( castWord64ToDouble
            (BS.foldr (\w acc -> (acc `shiftL` 8) + fromIntegral w) 0 raw)
        )

list :: P TValue
list = do
    h <- byte
    let et = fromIntegral (h .&. 0x0f)
        short = fromIntegral (h `shiftR` 4)
    n <- if short == (15 :: Int) then fromIntegral <$> uvarint else pure short
    TList <$> mapM (const (value et)) [1 .. n]

field :: Int -> TStructFields -> Maybe TValue
field k fs = snd <$> find ((== k) . fst) fs

fieldInt :: Int -> TStructFields -> Maybe Int
fieldInt k fs = case field k fs of
    Just (TInt n) -> Just (fromIntegral n)
    _ -> Nothing

fieldBinary :: Int -> TStructFields -> Maybe BS.ByteString
fieldBinary k fs = case field k fs of
    Just (TBinary s) -> Just s
    _ -> Nothing

fieldList :: Int -> TStructFields -> [TValue]
fieldList k fs = case field k fs of
    Just (TList vs) -> vs
    _ -> []

fieldStruct :: Int -> TStructFields -> Maybe TStructFields
fieldStruct k fs = case field k fs of
    Just (TStruct s) -> Just s
    _ -> Nothing
