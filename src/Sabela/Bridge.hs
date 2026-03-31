{-# LANGUAGE OverloadedStrings #-}

module Sabela.Bridge
    ( bridgePreamble
    , widgetPreamble
    , pythonBridgePreamble
    , isTemplateHaskellOutput
    ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

bridgePreamble :: M.Map Text Text -> Text
bridgePreamble store
    | M.null store = ""
    | otherwise =
        T.unlines
            [ "let _bridge_" <> name <> " = " <> T.pack (show (T.unpack val))
            | (name, val) <- M.toList store
            ]

widgetPreamble :: Int -> M.Map Text Text -> Text
widgetPreamble cid vals =
    let pairs = show [(T.unpack k, T.unpack v) | (k, v) <- M.toList vals]
     in T.unlines
            [ "writeIORef _sabelaWidgetRef " <> T.pack pairs
            , "writeIORef _sabelaCellIdRef " <> T.pack (show (show cid))
            ]

pythonBridgePreamble :: M.Map Text Text -> Text
pythonBridgePreamble store
    | M.null store = ""
    | otherwise =
        T.unlines [pythonBinding name val | (name, val) <- M.toList store]

pythonBinding :: Text -> Text -> Text
pythonBinding name val
    | T.any (== '\n') val =
        "import base64; _bridge_"
            <> name
            <> " = base64.b64decode("
            <> T.pack (show (encodeBase64 val))
            <> ").decode('utf-8')"
    | otherwise = "_bridge_" <> name <> " = " <> T.pack (show (T.unpack val))

encodeBase64 :: Text -> String
encodeBase64 t =
    let bytes = map (fromIntegral . fromEnum) (T.unpack t) :: [Word8]
     in map (toEnum . fromIntegral) (b64Encode bytes)

b64Encode :: [Word8] -> [Word8]
b64Encode [] = []
b64Encode bs =
    let (chunk, rest) = splitAt 3 bs
     in encodeChunk chunk ++ b64Encode rest

encodeChunk :: [Word8] -> [Word8]
encodeChunk chunk =
    let padded = chunk ++ replicate (3 - length chunk) 0
        [a, b, c] = padded
        i0 = fromIntegral a `shiftR` 2
        i1 = ((fromIntegral a .&. 3) `shiftL` 4) .|. (fromIntegral b `shiftR` 4)
        i2 = ((fromIntegral b .&. 15) `shiftL` 2) .|. (fromIntegral c `shiftR` 6)
        i3 = fromIntegral c .&. 63
        enc = map (b64Table !!) $ case length chunk of
            1 -> [i0, i1]
            2 -> [i0, i1, i2]
            _ -> [i0, i1, i2, i3]
        pad = replicate (case length chunk of 1 -> 2; 2 -> 1; _ -> 0) (fromIntegral (fromEnum '='))
     in enc ++ pad

b64Table :: [Word8]
b64Table = map (fromIntegral . fromEnum) (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['+', '/'])

isTemplateHaskellOutput :: Text -> Bool
isTemplateHaskellOutput err =
    not (T.null (T.strip err))
        && all isTHLine (filter (not . T.null . T.strip) (T.lines err))
  where
    isTHLine l = " :: " `T.isInfixOf` l
