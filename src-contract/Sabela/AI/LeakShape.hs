{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.LeakShape (
    controlCharred,
    doubleEncodedJson,
    embeddedSerialisation,
    hashQualifiedToken,
    infoDumpLine,
    leakyLine,
    leakyToken,
    longHashToken,
) where

import Data.Char (isAlphaNum, isDigit, isHexDigit, isLetter)
import Data.Text (Text)
import qualified Data.Text as T

embeddedSerialisation :: Text -> Bool
embeddedSerialisation = T.isInfixOf "{\""

doubleEncodedJson :: Text -> Bool
doubleEncodedJson = T.isInfixOf "{\\\""

longHashToken :: Text -> Bool
longHashToken = any hashSeg . T.split (`elem` ("-:" :: String))
  where
    hashSeg seg = T.length seg >= 20 && T.all isHexDigit seg

hashQualifiedToken :: Text -> Bool
hashQualifiedToken w = case T.breakOn ":" w of
    (pre, post) ->
        not (T.null (T.drop 1 post)) && versionHashed (T.splitOn "-" pre)
  where
    versionHashed parts = case snd (break isVersion parts) of
        (v : rest) -> isVersion v && all isHashy rest
        [] -> False
    isVersion s =
        not (T.null s)
            && T.count "." s >= 1
            && T.all (\c -> isDigit c || c == '.') s
    isHashy s =
        not (T.null s)
            && T.all isAlphaNum s
            && T.any isDigit s
            && T.any isLetter s

controlCharred :: Text -> Bool
controlCharred = T.any (\c -> c < ' ' && c `notElem` ("\n\t\r" :: String))

infoDumpLine :: Text -> Bool
infoDumpLine l =
    "-- Defined in" `T.isInfixOf` l
        || "instance " `T.isPrefixOf` T.stripStart l

leakyToken :: Text -> Bool
leakyToken w = longHashToken w || hashQualifiedToken w

leakyLine :: Text -> Bool
leakyLine l =
    embeddedSerialisation l
        || controlCharred l
        || any leakyToken (T.words l)
