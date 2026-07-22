{-# LANGUAGE OverloadedStrings #-}

{- | Leak-SHAPE detectors shared by every response validator (5.2\/9.2,
R3.6\/R3.10): serialisation-in-string, unit-id\/package-hash atoms, control
chars, @:info@ dumps. Keyed on shape only, never on content or library.
-}
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

-- | A decoded string that itself carries a JSON serialisation (@{"@).
embeddedSerialisation :: Text -> Bool
embeddedSerialisation = T.isInfixOf "{\""

{- | A once-serialised payload carrying a second encoding level: the escaped
@{\\"@ open of a JSON object inside a JSON string.
-}
doubleEncodedJson :: Text -> Bool
doubleEncodedJson = T.isInfixOf "{\\\""

-- | A GHC unit-id style hash: 20+ consecutive hex chars in a token segment.
longHashToken :: Text -> Bool
longHashToken = any hashSeg . T.split (`elem` ("-:" :: String))
  where
    hashSeg seg = T.length seg >= 20 && T.all isHexDigit seg

{- | A package-qualified atom: @pkg-1.2.3:M.N@ or the abi-hashed
@pkg-1.2.3-c1e52ef7:M.N@ — GHC internals, never writable source.
-}
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

-- | Control characters outside ordinary whitespace.
controlCharred :: Text -> Bool
controlCharred = T.any (\c -> c < ' ' && c `notElem` ("\n\t\r" :: String))

-- | A GHCi @:info@ dump line: provenance comment or instance listing.
infoDumpLine :: Text -> Bool
infoDumpLine l =
    "-- Defined in" `T.isInfixOf` l
        || "instance " `T.isPrefixOf` T.stripStart l

-- | Any single-token leak shape.
leakyToken :: Text -> Bool
leakyToken w = longHashToken w || hashQualifiedToken w

-- | Any per-line leak shape a verifier surface must never emit.
leakyLine :: Text -> Bool
leakyLine l =
    embeddedSerialisation l
        || controlCharred l
        || any leakyToken (T.words l)
