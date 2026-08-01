{-# LANGUAGE OverloadedStrings #-}

{- | Shape predicates for text the harness is about to hand to a model:
what leaks build provenance, and what may be presented as a callable name.
-}
module Sabela.AI.LeakShape (
    controlCharred,
    doubleEncodedJson,
    embeddedSerialisation,
    hashQualifiedToken,
    infoDumpLine,
    leakyLine,
    leakyToken,
    lexicalEntity,
    longHashToken,
    scrubLeakyText,
    scrubLeakyToken,
) where

import Data.Char (isAlphaNum, isDigit, isHexDigit, isLetter, isUpper)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

embeddedSerialisation :: Text -> Bool
embeddedSerialisation = T.isInfixOf "{\""

doubleEncodedJson :: Text -> Bool
doubleEncodedJson = T.isInfixOf "{\\\""

longHashToken :: Text -> Bool
longHashToken = any hashSegment . T.split unitSeparator

hashSegment :: Text -> Bool
hashSegment seg = T.length seg >= 20 && T.all isHexDigit seg

unitSeparator :: Char -> Bool
unitSeparator c = c == '-' || c == ':'

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

{- | The one leak predicate. 'Nothing' means the token is clean; 'Just'
carries the repaired token. Detection ('leakyToken') is defined as its
presence, so a token can never be detected leaky yet be unrepairable.
-}
scrubLeakyToken :: Text -> Maybe Text
scrubLeakyToken w
    | leakShaped w = Just (settle w)
    | otherwise = Nothing
  where
    settle t
        | not (leakShaped t) = t
        | T.length peeled < T.length t = settle peeled
        | otherwise = t
      where
        peeled = peelLeak t

leakShaped :: Text -> Bool
leakShaped w = longHashToken w || hashQualifiedToken w

{- | Remove one layer of build provenance: a @pkg-ver[-hash]:@ unit qualifier,
or an interior hash component. Only the unit id itself goes — everything to its
left is kept, so brackets and neighbouring atoms survive the repair.
-}
peelLeak :: Text -> Text
peelLeak t
    | hashQualifiedToken t = T.dropWhileEnd unitChar pre <> T.drop 1 post
    | otherwise = dropHashSegments t
  where
    (pre, post) = T.breakOn ":" t
    unitChar c = isAlphaNum c || c `elem` ("-_." :: String)

dropHashSegments :: Text -> Text
dropHashSegments = T.concat . keep . T.groupBy sameClass
  where
    sameClass a b = unitSeparator a == unitSeparator b
    keep (sep : seg : rest)
        | T.all unitSeparator sep
        , hashSegment seg =
            keep rest
    keep (seg : rest)
        | hashSegment seg = keep rest
        | otherwise = seg : keep rest
    keep [] = []

leakyToken :: Text -> Bool
leakyToken = isJust . scrubLeakyToken

{- | Repair every leaky token in a line, preserving its layout. Deleting the
line instead would report a signature the harness never observed.
-}
scrubLeakyText :: Text -> Text
scrubLeakyText = T.intercalate " " . map one . T.splitOn " "
  where
    one w = fromMaybe w (scrubLeakyToken w)

leakyLine :: Text -> Bool
leakyLine l =
    embeddedSerialisation l
        || controlCharred l
        || any leakyToken (T.words l)

{- | Whether a token may be presented as a callable name: an identifier, an
operator section, or a qualified form of either. Anything else is a fragment
of a declaration the parser did not finish reading.
-}
lexicalEntity :: Text -> Bool
lexicalEntity t =
    not (T.null t)
        && (operatorSection t || all identPart (dotParts t))

dotParts :: Text -> [Text]
dotParts = filter (not . T.null) . T.splitOn "."

identPart :: Text -> Bool
identPart s = case T.uncons s of
    Just (c, rest) ->
        (isLetter c || c == '_')
            && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') rest
    Nothing -> False

operatorSection :: Text -> Bool
operatorSection s
    | "(" `T.isPrefixOf` s
    , ")" `T.isSuffixOf` s
    , T.length s > 2 =
        let (qual, op) = splitModuleQualifier (T.init (T.drop 1 s))
         in not (T.null op)
                && T.all (`elem` operatorChars) op
                && all identPart (dotParts qual)
    | otherwise = False
  where
    operatorChars = "!#$%&*+./<=>?@\\^|-~:" :: String

splitModuleQualifier :: Text -> (Text, Text)
splitModuleQualifier = go ""
  where
    go acc s = case T.breakOn "." s of
        (h, rest)
            | not (T.null rest)
            , moduleIdent h ->
                go (acc <> h <> ".") (T.drop 1 rest)
        _ -> (acc, s)
    moduleIdent h = case T.uncons h of
        Just (c, r) ->
            isUpper c
                && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') r
        Nothing -> False
