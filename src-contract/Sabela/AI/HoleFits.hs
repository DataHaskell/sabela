{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.HoleFits (
    HoleFit (..),
    holeFitsJson,
    parseHoleFits,
    refinementFits,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (sanitizeTypeText)

data HoleFit = HoleFit
    { hfWrite :: Text
    , hfType :: Text
    , hfRefined :: Bool
    , hfModule :: Maybe Text
    }
    deriving (Eq, Show)

holeFitsJson :: Int -> Text -> [Value]
holeFitsJson cap = map render . take cap . parseHoleFits
  where
    render f =
        object
            ( [ "write" .= sanitizeTypeText (hfWrite f)
              , "type" .= sanitizeTypeText (hfType f)
              , "refined" .= hfRefined f
              ]
                <> ["module" .= m | Just m <- [hfModule f]]
            )

parseHoleFits :: Text -> [HoleFit]
parseHoleFits blob = case afterValidHeader (T.lines blob) of
    [] -> []
    body ->
        let (plainLs, refLs) = break isRefinementHeader body
         in plainFits plainLs ++ refinementSkeletons (drop 1 refLs)

afterValidHeader :: [Text] -> [Text]
afterValidHeader = drop 1 . dropWhile (not . isValidHeader)

isValidHeader :: Text -> Bool
isValidHeader = T.isInfixOf "Valid hole fits include"

isRefinementHeader :: Text -> Bool
isRefinementHeader = T.isInfixOf "Valid refinement hole fits include"

plainFits :: [Text] -> [HoleFit]
plainFits [] = []
plainFits (l : ls)
    | isEntryStart l =
        let (cont, rest) = span isTypeContinuation ls
            sig = T.unwords (map T.strip (l : cont))
            modu = importedModule (takeWhile isProvenance rest)
         in maybe
                id
                (\(n, t) -> (HoleFit n t False modu :))
                (splitNameType sig)
                (plainFits rest)
    | otherwise = plainFits ls
  where
    isEntryStart x = "::" `T.isInfixOf` x && not (isProvenance x)

importedModule :: [Text] -> Maybe Text
importedModule ls =
    case [ T.takeWhile (/= '\8217') r | l <- ls, let (_, r) = breakAfter l, not (T.null r)
         ] of
        (m : _) | not (T.null m) -> Just m
        _ -> Nothing
  where
    breakAfter l = fmap (T.drop (T.length marker)) (T.breakOn marker (T.strip l))
    marker = "(imported from \8216" :: Text

refinementSkeletons :: [Text] -> [HoleFit]
refinementSkeletons = map toFit . groupEntries
  where
    toFit (skel, blk) =
        HoleFit (T.strip skel) (whereType blk) True (importedModule blk)

groupEntries :: [Text] -> [(Text, [Text])]
groupEntries lns = case filter (not . blank) lns of
    [] -> []
    (first : _) -> go (indentOf first) (dropWhile blank lns)
  where
    go _ [] = []
    go base (x : xs)
        | blank x = go base xs
        | indentOf x <= base =
            let (blk, rest) = span (\y -> blank y || indentOf y > base) xs
             in (x, blk) : go base rest
        | otherwise = go base xs

whereType :: [Text] -> Text
whereType blk =
    typeAfterColon
        ( T.unwords
            (map T.strip (takeWhile (not . stops) (dropWhile (not . isWhere) blk)))
        )
  where
    isWhere x = "where" `T.isPrefixOf` T.strip x
    stops x = any (`T.isPrefixOf` T.strip x) ["with ", "(imported", "(and "]
    typeAfterColon s = case T.breakOn "::" s of
        (_, r) | not (T.null r) -> T.strip (T.drop 2 r)
        _ -> ""

isTypeContinuation :: Text -> Bool
isTypeContinuation x =
    not (blank x) && not ("::" `T.isInfixOf` x) && not (isProvenance x)

isProvenance :: Text -> Bool
isProvenance x =
    any (`T.isPrefixOf` T.strip x) ["with ", "where ", "(imported", "(and "]

splitNameType :: Text -> Maybe (Text, Text)
splitNameType sig = case T.breakOn "::" sig of
    (name, rest)
        | not (T.null rest) ->
            let n = T.strip name
                t = T.strip (T.drop 2 rest)
             in if T.null n || T.null t then Nothing else Just (n, t)
    _ -> Nothing

blank :: Text -> Bool
blank = T.null . T.strip

indentOf :: Text -> Int
indentOf = T.length . T.takeWhile (== ' ')

refinementFits :: Text -> [(Text, Text)]
refinementFits blob = concatMap fitOf afterHeader
  where
    afterHeader =
        drop 1 (dropWhile (not . T.isInfixOf "Valid refinement hole fits") ls)
    ls = T.lines blob
    fitOf l =
        let s = T.strip l
            (nm, rest) = T.breakOn " (_ :: " s
         in [ (nm, ty)
            | not (T.null nm)
            , not (T.any (== ' ') nm)
            , Just ty <- [balancedPrefix (T.drop 7 rest)]
            ]
    balancedPrefix = go (0 :: Int) ""
      where
        go d acc s = case T.uncons s of
            Nothing -> Nothing
            Just (c, rest)
                | c == ')' && d == 0 -> Just (T.pack (reverse acc))
                | c `elem` ("([" :: String) -> go (d + 1) (c : acc) rest
                | c `elem` (")]" :: String) -> go (d - 1) (c : acc) rest
                | otherwise -> go d (c : acc) rest
