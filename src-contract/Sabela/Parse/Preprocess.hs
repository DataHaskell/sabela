{-# LANGUAGE OverloadedStrings #-}

module Sabela.Parse.Preprocess (
    preprocess,
    noTopLevelIn,
) where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T

preprocess :: Text -> [Text]
preprocess src = concatMap rewriteLine (T.lines src)
  where
    rewriteLine raw
        | shouldDrop trimmed = []
        | indented = [raw]
        | Just rest <- T.stripPrefix "let " raw
        , noTopLevelIn rest =
            [rest]
        | Just (binder, rhs) <- splitTopLevelArrow raw =
            [binder <> " = " <> rhs]
        | otherwise = [raw]
      where
        trimmed = T.stripStart raw
        indented = raw /= trimmed

    shouldDrop t =
        T.null t
            || ":" `T.isPrefixOf` t
            || "-- cabal:" `T.isPrefixOf` t
            || "--cabal:" `T.isPrefixOf` t

noTopLevelIn :: Text -> Bool
noTopLevelIn = go (0 :: Int) (0 :: Int) . T.unpack
  where
    go _ _ [] = True
    go p b (' ' : 'i' : 'n' : ' ' : _) | p == 0 && b == 0 = False
    go p b ('(' : rest) = go (p + 1) b rest
    go p b (')' : rest) = go (max 0 (p - 1)) b rest
    go p b ('[' : rest) = go p (b + 1) rest
    go p b (']' : rest) = go p (max 0 (b - 1)) rest
    go p b (_ : rest) = go p b rest

splitTopLevelArrow :: Text -> Maybe (Text, Text)
splitTopLevelArrow t =
    case findTopLevelArrow 0 0 (T.unpack t) of
        Nothing -> Nothing
        Just idx ->
            let (lhs, rhs0) = T.splitAt idx t
                rhs = T.drop 2 rhs0
                lhsTrim = T.strip lhs
             in if isSimpleIdent lhsTrim
                    then Just (lhsTrim, T.stripStart rhs)
                    else Nothing
  where
    findTopLevelArrow :: Int -> Int -> String -> Maybe Int
    findTopLevelArrow _ _ [] = Nothing
    findTopLevelArrow p b ('<' : '-' : _) | p == 0 && b == 0 = Just 0
    findTopLevelArrow p b ('(' : rest) =
        succPos <$> findTopLevelArrow (p + 1) b rest
    findTopLevelArrow p b (')' : rest) =
        succPos <$> findTopLevelArrow (max 0 (p - 1)) b rest
    findTopLevelArrow p b ('[' : rest) =
        succPos <$> findTopLevelArrow p (b + 1) rest
    findTopLevelArrow p b (']' : rest) =
        succPos <$> findTopLevelArrow p (max 0 (b - 1)) rest
    findTopLevelArrow p b (_ : rest) = succPos <$> findTopLevelArrow p b rest

    succPos :: Int -> Int
    succPos = (+ 1)

isSimpleIdent :: Text -> Bool
isSimpleIdent t = case T.uncons t of
    Just (c, rest) ->
        (Char.isLower c || c == '_')
            && T.all isIdentChar rest
    Nothing -> False
  where
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''
