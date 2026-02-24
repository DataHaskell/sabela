{-# LANGUAGE OverloadedStrings #-}

module Sabela.Errors where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Model (CellError (..))

parseErrors :: Text -> [CellError]
parseErrors stderr
    | T.null stderr = []
    | otherwise = concatMap parseSingleError (splitErrors stderr)
  where
    parseSingleError block =
        let ls = T.lines block
         in case ls of
                (hdr : _) -> case parseErrorHeader hdr of
                    Just (ln, col) -> [CellError (Just ln) col (T.strip block)]
                    Nothing -> [CellError Nothing Nothing (T.strip block)]
                _ -> []

    splitErrors t = filter (not . T.null . T.strip) $ splitOnHeaders (T.lines t) [] []

    splitOnHeaders [] current acc =
        let b = T.unlines (reverse current)
         in reverse (if T.null (T.strip b) then acc else b : acc)
    splitOnHeaders (l : ls) current acc
        | isErrorHeader l && not (null current) =
            let b = T.unlines (reverse current)
             in splitOnHeaders ls [l] (b : acc)
        | otherwise = splitOnHeaders ls (l : current) acc

    isErrorHeader l =
        "<interactive>:" `T.isPrefixOf` l
            || "<cell>:" `T.isPrefixOf` l

parseErrorHeader :: Text -> Maybe (Int, Maybe Int)
parseErrorHeader hdr =
    let after = case T.stripPrefix "<interactive>:" hdr of
            Just r -> Just r
            Nothing -> T.stripPrefix "<cell>:" hdr
     in case after of
            Nothing -> Nothing
            Just rest ->
                let (lineStr, rest2) = T.span isDigit rest
                 in case reads (T.unpack lineStr) :: [(Int, String)] of
                        [(ln, _)] ->
                            let col = case T.stripPrefix ":" rest2 of
                                    Just r3 -> case reads (T.unpack (T.takeWhile isDigit r3)) :: [(Int, String)] of
                                        [(c, _)] -> Just c
                                        _ -> Nothing
                                    Nothing -> Nothing
                             in Just (ln, col)
                        _ -> Nothing
