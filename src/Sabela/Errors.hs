{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Errors where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
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
                    Nothing
                        -- Only treat as error if it contains "error" or "Error"
                        | "error" `T.isInfixOf` T.toLower (T.strip block) ->
                            [CellError Nothing Nothing (T.strip block)]
                        | otherwise -> []
                _ -> []

    splitErrors t = filter (not . T.null . T.strip) $ splitOnHeaders (T.lines t) [] []

    -- Bang both accumulators so a long GHCi stderr doesn't pile a
    -- thunk chain until the terminal 'reverse' forces it.
    splitOnHeaders [] !current !acc =
        let b = T.unlines (reverse current)
         in reverse (if T.null (T.strip b) then acc else b : acc)
    splitOnHeaders (l : ls) !current !acc
        | isErrorHeader l && not (null current) =
            let b = T.unlines (reverse current)
             in splitOnHeaders ls [l] (b : acc)
        | otherwise = splitOnHeaders ls (l : current) acc

    isErrorHeader l =
        "<interactive>:" `T.isPrefixOf` l
            || "<cell>:" `T.isPrefixOf` l

parseErrorHeader :: Text -> Maybe (Int, Maybe Int)
parseErrorHeader hdr = do
    rest <-
        T.stripPrefix "<interactive>:" hdr
            <|> T.stripPrefix "<cell>:" hdr
    case TR.decimal rest of
        Right (ln, rest2) ->
            let col = case T.stripPrefix ":" rest2 of
                    Just r3 -> case TR.decimal r3 of
                        Right (c, _) -> Just c
                        _ -> Nothing
                    Nothing -> Nothing
             in Just (ln, col)
        _ -> Nothing
