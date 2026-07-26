{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Errors where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Sabela.Model (CellError (..))
import ScriptHs.Compiled (parseLinePragmaTag)

ghcCodeIn :: Text -> Maybe Int
ghcCodeIn t = case T.breakOn "[GHC-" t of
    (_, rest)
        | not (T.null rest) ->
            let digits = T.takeWhile isDigit (T.drop 5 rest)
             in if T.null digits
                    then Nothing
                    else either (const Nothing) (Just . fst) (TR.decimal digits)
    _ -> Nothing

parseErrors :: Text -> [CellError]
parseErrors stderr
    | T.null stderr = []
    | otherwise = concatMap parseSingleError (splitErrors stderr)
  where
    parseSingleError block =
        let ls = T.lines block
         in case ls of
                (hdr : _) -> case parseErrorHeader hdr of
                    Just (ln, col) ->
                        [CellError (Just ln) col (T.strip block) (ghcCodeIn block)]
                    Nothing
                        | "error" `T.isInfixOf` T.toLower (T.strip block) ->
                            [CellError Nothing Nothing (T.strip block) (ghcCodeIn block)]
                        | otherwise -> []
                _ -> []

    splitErrors t = filter (not . T.null . T.strip) $ splitOnHeaders (T.lines t) [] []

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

parseCompiledErrors :: Text -> (M.Map Int [CellError], [CellError])
parseCompiledErrors stderrText =
    let blocks = splitCompiledBlocks stderrText
        errBlocks = [b | b <- blocks, "error" `T.isInfixOf` T.toLower b]
     in foldr addBlock (M.empty, []) errBlocks
  where
    addBlock block (m, loose) = case compiledHeader block of
        Just (cid, ln, col) ->
            let ce = CellError (Just ln) col (scrubTags (T.strip block)) (ghcCodeIn block)
             in (M.insertWith (++) cid [ce] m, loose)
        Nothing -> (m, CellError Nothing Nothing (T.strip block) (ghcCodeIn block) : loose)

compiledHeader :: Text -> Maybe (Int, Int, Maybe Int)
compiledHeader block = case T.lines block of
    (hdr : _) -> do
        cid <- parseLinePragmaTag (T.takeWhile (/= ':') hdr)
        rest <- T.stripPrefix ":" (T.dropWhile (/= ':') hdr)
        case TR.decimal rest of
            Right (ln, rest2) ->
                let col = case T.stripPrefix ":" rest2 of
                        Just r -> either (const Nothing) (Just . fst) (TR.decimal r)
                        Nothing -> Nothing
                 in Just (cid, ln, col)
            _ -> Nothing
    _ -> Nothing

scrubTags :: Text -> Text
scrubTags block = T.unlines (map scrubLine (T.lines block))
  where
    scrubLine l =
        let tag = T.takeWhile (/= ':') l
         in case parseLinePragmaTag tag of
                Just _ -> "line " <> T.drop 1 (T.dropWhile (/= ':') l)
                Nothing -> l

splitCompiledBlocks :: Text -> [Text]
splitCompiledBlocks t = go (T.lines t) [] []
  where
    go [] cur acc = reverse (flush cur acc)
    go (l : ls) cur acc
        | isHdr l && not (null cur) = go ls [l] (flush cur acc)
        | otherwise = go ls (l : cur) acc
    flush cur acc =
        let b = T.unlines (reverse cur)
         in if T.null (T.strip b) then acc else b : acc
    isHdr l = case parseLinePragmaTag (T.takeWhile (/= ':') l) of
        Just _ -> True
        Nothing -> False
