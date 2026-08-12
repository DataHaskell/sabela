{-# LANGUAGE OverloadedStrings #-}

module Sabela.Errors where

import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Sabela.AI.Health (harnessNames)
import Sabela.Errors.Locate (
    attributeHeader,
    originCol,
    originLine,
    startsWithPosition,
 )
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

{- | Diagnostics from GHCi's plain-text stderr. A position filed under a cell's
LINE-pragma tag is a line of that cell and is kept; an @\<interactive\>@
position is a session position, and `attributeHeader` relabels it rather than
letting a reader take 685 for line 685 of a five-line cell.
-}
parseErrors :: Text -> [CellError]
parseErrors stderr
    | T.null stderr = []
    | otherwise = concatMap parseSingleError (splitErrors stderr)
  where
    parseSingleError block
        | isWarningBlock block = []
        | otherwise = case T.lines block of
            (hdr : rest) -> case attributeHeader hdr of
                Just (origin, hdr') ->
                    [ CellError
                        (originLine origin)
                        (originCol origin)
                        (T.strip (T.unlines (hdr' : rest)))
                        (ghcCodeIn block)
                    ]
                Nothing
                    | mentionsError block ->
                        [CellError Nothing Nothing (T.strip block) (ghcCodeIn block)]
                    | otherwise -> []
            _ -> []

{- | Whether a blob reports a diagnostic at all, as opposed to being harness
protocol text that happens to travel on the same stream.
-}
mentionsError :: Text -> Bool
mentionsError b = "error" `T.isInfixOf` T.toLower b

splitErrors :: Text -> [Text]
splitErrors t = filter (not . T.null . T.strip) (splitOnHeaders (T.lines t) [] [])

splitOnHeaders :: [Text] -> [Text] -> [Text] -> [Text]
splitOnHeaders [] current acc =
    let b = T.unlines (reverse current)
     in reverse (if T.null (T.strip b) then acc else b : acc)
splitOnHeaders (l : ls) current acc
    | isErrorHeader l && not (null current) =
        let b = T.unlines (reverse current)
         in splitOnHeaders ls [l] (b : acc)
    | otherwise = splitOnHeaders ls (l : current) acc

{- | A line that opens a diagnostic: it starts with a position, either an
interactive one or one filed under a cell's LINE-pragma tag.
-}
isErrorHeader :: Text -> Bool
isErrorHeader = startsWithPosition

{- | Everything in a stderr blob that is not a warning diagnostic. A warning is
not a failure, and reading it as one refuses cells that compile.
-}
dropWarningBlocks :: Text -> Text
dropWarningBlocks =
    T.strip
        . T.unlines
        . map T.stripEnd
        . filter (not . isWarningBlock)
        . splitErrors

{- | A diagnostic whose header says @warning:@ is not an error. Without this
the compile gate refuses any cell GHC merely warns about.
-}
isWarningBlock :: Text -> Bool
isWarningBlock block = case T.lines block of
    (hdr : _) -> "warning:" `T.isInfixOf` T.toLower hdr
    [] -> False

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

{- | Drops the context frames that name the harness's own wrapper, keeping every
other section. Truncating instead would lose the hole fits, which GHC prints
after the context rather than before it. A listing section is kept and pruned
entry by entry, so one harness name among the offers does not cost the model
the real ones. A diagnostic that is nothing but harness frames is kept whole:
scrubbing a failure into silence would report it as no failure at all. Harness
protocol text carries no diagnostic, so it still goes.
-}
scrubHarnessFrames :: Text -> Text
scrubHarnessFrames body
    | not (namesHarness body) = body
    | T.null (T.strip scrubbed), mentionsError body = body
    | otherwise = scrubbed
  where
    scrubbed = T.stripEnd (T.concat (map prune (filter keep (sections body))))
    keep s = isListing s || not (namesHarness s)
    prune s = if isListing s then dropHarnessLines s else s
    isListing s = any (`T.isInfixOf` s) listingHeads

namesHarness :: Text -> Bool
namesHarness s = any (`T.isInfixOf` s) harnessNames

{- | Removes the entries of a listing that name a harness binder, keeping the
section's own header line and every other entry. Splitting and rejoining on the
separator the blob itself uses leaves an escaped diagnostic escaped.
-}
dropHarnessLines :: Text -> Text
dropHarnessLines s = case [sep | sep <- frameSeparators, sep `T.isInfixOf` s] of
    (sep : _) -> case T.splitOn sep s of
        (hdr : rest) -> T.intercalate sep (hdr : filter (not . namesHarness) rest)
        [] -> s
    [] -> s

{- | Sections that list candidates rather than describe one frame. Their
entries stand alone, so they are pruned rather than dropped whole.
-}
listingHeads :: [Text]
listingHeads = ["Relevant bindings", "Valid hole fits", "Valid refinement"]

{- | Splits a diagnostic into sections, each carrying the separator that
introduced it so the text rejoins unchanged. GHC bullets and indents its
frames, so a section is recognised by line content, not by a fixed marker.
-}
sections :: Text -> [Text]
sections body = case [s | s <- frameSeparators, s `T.isInfixOf` body] of
    [] -> [body]
    (sep : _) ->
        zipWith (rejoin sep) [0 :: Int ..] (groupLines (T.splitOn sep body))
  where
    rejoin sep i g = (if i == 0 then "" else sep) <> T.intercalate sep g

{- | Groups lines into sections, starting one at each line that opens a frame
or a listing. The first group holds the headline, which opens neither.
-}
groupLines :: [Text] -> [[Text]]
groupLines [] = []
groupLines (l : ls) = go [l] ls
  where
    go acc [] = [reverse acc]
    go acc (x : xs)
        | headsSection x = reverse acc : go [x] xs
        | otherwise = go (x : acc) xs

-- | Whether a line opens a section, ignoring GHC's indentation and bullet.
headsSection :: Text -> Bool
headsSection l = any (`T.isPrefixOf` bare) sectionHeads
  where
    bare = T.dropWhile (\c -> isSpace c || c == '\8226') l

sectionHeads :: [Text]
sectionHeads = ["In ", "Relevant bindings", "Valid hole fits", "Valid refinement"]

frameSeparators :: [Text]
frameSeparators = ["\\n", "\n"]

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
