{-# LANGUAGE OverloadedStrings #-}

module Sabela.Parse.Normalize (
    looksLikeHaskellCode,
    unwrapMain,
    rewriteTopLevelLet,
    fixRawNewlineInString,
    fixSpuriousUnicodeEscapes,
    sanitizeTransport,
    normalizeCode,
    normalizeInsert,
) where

import Data.Char (chr, isHexDigit, isLower)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (readHex)

import Sabela.AI.NormalizeProposals (
    foldCabalComments,
    renameKeywordBindings,
 )
import Sabela.Model (CellType (..))
import Sabela.Parse (CellSymbols (..), cellSymbols)
import Sabela.Parse.Preprocess (noTopLevelIn)

definesMain :: Text -> Bool
definesMain = any isTopMain . T.lines
  where
    isTopMain l =
        topLevel l
            && firstWord l == Just "main"
            && (" :: " `T.isInfixOf` l || "=" `T.isInfixOf` l)
    topLevel l = case T.uncons l of
        Just (c, _) -> c /= ' ' && c /= '\t'
        Nothing -> False
    firstWord l = case T.words l of
        (w : _) -> Just w
        _ -> Nothing

unwrapMain :: Text -> Text
unwrapMain src
    | not (definesMain src) = src
    | otherwise = T.unlines (concatMap step (T.lines src))
  where
    step l
        | isMainLine l, " :: " `T.isInfixOf` l, not ("=" `T.isInfixOf` l) = []
        | isMainLine l
        , (lhs, rhs) <- T.breakOn "=" l
        , T.strip lhs == "main"
        , not (T.null rhs) =
            [T.stripStart (T.drop 1 rhs)]
        | otherwise = [l]
    isMainLine l = case T.uncons l of
        Just (c, _) -> c /= ' ' && c /= '\t' && firstWord l == Just "main"
        Nothing -> False
    firstWord l = case T.words l of
        (w : _) -> Just w
        _ -> Nothing

looksLikeHaskellCode :: Text -> Bool
looksLikeHaskellCode src =
    firstLineIsCode || not (S.null (csDefs (cellSymbols src)))
  where
    firstLineIsCode = case dropWhile T.null (map T.strip (T.lines src)) of
        [] -> False
        (l : _) -> any (`T.isPrefixOf` l) codeOpeners || bindingOrSig l
    codeOpeners =
        [ "import "
        , "{-#"
        , "-- cabal:"
        , "module "
        , "data "
        , "newtype "
        , "type "
        , "class "
        , "instance "
        ]
    bindingOrSig l = case T.uncons l of
        Just (c, _)
            | isLower c || c == '_' -> " :: " `T.isInfixOf` l || " = " `T.isInfixOf` l
        _ -> False

rewriteTopLevelLet :: Text -> Text
rewriteTopLevelLet src = T.intercalate "\n" (go (T.lines src)) <> trailing
  where
    trailing = if "\n" `T.isSuffixOf` src then "\n" else ""
    go [] = []
    go (line : rest) = case T.stripPrefix "let " line of
        Just body
            | noTopLevelIn body ->
                let bcol = T.length line - T.length (T.stripStart (T.drop 3 line))
                    (block, after) = span (contAt bcol) rest
                 in (T.drop bcol line : map (T.drop bcol) block) ++ go after
        _ -> line : go rest
    contAt bcol l =
        not (T.null (T.strip l)) && T.length (T.takeWhile (== ' ') l) >= bcol

fixRawNewlineInString :: Text -> Text
fixRawNewlineInString src = T.pack (outside (T.unpack src))
  where
    outside [] = []
    outside ('-' : '-' : rest) =
        let (body, rest') = break (== '\n') rest
         in '-' : '-' : body ++ outside rest'
    outside (c@'"' : rest) = c : inside rest
    outside (c : rest) = c : outside rest
    inside [] = []
    inside ('\\' : c : rest) = '\\' : c : inside rest
    inside ('"' : rest) = '"' : outside rest
    inside ('\n' : rest) = '\\' : 'n' : inside rest
    inside (c : rest) = c : inside rest

fixSpuriousUnicodeEscapes :: Text -> Text
fixSpuriousUnicodeEscapes src = T.pack (go (T.unpack src))
  where
    go [] = []
    go ('\\' : 'u' : rest)
        | (hex, rest') <- splitAt 4 rest
        , length hex == 4
        , all isHexDigit hex
        , [(n, "")] <- readHex hex =
            chr n : go rest'
    go (c : rest) = c : go rest

sanitizeTransport :: Text -> (Text, [Text])
sanitizeTransport src = (afterUnicode, notes)
  where
    afterNewline = fixRawNewlineInString src
    afterUnicode = fixSpuriousUnicodeEscapes afterNewline
    notes =
        [newlineMsg | afterNewline /= src]
            <> [unicodeMsg | afterUnicode /= afterNewline]
    newlineMsg = "Escaped a raw newline found inside a string literal."
    unicodeMsg = "Rewrote a spurious `\\uXXXX` escape to the character it denotes."

normalizeCode :: Text -> (Text, [Text])
normalizeCode src = (unMained, notes)
  where
    (deTransport, transportNotes) = sanitizeTransport src
    (deCabal, cabalNotes) = foldCabalComments deTransport
    deLet = rewriteTopLevelLet deCabal
    (renamed, renameNotes) = renameKeywordBindings deLet
    unMained = unwrapMain renamed
    notes =
        transportNotes
            <> cabalNotes
            <> [letMsg | deLet /= deCabal]
            <> renameNotes
            <> [mainMsg | unMained /= renamed]
    letMsg = "Rewrote a top-level `let x = …` to a plain `x = …` declaration."
    mainMsg = "Rewrote `main` to a top-level do so the cell runs."

normalizeInsert :: CellType -> Text -> (CellType, Text, [Text])
normalizeInsert ty src = (ty', src', notes)
  where
    reclassified = ty == ProseCell && looksLikeHaskellCode src
    ty' = if reclassified then CodeCell else ty
    (src', codeNotes) =
        if ty' == CodeCell then normalizeCode src else (src, [])
    notes = [reclassMsg | reclassified] <> codeNotes
    reclassMsg = "Inserted as a CodeCell — the source is Haskell, not prose."
