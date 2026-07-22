{-# LANGUAGE OverloadedStrings #-}

{- | Cell-source normalization for AI inserts: detect Haskell mis-typed as a
prose cell, and unwrap a top-level @main@ to bare top-level code so it runs. The
mutation chokepoint ('Sabela.AI.Capabilities.Edit') applies 'normalizeInsert'
and reports a note for each correction; the parsing itself reuses
"Sabela.Parse".
-}
module Sabela.Parse.Normalize (
    looksLikeHaskellCode,
    unwrapMain,
    rewriteTopLevelLet,
    normalizeCode,
    normalizeInsert,
) where

import Data.Char (isLower)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.NormalizeProposals (
    foldCabalComments,
    renameKeywordBindings,
 )
import Sabela.Model (CellType (..))
import Sabela.Parse (CellSymbols (..), cellSymbols)
import Sabela.Parse.Preprocess (noTopLevelIn)

{- | True if the cell has a top-level @main@ binding or signature. Textual, not
AST-based, so it still fires on cells the parser chokes on — Template Haskell
splices, a LANGUAGE pragma the GHCi-fragment wrapper can't place — which is
exactly where a defined-but-never-run @main@ slips through. The line test mirrors
'unwrapMain'\'s own @isMainLine@, so the gate and the rewrite agree.
-}
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

{- | Rewrite a top-level @main@ to bare top-level code so the cell runs: a
@main :: …@ signature and the @main = @ of @main = do …@ are dropped, leaving
@do …@; @main = e@ becomes @e@. No top-level main → unchanged.
-}
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

{- | True when a cell's source is unmistakably Haskell, not prose: the first
non-blank line opens with an import / pragma / @-- cabal:@ / decl keyword / a
top-level binding or signature, or it parses to a top-level definition.
-}
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

{- | Rewrite a statement-form top-level @let@ into plain declarations, including a
multi-line block (de-indent the @let@ line and each continuation to its binding
column). A @let ... in ...@ expression and an indented (nested) @let@ are left;
a source with no such @let@ is preserved byte-identically (R7.2).
-}
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
    -- A continuation of the block: a non-blank line indented to at least the
    -- binding column (a shallower line ends the block).
    contAt bcol l =
        not (T.null (T.strip l)) && T.length (T.takeWhile (== ' ') l) >= bcol

{- | The code-cell generator composition, one note per fix: cabal-comment
fold, top-level @let@ rewrite, keyword-binding rename, @main@ unwrap. The
UNGATED candidate — 'Sabela.AI.NormalizeGate' vets it before it is kept.
-}
normalizeCode :: Text -> (Text, [Text])
normalizeCode src = (unMained, notes)
  where
    (deCabal, cabalNotes) = foldCabalComments src
    deLet = rewriteTopLevelLet deCabal
    (renamed, renameNotes) = renameKeywordBindings deLet
    unMained = unwrapMain renamed
    notes =
        cabalNotes
            <> [letMsg | deLet /= deCabal]
            <> renameNotes
            <> [mainMsg | unMained /= renamed]
    letMsg = "Rewrote a top-level `let x = …` to a plain `x = …` declaration."
    mainMsg = "Rewrote `main` to a top-level do so the cell runs."

{- | Normalize an AI-inserted cell: a prose cell that is really Haskell becomes a
code cell, and the 'normalizeCode' generators run on code cells. Returns the
type, source, and a note per fix.
-}
normalizeInsert :: CellType -> Text -> (CellType, Text, [Text])
normalizeInsert ty src = (ty', src', notes)
  where
    reclassified = ty == ProseCell && looksLikeHaskellCode src
    ty' = if reclassified then CodeCell else ty
    (src', codeNotes) =
        if ty' == CodeCell then normalizeCode src else (src, [])
    notes = [reclassMsg | reclassified] <> codeNotes
    reclassMsg = "Inserted as a CodeCell — the source is Haskell, not prose."
