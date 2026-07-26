{-# LANGUAGE OverloadedStrings #-}

{- | Render a candidate cell for G1's compile gate: the same GHCi input a
committed cell would get, except every non-declaration piece — an action or
monadic bind 'ScriptHs.Render.toGhciScript' would feed to GHCi as a
statement to EXECUTE — is rebound to a fresh, unused top-level name instead.
GHC still type-checks it (the whole point of the gate); nothing is ever
forced, so nothing runs. The same trick 'Sabela.AI.Capabilities.Try' already
uses for a single trial expression ('hiddenExpressionBinding'), generalized
to an arbitrary multi-piece cell.

A consecutive run of statements is rendered as ONE non-executing @do@ block
(see 'doBlock'), so a bind's name stays in scope for the statements after
it. Remaining limitation: a top-level DECLARATION that references a
do-bound name is still not seen, because the declaration cannot live inside
the block; GHCi's own top-level binds do allow that, so such a cell is
rejected though it would run.
-}
module Sabela.AI.Capabilities.Edit.CompileGate.Render (
    renderNonExecuting,
    renderForDiagnostics,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import ScriptHs.Parser (Line, parseScriptNumbered, scriptLines)
import ScriptHs.Render (
    Kind (..),
    Piece (..),
    bindStatementBody,
    lineText,
    mergePieces,
    toPieces,
    unRewriteSplice,
 )

renderNonExecuting :: Text -> Text
renderNonExecuting src =
    T.unlines (concat (zipWith renderGroup [0 :: Int ..] (groupStatements pieces)))
  where
    pieces =
        mergePieces (toPieces (scriptLines (fst (parseScriptNumbered src))))

{- | A consecutive run of statements, or any other piece. The run is what
lets a bind stay in scope for the statements after it.
-}
data RenderGroup = GStatements [Piece] | GOther Piece

isStatement :: Piece -> Bool
isStatement (PUnit KAction _) = True
isStatement (PUnit KIOBind _) = True
isStatement _ = False

groupStatements :: [Piece] -> [RenderGroup]
groupStatements [] = []
groupStatements (p : ps)
    | isStatement p =
        let (run, rest) = span isStatement (p : ps)
         in GStatements run : groupStatements rest
    | otherwise = GOther p : groupStatements ps

renderGroup :: Int -> RenderGroup -> [Text]
renderGroup i (GStatements ps) = doBlock i (map statementBody ps)
renderGroup i (GOther p) = pieceLines i p

-- | A statement as it appears in a do block: a bind keeps its pattern.
statementBody :: Piece -> Text
statementBody (PUnit _ ls) = bodyOf ls
statementBody _ = ""

{- | One run of statements as a single non-executing @do@ block bound to a
fresh, unused name. GHC type-checks the whole sequence — so a bind's name IS
in scope for the statements after it — and nothing is ever forced, so
nothing runs.

This is what replaced the old per-statement probe, which dropped each bind's
pattern and therefore rejected the commonest idiom there is: @df <- readCsv
…@ followed by any use of @df@. The gate was refusing correct cells, which
is a worse failure than the one it guards against — live_test24 could not
load a CSV at all.
-}
doBlock :: Int -> [Text] -> [Text]
doBlock i stmts =
    [":{", name <> " = do"]
        ++ map ("    " <>) (concatMap T.lines (stmts <> tailStatement))
        ++ [":}"]
  where
    name = "_sabelaGateStmts" <> T.pack (show i)
    -- A do block may not END in a bind, and a cell often does.
    tailStatement
        | maybe False endsInBind (lastOf stmts) = ["pure ()"]
        | otherwise = []
    endsInBind t = case bindStatementBody (lastLine t) of
        Just _ -> True
        Nothing -> False
    lastLine = fromMaybe "" . lastOf . T.lines
    lastOf xs = if null xs then Nothing else Just (last xs)

{- | Evidence rendering for G6: like 'renderNonExecuting', but each binder gets
its own block instead of one block for the whole declaration run. GHC halts a
block at its first error, so a merged run hides every defect after the first;
splitting per binder lets independent defects be seen — and proved — together.

Never use this to decide a commit. It is a strictly more permissive view of the
cell than GHCi will actually take, so 'renderNonExecuting' remains the verdict.
-}
renderForDiagnostics :: Text -> Text
renderForDiagnostics src =
    T.unlines (concat (zipWith pieceLines [0 :: Int ..] pieces))
  where
    pieces =
        regroupByBinder
            (mergePieces (toPieces (scriptLines (fst (parseScriptNumbered src)))))

{- | Split each merged declaration run back into one unit per binder, keeping a
signature with the binder it introduces and all equations of a binder together
(separating those would let a later clause shadow an earlier one).
-}
regroupByBinder :: [Piece] -> [Piece]
regroupByBinder = concatMap split
  where
    split (PUnit KDeclaration ls) =
        [PUnit KDeclaration g | g <- groupByBinder ls]
    split p = [p]

-- | Group declaration lines so each group covers exactly one binder.
groupByBinder :: [Line] -> [[Line]]
groupByBinder = foldr step []
  where
    step l [] = [[l]]
    step l (g : gs)
        | continues l (head g) = (l : g) : gs
        | otherwise = [l] : g : gs
    -- A line keeps the group below it when that group continues the same
    -- declaration: an indented continuation, or the same binder again (a
    -- signature and its binding, or another equation of one function).
    continues l next =
        let name = binderOf (lineText l)
         in isIndented (lineText next)
                || (not (T.null name) && name == binderOf (lineText next))

isIndented :: Text -> Bool
isIndented t = case T.uncons t of
    Just (c, _) -> c == ' ' || c == '\t'
    Nothing -> True

{- | The name a top-level declaration line binds, or empty when the line is a
continuation or a declaration form (@data@, @class@, …) that binds no value.
-}
binderOf :: Text -> Text
binderOf t
    | isIndented t = ""
    | tok `elem` declKeywords = ""
    | not (T.null tok), isAlpha (T.head tok) || T.head tok == '_' = tok
    | otherwise = ""
  where
    tok = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') (T.stripStart t)

declKeywords :: [Text]
declKeywords =
    [ "data"
    , "newtype"
    , "type"
    , "class"
    , "instance"
    , "foreign"
    , "pattern"
    , "infixl"
    , "infixr"
    , "infix"
    ]

pieceLines :: Int -> Piece -> [Text]
pieceLines _ PBlank = [""]
pieceLines _ (PGhciCommand _) = []
pieceLines _ (PPragma t) = [t]
pieceLines _ (PImport t) = [t]
pieceLines _ (PUnit KComment ls) = wrapDecl (bodyOf ls)
pieceLines _ (PUnit KDeclaration ls) = wrapDecl (bodyOf ls)
pieceLines _ (PUnit KTHSplice ls) = wrapDecl (unRewriteSplice (bodyOf ls))
pieceLines i (PUnit KAction ls) = probeDecl i (bodyOf ls)
pieceLines i (PUnit KIOBind ls) = probeDecl i (dropBindPattern (bodyOf ls))

bodyOf :: [Line] -> Text
bodyOf = T.intercalate "\n" . map lineText

wrapDecl :: Text -> [Text]
wrapDecl body = [":{"] ++ T.lines body ++ [":}"]

{- | @pat <- expr@ (any pattern) becomes just @expr@: a probe never needs the
bound name, only that the right-hand side itself type-checks.
-}
probeDecl :: Int -> Text -> [Text]
probeDecl i body =
    [":{", "_sabelaGateProbe" <> T.pack (show i) <> " = (" <> body <> ")", ":}"]

{- | Drop a bind's pattern using the same statement-level notion of @<-@ that
classified the piece ('ScriptHs.Render.bindStatementBody'). Splitting on the
first textual @<-@ instead once truncated a list comprehension mid-expression
and probed the remainder, so the gate reported a parse error at a column the
candidate did not contain (the live_test19 regression).
-}
dropBindPattern :: Text -> Text
dropBindPattern t = fromMaybe t (bindStatementBody t)
