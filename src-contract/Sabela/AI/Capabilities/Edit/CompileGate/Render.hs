{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.CompileGate.Render (
    renderNonExecuting,
    renderForDiagnostics,
    renderForParsing,
    isGeneratedBinder,
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
    linePragma,
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

data RenderGroup = GStatements [Piece] | GOther Piece

{- | scripths classifies any unrecognised lead line as an action, which catches
top-level declaration keywords it has no rule for (@foreign@, @deriving@). Those
are declarations and must not be folded into a @do@ block.
-}
isStatement :: Piece -> Bool
isStatement (PUnit KAction ls) = not (declHeaded ls)
isStatement (PUnit KIOBind ls) = not (declHeaded ls)
isStatement _ = False

declHeaded :: [Line] -> Bool
declHeaded [] = False
declHeaded (l : _) = firstToken (lineText l) `elem` declKeywords

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

statementBody :: Piece -> Text
statementBody (PUnit _ ls) = bodyOf ls
statementBody _ = ""

doBlock :: Int -> [Text] -> [Text]
doBlock i stmts =
    [":{"] ++ doBinding ("_sabelaGateStmts" <> T.pack (show i)) stmts ++ [":}"]

{- | A run of statements as one @name = do@ binding, with @pure ()@ appended
when the run ends in a bind so the block still closes on an expression.
-}
doBinding :: Text -> [Text] -> [Text]
doBinding name stmts =
    (name <> " = do")
        : map ("    " <>) (concatMap T.lines (stmts <> tailStatement))
  where
    tailStatement
        | maybe False endsInBind (lastOf stmts) = ["pure ()"]
        | otherwise = []
    endsInBind t = case bindStatementBody (lastLine t) of
        Just _ -> True
        Nothing -> False
    lastLine = fromMaybe "" . lastOf . T.lines
    lastOf xs = if null xs then Nothing else Just (last xs)

{- | Cell source as a parseable module body: each run of statements (actions and
@\<-@ binds) becomes one generated @do@ binding, everything else passes through.
A @LINE@ pragma per group keeps diagnostics in the cell's own coordinates.
-}
renderForParsing :: Text -> Text
renderForParsing src =
    T.intercalate
        "\n"
        (linePragma 1 cellTag : concat (zipWith parseGroup [0 :: Int ..] groups))
  where
    (sf, numbered) = parseScriptNumbered src
    groups =
        attachLines
            groupLen
            numbered
            (groupStatements (mergePieces (toPieces (scriptLines sf))))

cellTag :: Text
cellTag = "Cell"

{- | True for the binders 'renderForParsing' generates to hold statement runs.
Callers walking the parsed module must not mistake these for the cell's own.
-}
isGeneratedBinder :: Text -> Bool
isGeneratedBinder = T.isPrefixOf stmtsBinder

stmtsBinder :: Text
stmtsBinder = "_sabelaPreflightStmts"

parseGroup :: Int -> (Int, RenderGroup) -> [Text]
parseGroup i (n, GStatements ps) =
    linePragma n cellTag
        : doBinding (stmtsBinder <> T.pack (show i)) (map statementBody ps)
parseGroup _ (n, GOther p) = case parseLines p of
    [] -> []
    ls -> linePragma n cellTag : ls

parseLines :: Piece -> [Text]
parseLines PBlank = [""]
parseLines (PGhciCommand _) = []
parseLines (PPragma t) = [t]
parseLines (PImport t) = [t]
parseLines (PUnit KTHSplice ls) = T.lines (unRewriteSplice (bodyOf ls))
parseLines (PUnit _ ls) = T.lines (bodyOf ls)

{- | Pair each item with the source line its first 'Line' came from, by walking
the numbered lines and consuming each item's length.
-}
attachLines :: (a -> Int) -> [(Int, Line)] -> [a] -> [(Int, a)]
attachLines len numbered = go numbered 1
  where
    go _ _ [] = []
    go rest prev (x : xs) =
        let n = case rest of
                ((i, _) : _) -> i
                [] -> prev
         in (n, x) : go (drop (len x) rest) n xs

groupLen :: RenderGroup -> Int
groupLen (GStatements ps) = sum (map pieceLen ps)
groupLen (GOther p) = pieceLen p

pieceLen :: Piece -> Int
pieceLen (PUnit _ ls) = length ls
pieceLen _ = 1

renderForDiagnostics :: Text -> Text
renderForDiagnostics src =
    T.unlines (concat (zipWith pieceLines [0 :: Int ..] pieces))
  where
    pieces =
        regroupByBinder
            (mergePieces (toPieces (scriptLines (fst (parseScriptNumbered src)))))

regroupByBinder :: [Piece] -> [Piece]
regroupByBinder = concatMap split
  where
    split (PUnit KDeclaration ls) =
        [PUnit KDeclaration g | g <- groupByBinder ls]
    split p = [p]

groupByBinder :: [Line] -> [[Line]]
groupByBinder = foldr step []
  where
    step l [] = [[l]]
    step l (g : gs)
        | continues l (head g) = (l : g) : gs
        | otherwise = [l] : g : gs
    continues l next =
        let name = binderOf (lineText l)
         in isIndented (lineText next)
                || (not (T.null name) && name == binderOf (lineText next))

isIndented :: Text -> Bool
isIndented t = case T.uncons t of
    Just (c, _) -> c == ' ' || c == '\t'
    Nothing -> True

binderOf :: Text -> Text
binderOf t
    | isIndented t = ""
    | tok `elem` declKeywords = ""
    | not (T.null tok), isAlpha (T.head tok) || T.head tok == '_' = tok
    | otherwise = ""
  where
    tok = firstToken t

firstToken :: Text -> Text
firstToken =
    T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') . T.stripStart

declKeywords :: [Text]
declKeywords =
    [ "data"
    , "newtype"
    , "type"
    , "class"
    , "instance"
    , "foreign"
    , "deriving"
    , "default"
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

probeDecl :: Int -> Text -> [Text]
probeDecl i body =
    [":{", "_sabelaGateProbe" <> T.pack (show i) <> " = ("]
        ++ map ("    " <>) (T.lines body)
        ++ ["    )", ":}"]

dropBindPattern :: Text -> Text
dropBindPattern t = fromMaybe t (bindStatementBody t)
