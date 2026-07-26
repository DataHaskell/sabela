{-# LANGUAGE OverloadedStrings #-}

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

statementBody :: Piece -> Text
statementBody (PUnit _ ls) = bodyOf ls
statementBody _ = ""

doBlock :: Int -> [Text] -> [Text]
doBlock i stmts =
    [":{", name <> " = do"]
        ++ map ("    " <>) (concatMap T.lines (stmts <> tailStatement))
        ++ [":}"]
  where
    name = "_sabelaGateStmts" <> T.pack (show i)
    tailStatement
        | maybe False endsInBind (lastOf stmts) = ["pure ()"]
        | otherwise = []
    endsInBind t = case bindStatementBody (lastLine t) of
        Just _ -> True
        Nothing -> False
    lastLine = fromMaybe "" . lastOf . T.lines
    lastOf xs = if null xs then Nothing else Just (last xs)

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

probeDecl :: Int -> Text -> [Text]
probeDecl i body =
    [":{", "_sabelaGateProbe" <> T.pack (show i) <> " = (" <> body <> ")", ":}"]

dropBindPattern :: Text -> Text
dropBindPattern t = fromMaybe t (bindStatementBody t)
