{-# LANGUAGE OverloadedStrings #-}

{- | Does an edit change what the kernel sees? Comments and layout do not;
code tokens do, and so do the directives that live inside comments
(@-- cabal:@, @-- compile@). One predicate, so the edit handler, the dirty
marker and the compiled-module differ cannot disagree.
-}
module Sabela.Parse.Change (
    significantCodeChange,
) where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Parser.Lexer (ParseResult (..), Token (..), lexTokenStream)
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)

import Sabela.Parse (parserDynFlags)
import ScriptHs.Parser (ScriptFile (..), parseScript)

{- | True when the kernel would see a different cell: different code tokens,
dependencies, extensions or compile directive. Comment and whitespace edits
are insignificant; unlexable sources fall back to textual comparison.
-}
significantCodeChange :: Text -> Text -> Bool
significantCodeChange old new
    | old == new = False
    | directivesOf old /= directivesOf new = True
    | otherwise = case (lexSignature old, lexSignature new) of
        (Just a, Just b) -> a /= b
        _ -> True

-- | Metadata and compile directive only: what the comments contribute.
directivesOf :: Text -> ScriptFile
directivesOf src = (parseScript src){scriptLines = []}

{- | The token stream with positions erased. 'lexTokenStream' lexes in raw
mode, which emits comments as tokens, so they are filtered here. A trailing
comment can swallow the layout-closing tokens at end of input, so the trailing
run of virtual closers is stripped; mid-stream layout stays significant.
-}
lexSignature :: Text -> Maybe [String]
lexSignature src =
    case lexTokenStream opts buf loc of
        POk _ toks ->
            Just
                ( dropTrailingClosers
                    [show t | t <- map unLoc toks, not (isComment t)]
                )
        PFailed _ -> Nothing
  where
    opts = initParserOpts parserDynFlags
    buf = stringToStringBuffer (T.unpack src)
    loc = mkRealSrcLoc (fsLit "cell") 1 1

dropTrailingClosers :: [String] -> [String]
dropTrailingClosers = L.dropWhileEnd (`elem` ["ITvccurly", "ITsemi"])

isComment :: Token -> Bool
isComment ITlineComment{} = True
isComment ITblockComment{} = True
isComment ITdocComment{} = True
isComment _ = False
