{-# LANGUAGE OverloadedStrings #-}

{- | Top-level @pat <- act@ statements as the declarations they leave behind:
the gate must keep the name without performing anything, so the pattern is
redeclared through a proxy that types it from the action and can never run.
-}
module Sabela.AI.Capabilities.Edit.CompileGate.Bind (
    bindParts,
    bindBlock,
    boundProxy,
    withBoundProxy,
) where

import Data.Text (Text)
import qualified Data.Text as T

{- | A top-level @pat <- act@ compiled as the declaration it would leave
behind: the proxy types the pattern from the action, so later code sees the
name at its live type and nothing is performed.
-}
bindBlock :: Text -> Text -> [Text]
bindBlock pat body =
    [":{", pat <> " = " <> boundProxy <> " ("]
        ++ map ("    " <>) (T.lines body)
        ++ ["    )", ":}"]

boundProxy :: Text
boundProxy = "_sabelaGateBound"

{- | Declared once, ahead of the first bind block that applies it. @error@
rather than a runner: forcing it is a harness bug, never a way to run IO.
-}
boundProxyBlock :: [Text]
boundProxyBlock =
    [ ":{"
    , boundProxy <> " :: IO a -> a"
    , boundProxy
        <> " = error \"sabela gate: a bound statement is compiled, never \
           \performed\""
    , ":}"
    ]

withBoundProxy :: [Text] -> [Text]
withBoundProxy ls
    | any (T.isInfixOf boundProxy) ls = boundProxyBlock <> ls
    | otherwise = ls

{- | Split a bind statement at its top-level @<-@ into pattern and body. The
scan mirrors scripths' 'ScriptHs.Render.bindStatementBody', which yields only
the body; the gate must redeclare the pattern, so it needs both sides.
-}
bindParts :: Text -> Maybe (Text, Text)
bindParts full = go (0 :: Int) full
  where
    go depth t = case T.uncons t of
        Nothing -> Nothing
        Just ('"', rest) -> go depth (skipString rest)
        Just ('\'', rest) -> go depth (skipChar rest)
        Just ('<', rest)
            | depth == 0
            , "-" `T.isPrefixOf` rest ->
                let pos = T.length full - T.length t
                    pat = T.strip (T.take pos full)
                    body = T.strip (T.drop (pos + 2) full)
                 in if T.null pat || T.null body
                        then Nothing
                        else Just (pat, body)
        Just (c, rest)
            | c `elem` ("([{" :: String) -> go (depth + 1) rest
            | c `elem` (")]}" :: String) -> go (depth - 1) rest
            | otherwise -> go depth rest
    skipString t = case T.uncons t of
        Nothing -> t
        Just ('\\', rest) -> skipString (T.drop 1 rest)
        Just ('"', rest) -> rest
        Just (_, rest) -> skipString rest
    skipChar t = case T.uncons t of
        Just ('\\', rest) -> T.drop 1 (T.dropWhile (/= '\'') rest)
        Just (_, rest) | "'" `T.isPrefixOf` rest -> T.drop 1 rest
        _ -> t
