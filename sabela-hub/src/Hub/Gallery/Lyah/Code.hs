{-# LANGUAGE OverloadedStrings #-}

-- | Code/GHCi cell helpers for the LYAH converter, split out for the cap.
module Hub.Gallery.Lyah.Code (codeChunks) where

import Data.Text (Text)
import qualified Data.Text as T

codeChunks :: Text -> [Text] -> [Text]
codeChunks attrs code
    | isGhci attrs = ghciChunks code
    | isPlain attrs = [staticBlock code]
    | otherwise = [haskellCell (T.intercalate "\n" code)]

isGhci :: Text -> Bool
isGhci attrs = "ghci" `T.isInfixOf` attrs

isPlain :: Text -> Bool
isPlain attrs = "plain" `T.isInfixOf` attrs && not ("ghci" `T.isInfixOf` attrs)

haskellCell :: Text -> Text
haskellCell body = "```haskell\n" <> body <> "\n```"

staticBlock :: [Text] -> Text
staticBlock code = "```text\n" <> T.intercalate "\n" code <> "\n```"

-- ---------------------------------------------------------------------------
-- GHCi transcripts
-- ---------------------------------------------------------------------------

{- | Split a GHCi transcript into chunks: one runnable @haskell@ cell per
expression input, with meta-commands and intentional-error examples preserved as
static @text@ blocks. The banner and bare prompts are dropped.
-}
ghciChunks :: [Text] -> [Text]
ghciChunks = go
  where
    go [] = []
    go (l : ls)
        | isPrompt l =
            let input = stripPrompt l
                (out, rest) = break isPrompt ls
             in classify l input out ++ go rest
        | otherwise = go ls -- banner / stray output before the first prompt
    classify raw input out
        | T.null (T.strip input) = [] -- bare "ghci>"
        | isMeta input || looksLikeError out = [staticBlock (raw : out)]
        | otherwise = [haskellCell input]

isPrompt :: Text -> Bool
isPrompt l = "ghci>" `T.isPrefixOf` T.stripStart l

stripPrompt :: Text -> Text
stripPrompt l = T.strip (T.drop (T.length "ghci>") (T.stripStart l))

isMeta :: Text -> Bool
isMeta = T.isPrefixOf ":"

looksLikeError :: [Text] -> Bool
looksLikeError = any (\o -> any (`T.isInfixOf` o) signals)
  where
    signals =
        [ "error:"
        , "<interactive>"
        , "Couldn't match"
        , "No instance"
        , "parse error"
        , "Not in scope"
        , "not in scope"
        , "cannot construct"
        , "rigid type variable"
        ]
