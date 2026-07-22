{-# LANGUAGE OverloadedStrings #-}

{- | What the task actually asked for, read from the prompt: the
backtick-quoted @`name :: sig`@ deliverables, so the verify re-prompt can name
WHAT is missing instead of only that the check fails.
-}
module Siza.Agent.Deliverable (
    backtickSegments,
    definesName,
    isIdentName,
    missingDeliverables,
    requestedNames,
) where

import Data.Char (isAlphaNum, isLower)
import Data.Text (Text)
import qualified Data.Text as T

{- | The deliverable names a prompt requests: the leading identifier of each
backtick-quoted segment that carries a type signature (@`evalExpr :: …`@).
-}
requestedNames :: Text -> [Text]
requestedNames prompt =
    [ name
    | seg <- backtickSegments prompt
    , "::" `T.isInfixOf` seg
    , let name = T.takeWhile isIdentChar (T.strip seg)
    , not (T.null name)
    , maybe False (isLower . fst) (T.uncons name)
    ]

-- | Is this a lone lowercase Haskell identifier (a bindable value name)?
isIdentName :: Text -> Bool
isIdentName t =
    not (T.null t)
        && T.all isIdentChar t
        && maybe False (isLower . fst) (T.uncons t)

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | The text between each pair of backticks.
backtickSegments :: Text -> [Text]
backtickSegments t = case T.splitOn "`" t of
    (_ : rest) -> odds rest
    [] -> []
  where
    odds (x : _ : xs) = x : odds xs
    odds [x] = [x]
    odds [] = []

-- | Does any line of the source bind this name (definition or signature)?
definesName :: Text -> Text -> Bool
definesName name src =
    any ((== name) . firstWord) (T.lines src)
  where
    firstWord = T.takeWhile (/= ' ') . T.stripStart

-- | Requested deliverables that no cell source defines.
missingDeliverables :: Text -> [Text] -> [Text]
missingDeliverables prompt srcs =
    [n | n <- requestedNames prompt, not (any (definesName n) srcs)]
