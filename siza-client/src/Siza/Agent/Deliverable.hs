{-# LANGUAGE OverloadedStrings #-}

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

requestedNames :: Text -> [Text]
requestedNames prompt =
    [ name
    | seg <- backtickSegments prompt
    , "::" `T.isInfixOf` seg
    , let name = T.takeWhile isIdentChar (T.strip seg)
    , not (T.null name)
    , maybe False (isLower . fst) (T.uncons name)
    ]

isIdentName :: Text -> Bool
isIdentName t =
    not (T.null t)
        && T.all isIdentChar t
        && maybe False (isLower . fst) (T.uncons t)

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

backtickSegments :: Text -> [Text]
backtickSegments t = case T.splitOn "`" t of
    (_ : rest) -> odds rest
    [] -> []
  where
    odds (x : _ : xs) = x : odds xs
    odds [x] = [x]
    odds [] = []

definesName :: Text -> Text -> Bool
definesName name src =
    any ((== name) . firstWord) (T.lines src)
  where
    firstWord = T.takeWhile (/= ' ') . T.stripStart

missingDeliverables :: Text -> [Text] -> [Text]
missingDeliverables prompt srcs =
    [n | n <- requestedNames prompt, not (any (definesName n) srcs)]
