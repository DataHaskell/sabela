{-# LANGUAGE OverloadedStrings #-}

module Eval.VerdictLint (
    verdictProblems,
    verifierChannels,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Verdict (parseVerdict)

verifierChannels :: [Text]
verifierChannels = ["verify", "try", "scratchpad"]

verdictProblems :: [Value] -> [(Text, Text)]
verdictProblems = go []
  where
    go _ [] = []
    go _ (m : rest)
        | strAt "role" m == "assistant" = go (callNames m) rest
    go pending (m : rest)
        | strAt "role" m == "tool" = flagged <> go pending rest
        | otherwise = go pending rest
      where
        name = strAt "tool_name" m
        c = strAt "content" m
        answersCall = name == "verify" && name `elem` pending
        flagged =
            [ ("verifier-no-verdict", name <> ": " <> T.take 60 c)
            | name `elem` verifierChannels
            , not answersCall
            , Nothing <- [parseVerdict c]
            ]

callNames :: Value -> [Text]
callNames m = case lookupField "tool_calls" m of
    Just (Array a) ->
        [ n
        | Object c <- toList a
        , Just (Object f) <- [KM.lookup "function" c]
        , Just (String n) <- [KM.lookup "name" f]
        ]
    _ -> []

strAt :: Text -> Value -> Text
strAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
strAt _ _ = ""

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KM.lookup (K.fromText k) o
lookupField _ _ = Nothing
