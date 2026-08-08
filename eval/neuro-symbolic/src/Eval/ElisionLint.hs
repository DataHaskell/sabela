{-# LANGUAGE OverloadedStrings #-}

module Eval.ElisionLint (elisionProblems, loadBearingLine) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.EmitLedger (loadBearingKeys)

elisionProblems :: [Value] -> [(Text, Text)]
elisionProblems = go Map.empty . map contentOf
  where
    go _ [] = []
    go seen (c : rest) =
        [ ("elided-load-bearing-field", pfx)
        | pfx <- embeddedStubPrefixes c
        , Just orig <- [Map.lookup pfx seen]
        , any loadBearingLine (T.lines orig)
        ]
            <> [("elided-load-bearing-field", k) | k <- keyedStubs c]
            <> go (establish c seen) rest

keyedStubs :: Text -> [Text]
keyedStubs c =
    [ key
    | (pre, _) <- T.breakOnAll "[as established turn " c
    , Just key <- [valueKeyOf pre]
    , key `elem` loadBearingKeys
    ]

valueKeyOf :: Text -> Maybe Text
valueKeyOf pre0 = do
    pre1 <- dropQuote (T.stripEnd pre0)
    pre2 <- T.stripSuffix ":" (T.stripEnd pre1)
    pre3 <- dropQuote (T.stripEnd pre2)
    let key = T.takeWhileEnd (\ch -> ch /= '"' && ch /= '\\') pre3
    if T.null key then Nothing else Just key
  where
    dropQuote t = T.stripSuffix "\\\"" t <|> T.stripSuffix "\"" t

loadBearingLine :: Text -> Bool
loadBearingLine l =
    " :: " `T.isInfixOf` l
        || "build-depends:" `T.isInfixOf` l
        || "-- cabal:" `T.isInfixOf` l

establish :: Text -> Map Text Text -> Map Text Text
establish c seen =
    foldl'
        (\m block -> Map.insert (stubKey block) block m)
        seen
        (T.splitOn "\n\n" c)

stubKey :: Text -> Text
stubKey = T.take 40 . T.strip . T.takeWhile (/= '\n')

embeddedStubPrefixes :: Text -> [Text]
embeddedStubPrefixes c =
    concatMap stubPrefixes (filter (not . standaloneStub) (T.splitOn "\n\n" c))
  where
    standaloneStub block =
        let b = T.strip block
         in "[as established turn " `T.isPrefixOf` b
                && "\x2026]" `T.isSuffixOf` b

stubPrefixes :: Text -> [Text]
stubPrefixes c = case T.breakOn marker c of
    (_, rest)
        | T.null rest -> []
        | otherwise ->
            let after = T.drop (T.length marker) rest
             in case T.breakOn "\x2026]" (snd (T.breakOn ": " after)) of
                    (pfx, close)
                        | not (T.null close)
                        , unchangedRef after ->
                            T.drop 2 pfx : stubPrefixes after
                    _ -> stubPrefixes after
  where
    marker = "[as established turn "
    unchangedRef after =
        " (unchanged): " `T.isInfixOf` T.take 40 after

contentOf :: Value -> Text
contentOf (Object o) = case KM.lookup "content" o of
    Just (String s) -> s
    _ -> ""
contentOf _ = ""
