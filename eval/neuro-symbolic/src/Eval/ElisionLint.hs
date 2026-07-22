{-# LANGUAGE OverloadedStrings #-}

{- | R8-T1 x R8.4: load-bearing fields are elision-exempt. A dedup
back-reference stub (@[as established turn N (unchanged): anchor…]@) that is
EMBEDDED where a signature or cabal line was due — a load-bearing key's
value, or inline in a block citing the fact — elided it at delivery (the
barChart Plot-signature class). A stub standing alone as its own paragraph
is the honest whole-chunk back-reference of a true repeat (search-api §10,
EmitLedgerProtectSpec) and passes. Content-only: read off the messages.
-}
module Eval.ElisionLint (elisionProblems, loadBearingLine) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.EmitLedger (loadBearingKeys)

-- | (rule, detail) pairs, shaped for "Eval.TranscriptLint" wiring.
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

{- | Load-bearing keys whose JSON value IS a dedup stub (the run-181807
barChart class: @"type": "[as established turn 1 (unchanged): …]"@) — an
elision at the moment of delivery, whatever established the anchor.
-}
keyedStubs :: Text -> [Text]
keyedStubs c =
    [ key
    | (pre, _) <- T.breakOnAll "[as established turn " c
    , Just key <- [valueKeyOf pre]
    , key `elem` loadBearingKeys
    ]

-- | The JSON key whose value position @pre@'s tail opens, plain or escaped.
valueKeyOf :: Text -> Maybe Text
valueKeyOf pre0 = do
    pre1 <- dropQuote (T.stripEnd pre0)
    pre2 <- T.stripSuffix ":" (T.stripEnd pre1)
    pre3 <- dropQuote (T.stripEnd pre2)
    let key = T.takeWhileEnd (\ch -> ch /= '"' && ch /= '\\') pre3
    if T.null key then Nothing else Just key
  where
    dropQuote t = T.stripSuffix "\\\"" t <|> T.stripSuffix "\"" t

{- | A line the caller pastes or acts on verbatim: a type signature or a
cabal\/build-depends line. Everything else is summarisable prose.
-}
loadBearingLine :: Text -> Bool
loadBearingLine l =
    " :: " `T.isInfixOf` l
        || "build-depends:" `T.isInfixOf` l
        || "-- cabal:" `T.isInfixOf` l

{- | Record each paragraph block under its stub key (the anchor's first 40
chars, mirroring EmitLedger.backRef); the LATEST block under an anchor is
the identity a later reference points at.
-}
establish :: Text -> Map Text Text -> Map Text Text
establish c seen =
    foldl'
        (\m block -> Map.insert (stubKey block) block m)
        seen
        (T.splitOn "\n\n" c)

stubKey :: Text -> Text
stubKey = T.take 40 . T.strip . T.takeWhile (/= '\n')

{- | Anchor prefixes of stubs NOT standing alone as their own paragraph
block: only those elide at delivery; a standalone block IS the back-ref.
-}
embeddedStubPrefixes :: Text -> [Text]
embeddedStubPrefixes c =
    concatMap stubPrefixes (filter (not . standaloneStub) (T.splitOn "\n\n" c))
  where
    standaloneStub block =
        let b = T.strip block
         in "[as established turn " `T.isPrefixOf` b
                && "\x2026]" `T.isSuffixOf` b

-- | Every @[as established turn N (unchanged): prefix…]@ anchor prefix in c.
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
