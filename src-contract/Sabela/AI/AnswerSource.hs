{-# LANGUAGE OverloadedStrings #-}

{- | Where a query answer came from. The tag travels on the payload as @via@,
and the client's proof ledger reads it: only a compiler-backed source may add
a name to the resolved-names ledger. An index answer proves the world holds
the name, never that this session does.
-}
module Sabela.AI.AnswerSource (
    AnswerSource (..),
    sourceTag,
    parseSource,
    compilerBacked,
    answerSources,
    viaVocabulary,
    viaIsCompilerBacked,
) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)

data AnswerSource
    = SessionType
    | SessionInfo
    | LocalIndex
    | NotebookFacts
    | BuiltinVocab
    deriving (Bounded, Enum, Eq, Ord, Show)

answerSources :: [AnswerSource]
answerSources = [minBound .. maxBound]

sourceTag :: AnswerSource -> Text
sourceTag SessionType = "session-type"
sourceTag SessionInfo = "session-info"
sourceTag LocalIndex = "local-index"
sourceTag NotebookFacts = "notebook"
sourceTag BuiltinVocab = "builtin"

viaVocabulary :: [Text]
viaVocabulary = map sourceTag answerSources

parseSource :: Text -> Maybe AnswerSource
parseSource t = case mapMaybe match answerSources of
    (s : _) -> Just s
    [] -> Nothing
  where
    match s = if sourceTag s == t then Just s else Nothing

{- | True only for sources that asked a compiler. The index and the notebook
say what exists and what was written, not what type-checks now.
-}
compilerBacked :: AnswerSource -> Bool
compilerBacked SessionType = True
compilerBacked SessionInfo = True
compilerBacked LocalIndex = False
compilerBacked NotebookFacts = False
compilerBacked BuiltinVocab = False

-- | The @via@ tag on a payload, judged. An unknown tag is never trusted.
viaIsCompilerBacked :: Text -> Bool
viaIsCompilerBacked = maybe False compilerBacked . parseSource
