{-# LANGUAGE OverloadedStrings #-}

{- | The learning loop's memory: grade-verified solutions persisted and retrieved as
in-context exemplars for similar tasks. This is the one grounding form a weak model
actually follows — imitation of a working example in context, not a spec it ignores
or a tool result it discards. Search (rejection sampling) generates the verified data;
this module stores it and feeds it back so each solved task makes the next easier.

Gated by @SIZA_EXEMPLAR_STORE@ (a JSONL path). Unset ⇒ no load, no save, so the
default episode is byte-identical.
-}
module Siza.Agent.Exemplars (
    Exemplar (..),
    exemplarStorePath,
    saveExemplar,
    saveVerified,
    loadExemplars,
    retrieveExemplars,
    retrieveForPrompt,
    exemplarMessage,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    decodeStrict',
    encode,
    object,
    withObject,
    (.:),
    (.=),
 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

import Sabela.AI.Similarity (trigramSimilarity)

-- | One grade-verified solution: the task it solved and the working cell source.
data Exemplar = Exemplar
    { exTask :: Text
    , exSource :: Text
    }
    deriving (Eq, Show)

instance ToJSON Exemplar where
    toJSON e = object ["task" .= exTask e, "source" .= exSource e]

instance FromJSON Exemplar where
    parseJSON = withObject "Exemplar" $ \o -> Exemplar <$> o .: "task" <*> o .: "source"

-- | The exemplar-store path from @SIZA_EXEMPLAR_STORE@; Nothing disables the loop.
exemplarStorePath :: IO (Maybe FilePath)
exemplarStorePath = lookupEnv "SIZA_EXEMPLAR_STORE"

-- | Append a verified exemplar as one JSONL line.
saveExemplar :: FilePath -> Exemplar -> IO ()
saveExemplar fp e = BS.appendFile fp (BL.toStrict (encode e) <> "\n")

-- | Load all exemplars from the JSONL store (empty when the file is absent).
loadExemplars :: FilePath -> IO [Exemplar]
loadExemplars fp = do
    exists <- doesFileExist fp
    if not exists
        then pure []
        else do
            raw <- BS.readFile fp
            pure (mapMaybe decodeStrict' (filter (not . BS.null) (BC.lines raw)))

{- | The @k@ exemplars most similar to the task by trigram similarity of the task
text, above a small floor so an unrelated exemplar is not injected. Best first.
-}
retrieveExemplars :: Int -> Text -> [Exemplar] -> [Exemplar]
retrieveExemplars k task =
    take k
        . map snd
        . sortOn (Down . fst)
        . filter ((>= 0.1) . fst)
        . map (\e -> (trigramSimilarity task (exTask e), e))

{- | Retrieve the exemplar message for a similar prompt; [] when the store
env is unset (the default episode stays byte-identical).
-}
retrieveForPrompt :: Text -> IO [Value]
retrieveForPrompt prompt = do
    mstore <- exemplarStorePath
    case mstore of
        Nothing -> pure []
        Just fp -> do
            exs <- loadExemplars fp
            pure (exemplarMessage (retrieveExemplars 2 prompt exs))

{- | Persist a verified deliverable's healthy sources; no-op when the store
env is unset or the sources are blank.
-}
saveVerified :: Text -> [Text] -> IO ()
saveVerified prompt srcs = do
    mstore <- exemplarStorePath
    case mstore of
        Nothing -> pure ()
        Just fp -> do
            let src = T.intercalate "\n\n" srcs
            if T.null (T.strip src)
                then pure ()
                else saveExemplar fp (Exemplar prompt src)

{- | Render exemplars as one in-context user message (imitation, not a spec): the
working sources under a "similar solved task" heading, adapted by the model. Empty
list ⇒ no message.
-}
exemplarMessage :: [Exemplar] -> [Value]
exemplarMessage [] = []
exemplarMessage es =
    [ object
        [ "role" .= ("user" :: Text)
        , "content" .= body
        ]
    ]
  where
    body =
        "Here "
            <> (if length es == 1 then "is a" else "are")
            <> " verified working solution"
            <> (if length es == 1 then "" else "s")
            <> " to a SIMILAR task. Adapt the exact pattern (names, types, \
               \imports) to this task:\n\n"
            <> T.intercalate "\n\n" (map fence es)
    fence e = "```haskell\n" <> exSource e <> "\n```"
