{- |
Technique: exemplar memory [Context Economy].
Guarantee: dark unless SIZA_EXEMPLAR_STORE is set; off by default.
Entry: 'retrieveForPrompt'.
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

data Exemplar = Exemplar
    { exTask :: Text
    , exSource :: Text
    }
    deriving (Eq, Show)

instance ToJSON Exemplar where
    toJSON e = object ["task" .= exTask e, "source" .= exSource e]

instance FromJSON Exemplar where
    parseJSON = withObject "Exemplar" $ \o -> Exemplar <$> o .: "task" <*> o .: "source"

exemplarStorePath :: IO (Maybe FilePath)
exemplarStorePath = lookupEnv "SIZA_EXEMPLAR_STORE"

saveExemplar :: FilePath -> Exemplar -> IO ()
saveExemplar fp e = BS.appendFile fp (BL.toStrict (encode e) <> "\n")

loadExemplars :: FilePath -> IO [Exemplar]
loadExemplars fp = do
    exists <- doesFileExist fp
    if not exists
        then pure []
        else do
            raw <- BS.readFile fp
            pure (mapMaybe decodeStrict' (filter (not . BS.null) (BC.lines raw)))

retrieveExemplars :: Int -> Text -> [Exemplar] -> [Exemplar]
retrieveExemplars k task =
    take k
        . map snd
        . sortOn (Down . fst)
        . filter ((>= 0.1) . fst)
        . map (\e -> (trigramSimilarity task (exTask e), e))

retrieveForPrompt :: Text -> IO [Value]
retrieveForPrompt prompt = do
    mstore <- exemplarStorePath
    case mstore of
        Nothing -> pure []
        Just fp -> do
            exs <- loadExemplars fp
            pure (exemplarMessage (retrieveExemplars 2 prompt exs))

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
