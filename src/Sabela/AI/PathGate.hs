{-# LANGUAGE OverloadedStrings #-}

{- | Refuses to commit a cell that names a file which is not there, and
carries the nearest real candidates on the refusal so the next attempt has
something to aim at.
-}
module Sabela.AI.PathGate (
    pathRefs,
    writesFiles,
    pathGateCheck,
    pathFacts,
    factsFor,
) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesPathExist)
import System.FilePath (isAbsolute, normalise, takeExtension, (</>))

import Sabela.Api (errorJsonWith)

import Sabela.AI.Artefact (artefactPairs, sampleFile)
import Sabela.AI.PathResolve (
    PathLookup (..),
    isUrl,
    lookupPath,
    workDirFiles,
 )

data Tok = TIdent Text | TStr Text

{- | A string literal together with the nearest identifier preceding it,
which is what decides whether the literal is being read or written.
-}
data Lit = Lit
    { litText :: Text
    , litHead :: Text
    }

tokens :: Text -> [Tok]
tokens = go "" . T.unpack
  where
    go acc s = case s of
        [] -> flush acc []
        ('-' : '-' : rest) -> flush acc (go "" (dropWhile (/= '\n') rest))
        ('{' : '-' : rest) -> flush acc (go "" (blockComment (1 :: Int) rest))
        ('"' : rest) ->
            let (body, rest') = stringBody "" rest
             in flush acc (TStr (T.pack body) : go "" rest')
        ('\'' : rest) | null acc -> go "" (charLiteral rest)
        (c : rest)
            | identChar c -> go (c : acc) rest
            | otherwise -> flush acc (go "" rest)
    flush acc rest
        | null acc = rest
        | otherwise = TIdent (T.pack (reverse acc)) : rest

identChar :: Char -> Bool
identChar c = isAlphaNum c || c `elem` ("_.'" :: String)

stringBody :: String -> String -> (String, String)
stringBody acc s = case s of
    ('\\' : c : rest) -> stringBody (c : '\\' : acc) rest
    ('"' : rest) -> (reverse acc, rest)
    (c : rest) -> stringBody (c : acc) rest
    [] -> (reverse acc, [])

charLiteral :: String -> String
charLiteral s = case s of
    ('\\' : _ : rest) -> charLiteral rest
    ('\'' : rest) -> rest
    (_ : rest) -> charLiteral rest
    [] -> []

blockComment :: Int -> String -> String
blockComment n s
    | n <= 0 = s
    | otherwise = case s of
        ('-' : '}' : rest) -> blockComment (n - 1) rest
        ('{' : '-' : rest) -> blockComment (n + 1) rest
        (_ : rest) -> blockComment n rest
        [] -> []

literals :: Text -> [Lit]
literals = go "" . tokens
  where
    go lastId ts = case ts of
        [] -> []
        (TIdent i : rest) -> go i rest
        (TStr s : rest) -> Lit s lastId : go lastId rest

pathLits :: Text -> [Lit]
pathLits = filter gateable . literals

-- | The path literals a cell names, in source order.
pathRefs :: Text -> [Text]
pathRefs = map litText . pathLits

{- | A literal counts as a path only when it carries a data extension and
either names a directory or is handed to something that reads. Inline markup
is full of slashes and must never be mistaken for a filename.
-}
gateable :: Lit -> Bool
gateable l =
    pathShaped (litText l)
        && (T.any (== '/') (litText l) || readsLocalFile (litHead l))

pathShaped :: Text -> Bool
pathShaped t =
    not (T.null t)
        && T.length t <= maxPathChars
        && not (T.any notInAPath t)
        && T.toLower (T.pack (takeExtension (T.unpack t))) `elem` dataExtensions

maxPathChars :: Int
maxPathChars = 200

notInAPath :: Char -> Bool
notInAPath c = c `elem` ("<>\"'{}|*?=\n\t " :: String)

dataExtensions :: [Text]
dataExtensions =
    [ ".csv"
    , ".tsv"
    , ".json"
    , ".parquet"
    , ".arrow"
    , ".feather"
    , ".txt"
    , ".dat"
    , ".md"
    , ".xlsx"
    , ".xls"
    , ".yaml"
    , ".yml"
    , ".html"
    , ".png"
    , ".jpg"
    , ".jpeg"
    , ".svg"
    , ".db"
    , ".sqlite"
    ]

shapedLike :: [Text] -> Text -> Bool
shapedLike needles n = any (`T.isInfixOf` T.toLower n) needles

{- | A cell that produces files is exempt: the paths it names are meant not
to exist yet.
-}
writesFiles :: Text -> Bool
writesFiles src =
    any (shapedLike writeWords) [i | TIdent i <- tokens src]
  where
    writeWords =
        ["write", "save", "encode", "export", "create", "append", "dump", "output"]

-- | Reads from the filesystem rather than over the network.
readsLocalFile :: Text -> Bool
readsLocalFile n = shapedLike readWords n && not (shapedLike fetchWords n)
  where
    readWords = ["read", "load", "open", "parse", "decode", "import"]
    fetchWords = ["http", "url", "fetch", "download", "request", "get"]

{- | What the gate learned about each path a cell names while proving it is
there. The gate stats every one of them; discarding the answer is what left
the model guessing at column names it had never read.
-}
pathFacts :: FilePath -> Text -> IO [Value]
pathFacts workDir src = catMaybes <$> mapM (factsFor workDir . litText) (pathLits src)

factsFor :: FilePath -> Text -> IO (Maybe Value)
factsFor workDir path
    | isUrl (T.unpack path) = pure Nothing
    | otherwise = do
        got <- sampleFile (resolve workDir path)
        pure (fmap (\a -> object (("path" .= path) : artefactPairs a)) got)

pathGateCheck ::
    FilePath -> [Text] -> Text -> IO (Either Value (Text, [Text]))
pathGateCheck workDir others src
    | writesFiles src = pure (Right (src, []))
    | null subjects = pure (Right (src, []))
    | otherwise = do
        files <- workDirFiles workDir
        step files (src, []) subjects
  where
    subjects = filter (not . producedElsewhere others . litText) (pathLits src)

    step _ acc [] = pure (Right acc)
    step files acc (l : rest)
        | isUrl (T.unpack (litText l)) =
            if readsLocalFile (litHead l)
                then pure (Left (urlRejection (litText l)))
                else step files acc rest
        | otherwise = do
            here <- doesPathExist (resolve workDir (litText l))
            if here
                then step files acc rest
                else case lookupPath (T.unpack (litText l)) files of
                    Unique right -> step files (repair l right acc) rest
                    Nearby cs -> Left <$> missingRejection workDir (litText l) cs
                    NoneNearby -> Left <$> missingRejection workDir (litText l) []

    repair l right (s, notes) =
        ( T.replace (litText l) ("./" <> T.pack right) s
        , notes <> [repairNote (litText l) right]
        )

resolve :: FilePath -> Text -> FilePath
resolve workDir raw
    | isAbsolute p = p
    | otherwise = workDir </> normalise p
  where
    p = T.unpack raw

producedElsewhere :: [Text] -> Text -> Bool
producedElsewhere others lit =
    or [lit `elem` map litText (literals o) | o <- others, writesFiles o]

repairNote :: Text -> FilePath -> Text
repairNote wrong right =
    "Corrected the path "
        <> tick wrong
        <> " to "
        <> tick ("./" <> T.pack right)
        <> " before committing; that is where the file actually is."

{- | A refusal describes the files it is offering instead. A failing turn is
exactly when the artefact's real shape is worth the bytes.
-}
missingRejection :: FilePath -> Text -> [FilePath] -> IO Value
missingRejection workDir wrong cs = do
    facts <- catMaybes <$> mapM (factsFor workDir . T.pack) cs
    pure $
        errorJsonWith
            message
            ( [ "verdict" .= ("path-not-found" :: Text)
              , "missingPath" .= wrong
              , "candidates" .= map T.pack cs
              ]
                <> ["candidateFacts" .= toJSON facts | not (null facts)]
            )
  where
    message = case cs of
        [] ->
            "This cell reads "
                <> tick wrong
                <> ", which does not exist and which nothing under the work \
                   \directory resembles, so nothing was committed. Call \
                   \list_files to see what is there, or ask the user for the \
                   \correct path."
        _ ->
            "This cell reads "
                <> tick wrong
                <> ", which does not exist, so nothing was committed. Nearest \
                   \files under the work directory: "
                <> T.intercalate ", " (map (tick . T.pack) cs)
                <> ". Use one of those, or call list_files to see the rest."

urlRejection :: Text -> Value
urlRejection wrong =
    errorJsonWith
        message
        [ "verdict" .= ("url-as-path" :: Text)
        , "missingPath" .= wrong
        ]
  where
    message =
        tick wrong
            <> " is a URL, and the file-reading functions resolve local paths \
               \only, so nothing was committed and nothing would have been \
               \fetched. Download the file into the work directory first, or \
               \read a path that exists."

tick :: Text -> Text
tick t = "`" <> t <> "`"
