{-# LANGUAGE OverloadedStrings #-}

module Sabela.Errors.Json (
    DiagSpan (..),
    diagnosticSpans,
    parseJsonInteractive,
    parseJsonCompiled,
    annotateDefSites,
    quotedNames,
) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:), (.:?))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.Errors (scrubHarnessFrames)
import Sabela.Model (CellError (..))
import ScriptHs.Compiled (parseLinePragmaTag)

data Diag = Diag
    { dSeverity :: Text
    , dCellId :: Maybe Int
    , dError :: CellError
    , dSpan :: Maybe Span
    }

isWarning :: Diag -> Bool
isWarning d = dSeverity d == "Warning"

data DiagSpan = DiagSpan
    { dsMessage :: Text
    , dsStart :: (Int, Int)
    , dsEnd :: (Int, Int)
    }
    deriving (Eq, Show)

diagnosticSpans :: Text -> [DiagSpan]
diagnosticSpans raw =
    [ DiagSpan (ceMessage (dError d)) (spLine s, spCol s) (spEndLine s, spEndCol s)
    | d <- fst (decodeLines raw)
    , not (isWarning d)
    , Just s <- [dSpan d]
    ]

parseJsonInteractive :: Text -> ([CellError], [CellError], Text)
parseJsonInteractive raw =
    let (diags, residual) = decodeLines raw
        (warns, errs) = span' isWarning diags
     in (map dError errs, map dError warns, residual)
  where
    span' p ds = (filter p ds, filter (not . p) ds)

parseJsonCompiled :: Text -> (M.Map Int [CellError], [CellError])
parseJsonCompiled raw =
    foldr route (M.empty, []) [d | d <- fst (decodeLines raw), not (isWarning d)]
  where
    route d (m, loose) = case dCellId d of
        Just cid -> (M.insertWith (++) cid [dError d] m, loose)
        Nothing -> (m, dError d : loose)

decodeLines :: Text -> ([Diag], Text)
decodeLines raw = (diags, T.unlines residual)
  where
    (diags, residual) = foldr step ([], []) (T.lines raw)
    step line (ds, rs) = case decodeDiag line of
        Just d -> (d : ds, rs)
        Nothing | T.null (T.strip line) -> (ds, rs)
        Nothing -> (ds, line : rs)

decodeDiag :: Text -> Maybe Diag
decodeDiag line =
    case eitherDecodeStrict' (TE.encodeUtf8 line) of
        Right d -> Just d
        Left _ -> Nothing

instance FromJSON Diag where
    parseJSON = withObject "diagnostic" $ \o -> do
        sev <- o .: "severity"
        mspan <- o .:? "span"
        msgs <- o .: "message"
        hints <- fromMaybe [] <$> o .:? "hints"
        code <- o .:? "code"
        pure
            Diag
                { dSeverity = sev
                , dCellId = mspan >>= (parseLinePragmaTag . spFile)
                , dError =
                    CellError
                        (spLine <$> mspan)
                        (spCol <$> mspan)
                        (renderMessage msgs hints)
                        code
                , dSpan = mspan
                }

renderMessage :: [Text] -> [Text] -> Text
renderMessage msgs hints =
    T.intercalate "\n" (filter (not . T.null . T.strip) (scrubbed ++ hints))
  where
    scrubbed = map scrubHarnessFrames msgs

annotateDefSites :: (Text -> Maybe Int) -> Text -> Text
annotateDefSites resolve = T.intercalate "\n" . map annotateLine . T.lines
  where
    annotateLine l
        | "Perhaps" `T.isInfixOf` l, found@(_ : _) <- resolved l = l <> suffix found
        | otherwise = l
    resolved l = [(n, c) | n <- quotedNames l, Just c <- [resolve n]]
    suffix [(_, c)] = " (defined in cell " <> tShow c <> ")"
    suffix found =
        " ("
            <> T.intercalate ", " ["`" <> n <> "' in cell " <> tShow c | (n, c) <- found]
            <> ")"
    tShow = T.pack . show

quotedNames :: Text -> [Text]
quotedNames t = between '`' '\'' t ++ between '\8216' '\8217' t
  where
    between open close = go
      where
        go s = case T.breakOn (T.singleton open) s of
            (_, r)
                | not (T.null r) ->
                    case T.breakOn (T.singleton close) (T.drop 1 r) of
                        (tok, after)
                            | not (T.null after) -> tok : go (T.drop 1 after)
                        _ -> []
            _ -> []

data Span = Span
    { spFile :: Text
    , spLine :: Int
    , spCol :: Int
    , spEndLine :: Int
    , spEndCol :: Int
    }

instance FromJSON Span where
    parseJSON = withObject "span" $ \o -> do
        file <- o .: "file"
        Pos ln col <- o .: "start"
        Pos el ec <- fromMaybe (Pos ln col) <$> o .:? "end"
        pure (Span file ln col el ec)

data Pos = Pos Int Int

instance FromJSON Pos where
    parseJSON = withObject "position" $ \o ->
        Pos <$> o .: "line" <*> o .: "column"
