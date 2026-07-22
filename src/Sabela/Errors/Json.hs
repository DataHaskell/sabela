{-# LANGUAGE OverloadedStrings #-}

{- | Parse GHC's @-fdiagnostics-as-json@ output (NDJSON: one diagnostic object
per stderr line) into 'CellError's, split by severity.

Each diagnostic carries a structured @span@ (file + start line\/column) and a
@severity@, so lines and the originating cell come back exact rather than
scraped from human-readable text ("Sabela.Errors" is the pre-9.8 fallback). The
@span.file@ is the @sabela-cell-N@ LINE-pragma tag the renderers emit
('ScriptHs.Compiled.linePragmaTag'), so compiled diagnostics route per cell the
same way the textual 'Sabela.Errors.parseCompiledErrors' did.

Lines that are not JSON diagnostics (runtime stderr, linker\/TH noise) are
returned verbatim as residual text, so the runtime-error path still sees them.
-}
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

import Sabela.Model (CellError (..))
import ScriptHs.Compiled (parseLinePragmaTag)

-- | A decoded GHC diagnostic, reduced to what Sabela renders and routes.
data Diag = Diag
    { dSeverity :: Text
    , dCellId :: Maybe Int
    , dError :: CellError
    , dSpan :: Maybe Span
    }

isWarning :: Diag -> Bool
isWarning d = dSeverity d == "Warning"

-- | An error diagnostic's message with its full source span (start, end).
data DiagSpan = DiagSpan
    { dsMessage :: Text
    , dsStart :: (Int, Int)
    , dsEnd :: (Int, Int)
    }
    deriving (Eq, Show)

{- | Error diagnostics with their full source span, for the typed-hole engine to
hole the offending subterm by range. Warnings and spanless diagnostics are dropped
(nothing to hole).
-}
diagnosticSpans :: Text -> [DiagSpan]
diagnosticSpans raw =
    [ DiagSpan (ceMessage (dError d)) (spLine s, spCol s) (spEndLine s, spEndCol s)
    | d <- fst (decodeLines raw)
    , not (isWarning d)
    , Just s <- [dSpan d]
    ]

{- | Diagnostics for an interpreted cell: @(errors, warnings, residual)@. The
cell is already known to the caller, so the span tag is ignored here; residual
is every non-JSON stderr line, joined, for the runtime-error path.
-}
parseJsonInteractive :: Text -> ([CellError], [CellError], Text)
parseJsonInteractive raw =
    let (diags, residual) = decodeLines raw
        (warns, errs) = span' isWarning diags
     in (map dError errs, map dError warns, residual)
  where
    span' p ds = (filter p ds, filter (not . p) ds)

{- | Error diagnostics for a compiled @:load@, routed per cell by the span tag,
matching 'Sabela.Errors.parseCompiledErrors': @(perCell, loose)@. Tagless
errors are loose; warnings never count as a compile failure, so they are
dropped.
-}
parseJsonCompiled :: Text -> (M.Map Int [CellError], [CellError])
parseJsonCompiled raw =
    foldr route (M.empty, []) [d | d <- fst (decodeLines raw), not (isWarning d)]
  where
    route d (m, loose) = case dCellId d of
        Just cid -> (M.insertWith (++) cid [dError d] m, loose)
        Nothing -> (m, dError d : loose)

-- | Decode each line; JSON diagnostics on one side, untouched lines on the other.
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

-- | GHC paragraphs then hints, one per line.
renderMessage :: [Text] -> [Text] -> Text
renderMessage msgs hints =
    T.intercalate "\n" (filter (not . T.null . T.strip) (msgs ++ hints))

{- | Annotate each @Perhaps use `name'@ suggestion with the cell that defines
@name@, so the reader can find it. GHC's hint gives a bare @(line N)@ that names
the line but not the cell (it abbreviates the synthetic per-cell file tag away),
so without this a "use `triple'" hint can't be followed. @resolve@ maps a name
to its defining cell (from notebook defs); unresolved names are left untouched.
-}
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

-- | Identifiers GHC quoted in a hint line, in both @`name'@ and @‘name’@ forms.
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

-- | The bits of a diagnostic @span@ Sabela uses (start + end position + file tag).
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

-- | A diagnostic span's @{line, column}@ endpoint.
data Pos = Pos Int Int

instance FromJSON Pos where
    parseJSON = withObject "position" $ \o ->
        Pos <$> o .: "line" <*> o .: "column"
