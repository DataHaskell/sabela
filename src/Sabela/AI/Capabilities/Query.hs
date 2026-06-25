{-# LANGUAGE OverloadedStrings #-}

{- | Lightweight introspection tools: GHCi @:type@/@:info@/@:kind@/@:browse@/@:doc@,
the static API reference card, and 'explore_result' drill-down into
handles returned by other tools.
-}
module Sabela.AI.Capabilities.Query (
    execGhciQuery,
    execApiReference,
    execExploreResult,
    execPeekData,

    -- * Pieces
    runExplore,
    GhciOp (..),
    parseGhciOp,
    ExploreOp (..),
    parseExploreOp,
    parseHoleFits,
) where

import Control.Exception (try)
import Control.Exception.Base (IOException)
import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)
import System.FilePath (normalise, splitDirectories, (</>))

import Sabela.AI.Capabilities.Util (field, fieldInt, fieldText)
import Sabela.AI.Handles (
    HandleId (..),
    LargeResult (..),
    grepLines,
    headLines,
    lookupHandle,
    sliceLines,
    tailLines,
 )
import Sabela.AI.PeekData (peekData, peekResultJSON)
import Sabela.AI.ReferenceCard (sliceApiReference)
import Sabela.AI.Store
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (getHaskellSession)

{- | The GHCi introspection ops the schema declares an enum for.
Parsed once at the boundary so the dispatcher is total.
-}
data GhciOp = OpType | OpInfo | OpKind | OpBrowse | OpDoc | OpHoleFits | OpBindings
    deriving (Eq, Show)

parseGhciOp :: Text -> Maybe GhciOp
parseGhciOp "type" = Just OpType
parseGhciOp "info" = Just OpInfo
parseGhciOp "kind" = Just OpKind
parseGhciOp "browse" = Just OpBrowse
parseGhciOp "doc" = Just OpDoc
parseGhciOp "holefits" = Just OpHoleFits
parseGhciOp "bindings" = Just OpBindings
parseGhciOp _ = Nothing

-- | Every op needs an argument except bindings, which lists the whole session.
opNeedsArg :: GhciOp -> Bool
opNeedsArg OpBindings = False
opNeedsArg _ = True

execGhciQuery :: App -> Value -> IO ToolOutcome
execGhciQuery app input = do
    let rawOp = fieldText "op" input
        arg = fieldText "arg" input
    case parseGhciOp rawOp of
        Nothing ->
            pure
                ( errOutcome
                    ( errorJson
                        ( "Unknown op: "
                            <> rawOp
                            <> " (use type|info|kind|browse|doc|holefits|bindings)"
                        )
                    )
                )
        Just op
            | opNeedsArg op && T.null arg ->
                pure (errOutcome (errorJson "arg required"))
            | otherwise -> do
                mBackend <- getHaskellSession (appSessions app)
                case mBackend of
                    Nothing ->
                        pure
                            ( errOutcome
                                (errorJson "No live Haskell session — run a cell first to start GHCi.")
                            )
                    Just backend -> do
                        result <- runGhciOp backend op arg
                        pure $
                            okOutcome $
                                object ["op" .= rawOp, "arg" .= arg, "result" .= result]

runGhciOp :: SessionBackend -> GhciOp -> Text -> IO Text
runGhciOp backend op arg = case op of
    OpType -> sbQueryType backend arg
    OpInfo -> sbQueryInfo backend arg
    OpKind -> sbQueryKind backend arg
    OpBrowse -> sbQueryBrowse backend arg
    OpDoc -> sbQueryDoc backend arg
    OpHoleFits -> sbQueryHoleFits backend arg
    OpBindings -> sbQueryBindings backend

execApiReference :: Value -> IO ToolOutcome
execApiReference input = do
    let mName = case field "module" input of
            Just (String s) -> s
            _ -> ""
        body = sliceApiReference mName
    pure $ okOutcome $ object ["module" .= mName, "reference" .= body]

execExploreResult :: AIStore -> Value -> IO ToolOutcome
execExploreResult store input = do
    let hidText = fieldText "handle_id" input
        op = fieldText "op" input
    if T.null hidText
        then pure (errOutcome (errorJson "handle_id required"))
        else do
            mLr <- lookupHandle (aiHandles store) (HandleId hidText)
            case mLr of
                Nothing ->
                    pure
                        (errOutcome (errorJson ("Handle not found (may have expired): " <> hidText)))
                -- 'runExplore' returns 'errorJson' for an unknown op; propagate
                -- that as a typed tool error rather than masquerading as
                -- success.
                Just lr -> case parseExploreOp op of
                    Nothing ->
                        pure (errOutcome (runExplore op input lr))
                    Just _ ->
                        pure (okOutcome (runExplore op input lr))

{- | The four 'explore_result' ops the schema declares an enum for.
Parsed at the boundary so 'runExplore' is total.
-}
data ExploreOp = ExHead | ExTail | ExSlice | ExGrep
    deriving (Eq, Show)

parseExploreOp :: Text -> Maybe ExploreOp
parseExploreOp "head" = Just ExHead
parseExploreOp "tail" = Just ExTail
parseExploreOp "slice" = Just ExSlice
parseExploreOp "grep" = Just ExGrep
parseExploreOp _ = Nothing

runExplore :: Text -> Value -> LargeResult -> Value
runExplore rawOp input lr = case parseExploreOp rawOp of
    Nothing -> errorJson ("Unknown op: " <> rawOp <> " (use head|tail|slice|grep)")
    Just ExHead ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= headLines n lr, "totalLines" .= lrTotalLines lr]
    Just ExTail ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= tailLines n lr, "totalLines" .= lrTotalLines lr]
    Just ExSlice ->
        let from = fromMaybe 1 (fieldInt "from" input)
            to = fromMaybe (from + 20) (fieldInt "to" input)
         in object
                [ "lines" .= sliceLines from to lr
                , "from" .= from
                , "to" .= to
                , "totalLines" .= lrTotalLines lr
                ]
    Just ExGrep ->
        let pat = fieldText "pattern" input
            hits = [object ["line" .= i, "text" .= t] | (i, t) <- grepLines pat lr]
         in object ["hits" .= hits, "totalLines" .= lrTotalLines lr]

-- | Default number of data rows 'execPeekData' returns when @n@ is omitted.
defaultPeekRows :: Int
defaultPeekRows = 10

{- | Read a delimited file (path-checked to stay within the work dir) and
report its inferred delimiter, header, per-column type guesses, and first
@n@ rows — so the model can shape a dataframe load without guessing.
-}
execPeekData :: App -> Value -> IO ToolOutcome
execPeekData app input = do
    let relPath = T.unpack (fieldText "path" input)
        n = fromMaybe defaultPeekRows (fieldInt "n" input)
        workDir = envWorkDir (appEnv app)
    if null relPath
        then pure (errOutcome (errorJson "path required"))
        else do
            canon <- canonicalizePath (workDir </> relPath)
            if not (isWithinPath workDir canon)
                then pure (errOutcome (errorJson "Path escapes the work directory."))
                else do
                    eText <- try (TIO.readFile canon) :: IO (Either IOException Text)
                    case eText of
                        Left e ->
                            pure
                                ( errOutcome
                                    (errorJson ("Could not read file: " <> T.pack (show e)))
                                )
                        Right raw ->
                            pure $
                                okOutcome $
                                    object ["path" .= relPath, "peek" .= peekResultJSON (peekData n raw)]

{- | Parse GHC's @Valid hole fits include@ blob into @(name, type)@ pairs.

Pinned by a captured-real-blob fixture (GHC-version sensitive). Only the plain
valid fits are returned; the @Valid refinement hole fits include@ section is
dropped, as a refinement fit is a partial application with its own holes rather
than a name the model can drop in directly. Each entry is a @name :: type@ line
at the fit indent, with any wrapped type-continuation lines folded back in; the
@with@/@where@/@(imported …)@ provenance lines are discarded.
-}
parseHoleFits :: Text -> [(Text, Text)]
parseHoleFits blob = case dropToHeader (T.lines blob) of
    [] -> []
    fitLines -> collectFits (takeWhile (not . isRefinementHeader) fitLines)
  where
    dropToHeader = drop 1 . dropWhile (not . isValidHeader)
    isValidHeader = T.isInfixOf "Valid hole fits include"
    isRefinementHeader = T.isInfixOf "Valid refinement hole fits include"

{- | Group the fit-section lines into entries. An entry starts on the first
line carrying @::@ that is not a provenance line; subsequent deeper-indented
type-continuation lines are folded into its signature; provenance lines are
skipped.
-}
collectFits :: [Text] -> [(Text, Text)]
collectFits [] = []
collectFits (l : ls)
    | isEntryStart l =
        let (cont, rest) = span isTypeContinuation ls
            sig = T.unwords (map T.strip (l : cont))
         in maybe id (:) (splitNameType sig) (collectFits rest)
    | otherwise = collectFits ls
  where
    isEntryStart x = "::" `T.isInfixOf` x && not (isProvenance x)

-- | A wrapped type-signature line: indented, with no @::@ and not provenance.
isTypeContinuation :: Text -> Bool
isTypeContinuation x =
    not (T.null (T.strip x))
        && not ("::" `T.isInfixOf` x)
        && not (isProvenance x)

-- | The @with@/@where@/@(imported …)@/@(and …)@ lines GHC appends to a fit.
isProvenance :: Text -> Bool
isProvenance x =
    any (`T.isPrefixOf` T.strip x) ["with ", "where ", "(imported", "(and "]

-- | Split a @name :: type@ signature on its first @::@.
splitNameType :: Text -> Maybe (Text, Text)
splitNameType sig = case T.breakOn "::" sig of
    (name, rest)
        | not (T.null rest) ->
            let n = T.strip name
                t = T.strip (T.drop 2 rest)
             in if T.null n || T.null t then Nothing else Just (n, t)
    _ -> Nothing

-- | True when @child@ is @parent@ or nested under it (case-folded, normalised).
isWithinPath :: FilePath -> FilePath -> Bool
isWithinPath parent child =
    let p = splitDirectories (normalise parent)
        c = splitDirectories (normalise child)
     in p == take (length p) c
