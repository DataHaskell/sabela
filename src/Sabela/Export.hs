{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Export a notebook's dependency slice to a runnable, standalone Haskell
artifact: a single-file cabal script (@.hs@) or literate Haskell (@.lhs@).

The GHCi→module transform lives in "Sabela.Export.Block" (everything-in-@main@
from raw source); this module supplies the notebook context around it: the
backward slice from a target cell ("Sabela.Export.Analyze"), widget freezing,
prose/output comments, a stand-in prelude for the GHCi-injected display API, and
a type-aware resolver for trailing expressions (queries the live session so the
emitted file compiles).
-}
module Sabela.Export (
    exportCabalScript,
    exportLiterate,

    -- * Shared with the reactive exporter
    WidgetBind (..),
    parseWidgetBind,
    widgetDefault,
    exportPreludeDecls,
    mkTrailingResolver,

    -- * Pieces (exposed for testing)
    freezeWidgetSource,
    proseComment,
    splitArgs,
) where

import Control.Exception (SomeException, try)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (collectMetadata, mergedMeta)
import Sabela.Export.Analyze (
    backwardSlice,
    buildNotebookGraph,
    widgetConstructors,
 )
import Sabela.Export.Block (Hoisted (..), programActionExprs, splitProgram)
import Sabela.Model (Cell (..), CellType (..), Notebook (..), OutputItem (..))
import Sabela.Reactivity (cellPositionMap, haskellCodeCells)
import Sabela.SessionTypes (SessionBackend (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (getWidgetValues)
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Render (
    LhsBlock (..),
    TrailKind (..),
    TrailingResolver,
    renderCabalScriptHeader,
    renderLiterate,
 )

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Export the pipeline ending at @target@ as a single-file cabal script.
exportCabalScript :: App -> Int -> IO Text
exportCabalScript app target = do
    ex <- buildExport app target
    pure $
        assembleSections
            [ T.stripEnd (renderCabalScriptHeader (exMeta ex))
            , proseComment (exProse ex)
            , warningsComment (exWarnings ex)
            , T.stripEnd (exModule ex)
            , outputsComment (exOutputs ex)
            ]

-- | Export the pipeline ending at @target@ as literate Haskell.
exportLiterate :: App -> Int -> IO Text
exportLiterate app target = do
    ex <- buildExport app target
    let intro = T.intercalate "\n\n" (filter (not . T.null) (literateNote : exProse ex))
        blocks =
            [ LhsProse intro
            , LhsCode (T.lines (T.stripEnd (renderCabalScriptHeader (exMeta ex))))
            ]
                ++ [LhsProse (T.intercalate "\n" (exWarnings ex)) | not (null (exWarnings ex))]
                ++ [LhsCode (T.lines (T.stripEnd (exModule ex)))]
                ++ [ LhsProse ("**Recorded outputs:**\n\n" <> exOutputs ex)
                   | not (T.null (T.strip (exOutputs ex)))
                   ]
    pure (renderLiterate blocks <> "\n")
  where
    literateNote = "Exported from Sabela as a literate-Haskell pipeline."

-- ---------------------------------------------------------------------------
-- Building the export
-- ---------------------------------------------------------------------------

data Export = Export
    { exMeta :: CabalMeta
    , exModule :: Text
    , exProse :: [Text]
    , exOutputs :: Text
    , exWarnings :: [Text]
    }

buildExport :: App -> Int -> IO Export
buildExport app target = do
    nb <- readNotebook (appNotebook app)
    msession <- getHaskellSession (appSessions app)
    let ng = buildNotebookGraph nb
        allCode = haskellCodeCells nb
        sliceIds = S.fromList (map cellId (backwardSlice target ng))
        -- TH splices generate names cellNames can't see, so the dependency
        -- graph can't slice on them — always keep any cell with a splice.
        slice =
            filter
                (\c -> cellId c `S.member` sliceIds || "$(" `T.isInfixOf` cellSource c)
                allCode
        posMap = cellPositionMap nb
    frozen <- mapM (freezeCell app) slice
    resolver <- mkTrailingResolver msession (programActionExprs frozen)
    let (hoisted, doStmts) = splitProgram resolver S.empty frozen
        -- Imports/pragmas come from the *whole* notebook so a slice never
        -- misses an import authored in an excluded cell (imports create no
        -- dependency edges). Over-inclusion only yields unused-import warnings,
        -- which the cabal header suppresses.
        (importsH, _) = splitProgram (const TrailUnknown) S.empty (map cellSource allCode)
    pure
        Export
            { exMeta = mergedMeta (envGlobalDeps (appEnv app)) (collectMetadata nb)
            , exModule = assembleModule importsH hoisted (exportPreludeDecls slice) doStmts
            , exProse = sliceProse nb posMap slice
            , exOutputs = outputsText posMap slice
            , exWarnings = buildWarnings nb slice hoisted doStmts
            }

freezeCell :: App -> Cell -> IO Text
freezeCell app c = do
    vals <- getWidgetValues (appWidgets app) (cellId c)
    pure (freezeWidgetSource vals (cellSource c))

{- | Assemble the module: hoisted pragmas, @module Main where@, notebook-wide
imports, the display stand-in prelude, top-level declarations (data/class/
instance/TH), and a generated @main@ holding every statement in document order
— so value bindings see the @\<-@ binds they depend on.
-}
assembleModule :: Hoisted -> Hoisted -> [Text] -> [Text] -> Text
assembleModule importsH hoisted prelude doStmts =
    T.unlines . intercalate [""] . filter (not . null) $
        [ dedupT (hPragmas importsH)
        , ["module Main where"]
        , dedupT (hImports importsH)
        , prelude
        , hTopDecls hoisted
        , mainBlock
        ]
  where
    mainBlock = case doStmts of
        [] -> ["main :: IO ()", "main = pure ()"]
        _ ->
            "main :: IO ()"
                : "main = do"
                : concatMap (map ("    " <>) . T.lines) (doStmts ++ ["pure ()"])

buildWarnings :: Notebook -> [Cell] -> Hoisted -> [Text] -> [Text]
buildWarnings nb slice hoisted doStmts =
    concat
        [ [ "-- [sabela:export] "
                <> tShow npy
                <> " Python cell(s) omitted (cross-language pipeline not supported)."
          | npy > 0
          ]
        , [ "-- [sabela:export] target is not a Haskell code cell; nothing to export."
          | null slice
          ]
        , [thNote | hasTH && hasBind]
        ]
  where
    npy = length [c | c <- nbCells nb, cellType c == CodeCell, cellLang c == ST.Python]
    hasTH = any (T.isInfixOf "$(") (hTopDecls hoisted)
    hasBind = any (T.isInfixOf "<-") doStmts
    thNote =
        T.intercalate
            "\n"
            [ "-- [sabela:export] NOTE: uses Template Haskell splices that may depend on"
            , "-- values bound at runtime (e.g. `df <- ...`). Top-level splices cannot see"
            , "-- main-local bindings, so this may need manual lifting to compile."
            ]

dedupT :: [Text] -> [Text]
dedupT = go []
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs

-- ---------------------------------------------------------------------------
-- Type-aware trailing-expression resolution
-- ---------------------------------------------------------------------------

{- | Pre-resolve every trailing expression against the live session, producing
a pure resolver for 'toModule'. With no session, everything is 'TrailUnknown'
(commented out) — the module still compiles. Classification uses only
"does @:type@ succeed" probes on the *exact* expression we would emit, so a
stale session degrades safely rather than miscompiling.
-}
mkTrailingResolver :: Maybe SessionBackend -> [Text] -> IO TrailingResolver
mkTrailingResolver Nothing _ = pure (const TrailUnknown)
mkTrailingResolver (Just sb) exprs = do
    pairs <- mapM (\e -> (,) e <$> resolveOne sb (flatten e)) exprs
    let m = M.fromList pairs
    pure (\e -> M.findWithDefault TrailUnknown e m)
  where
    flatten = T.unwords . filter (not . T.null) . map T.strip . T.lines

resolveOne :: SessionBackend -> Text -> IO TrailKind
resolveOne sb e = do
    -- All probes use only Prelude operators (always in scope in GHCi) on the
    -- exact expression we'd emit, so success means it really compiles.
    ioUnit <- typeChecks sb ("(" <> e <> ") :: IO ()")
    if ioUnit
        then pure TrailIOUnit
        else do
            ioShow <- typeChecks sb ("print =<< (" <> e <> ")")
            if ioShow
                then pure TrailIOShow
                else do
                    isIO <- typeChecks sb ("(" <> e <> ") >> return ()")
                    if isIO
                        then pure TrailIOUnit
                        else do
                            pureShow <- typeChecks sb ("print (" <> e <> ")")
                            pure (if pureShow then TrailPure else TrailUnknown)

-- | Does @:type expr@ succeed (expression is well-typed in the session)?
typeChecks :: SessionBackend -> Text -> IO Bool
typeChecks sb expr = do
    r <- try (sbQueryType sb expr) :: IO (Either SomeException Text)
    pure $ case r of
        Left _ -> False
        Right t -> not (isTypeError t)

isTypeError :: Text -> Bool
isTypeError t =
    let lc = T.toLower t
     in T.null (T.strip t)
            || any
                (`T.isInfixOf` lc)
                [ "not in scope"
                , "error:"
                , "parse error"
                , "no instance"
                , "cannot "
                , "ambiguous"
                , "couldn't match"
                , "could not"
                ]

-- ---------------------------------------------------------------------------
-- Widget freezing
-- ---------------------------------------------------------------------------

{- | Rewrite simple widget binds — @x <- display (slider "name" def lo hi)@ and
friends — into plain bindings @x = <value>@, using the current 'WidgetStore'
value if present, otherwise the constructor's default argument. Composed or
unrecognized widget expressions are left unchanged (a known limitation).
-}
freezeWidgetSource :: M.Map Text Text -> Text -> Text
freezeWidgetSource vals = T.intercalate "\n" . map freezeLine . T.lines
  where
    freezeLine line = fromMaybe line (tryFreeze vals line)

-- | A parsed widget bind: @binder \<- display (ctor "name" args…)@.
data WidgetBind = WidgetBind
    { wbBinder :: Text
    , wbCtor :: Text
    , wbName :: Text
    , wbArgs :: [Text]
    }
    deriving (Show, Eq)

{- | Parse a single-line widget bind. Recognizes
@x \<- display (slider "n" def lo hi)@ and the other widget constructors,
with or without the @display@ wrapper.
-}
parseWidgetBind :: Text -> Maybe WidgetBind
parseWidgetBind line = do
    let (lhs, arrowRhs) = T.breakOn "<-" line
    rhs0 <- if T.null arrowRhs then Nothing else Just (T.drop 2 arrowRhs)
    let binder = T.strip lhs
    if not (isSimpleIdent binder)
        then Nothing
        else do
            (ctor, args) <- parseCtor (stripDisplay (T.strip rhs0))
            name <- argName args (if ctor == "button" then 1 else 0)
            pure WidgetBind{wbBinder = binder, wbCtor = ctor, wbName = name, wbArgs = args}

-- | The constructor's default-value argument, as Haskell source.
widgetDefault :: WidgetBind -> Maybe Text
widgetDefault wb = case wbCtor wb of
    "slider" -> argAt (wbArgs wb) 1
    "checkbox" -> argAt (wbArgs wb) 1
    "dropdown" -> argAt (wbArgs wb) 2
    "textInput" -> argAt (wbArgs wb) 1
    "button" -> Just "Nothing"
    _ -> Nothing

tryFreeze :: M.Map Text Text -> Text -> Maybe Text
tryFreeze vals line = do
    wb <- parseWidgetBind line
    frozen <- freezeValue vals (wbCtor wb) (wbArgs wb)
    let indent = T.takeWhile (== ' ') line
    pure (indent <> wbBinder wb <> " = " <> frozen)

-- | Strip a leading @display@ / @display $@ / @display ( … )@ wrapper.
stripDisplay :: Text -> Text
stripDisplay t0 =
    let t = T.strip t0
     in case T.stripPrefix "display" t of
            Just r ->
                let r' = T.stripStart r
                 in case T.stripPrefix "$" r' of
                        Just r2 -> T.stripStart r2
                        Nothing -> stripOuterParens r'
            Nothing -> t

stripOuterParens :: Text -> Text
stripOuterParens t =
    case (T.stripPrefix "(" t, T.stripSuffix ")" t) of
        (Just _, Just _) ->
            let inner = T.dropEnd 1 (T.drop 1 t)
             in if parenBalanced inner then T.strip inner else t
        _ -> t

parenBalanced :: Text -> Bool
parenBalanced = go (0 :: Int) . T.unpack
  where
    go d [] = d == 0
    go d (c : cs)
        | c `elem` ("([" :: String) = go (d + 1) cs
        | c `elem` (")]" :: String) = d > 0 && go (d - 1) cs
        | otherwise = go d cs

-- | A widget constructor application: @ctor arg1 arg2 …@.
parseCtor :: Text -> Maybe (Text, [Text])
parseCtor t =
    let t' = T.stripStart t
        (ctor, rest) = T.span isIdentChar t'
     in if S.member ctor widgetConstructors
            then Just (ctor, splitArgs (T.stripStart rest))
            else Nothing

freezeValue :: M.Map Text Text -> Text -> [Text] -> Maybe Text
freezeValue vals ctor args = case ctor of
    "slider" -> do
        name <- argName args 0
        let def = argAt args 1
        pure $ case lookupVal name of
            Just v -> annotate v (typeAnnOf def)
            Nothing -> fromMaybe "0" def
    "checkbox" -> do
        name <- argName args 0
        let def = argAt args 1
        pure $ case lookupVal name of
            Just "true" -> "True"
            Just "false" -> "False"
            _ -> fromMaybe "False" def
    "dropdown" -> do
        name <- argName args 0
        let def = argAt args 2
        pure $ case lookupVal name of
            Just v -> tShow v
            Nothing -> fromMaybe "\"\"" def
    "textInput" -> do
        name <- argName args 0
        let def = argAt args 1
        pure $ case lookupVal name of
            Just v -> tShow v
            Nothing -> fromMaybe "\"\"" def
    "button" -> do
        name <- argName args 1
        pure $ case lookupVal name of
            Just "clicked" -> "Just ()"
            _ -> "Nothing"
    _ -> Nothing
  where
    lookupVal name = case M.lookup name vals of
        Just v | not (T.null (T.strip v)) -> Just (T.strip v)
        _ -> Nothing

-- | The i-th argument of a constructor application, if present.
argAt :: [Text] -> Int -> Maybe Text
argAt args i = if i >= 0 && i < length args then Just (args !! i) else Nothing

-- | The i-th argument, interpreted as a string literal, with quotes stripped.
argName :: [Text] -> Int -> Maybe Text
argName args i = argAt args i >>= (T.stripPrefix "\"" Control.Monad.>=> T.stripSuffix "\"")

-- | Extract the type from a @( v :: T )@ annotation, if any.
typeAnnOf :: Maybe Text -> Maybe Text
typeAnnOf Nothing = Nothing
typeAnnOf (Just d) =
    let (_, r) = T.breakOn "::" d
     in if T.null r
            then Nothing
            else
                let ty = T.strip (T.dropWhileEnd (== ')') (T.strip (T.drop 2 r)))
                 in if T.null ty then Nothing else Just ty

annotate :: Text -> Maybe Text -> Text
annotate v Nothing = v
annotate v (Just ty) = "(" <> v <> " :: " <> ty <> ")"

{- | Split a Haskell application's arguments on top-level whitespace, treating
@(…)@, @[…]@, and string literals as atomic.
-}
splitArgs :: Text -> [Text]
splitArgs = map T.pack . goTop . T.unpack
  where
    goTop s = case dropWhile (== ' ') s of
        [] -> []
        s' -> let (arg, rest) = takeArg 0 False [] s' in reverse arg : goTop rest

    takeArg :: Int -> Bool -> String -> String -> (String, String)
    takeArg _ _ acc [] = (acc, [])
    takeArg d True acc ('\\' : n : cs) = takeArg d True (n : '\\' : acc) cs
    takeArg d True acc ('"' : cs) = takeArg d False ('"' : acc) cs
    takeArg d True acc (c : cs) = takeArg d True (c : acc) cs
    takeArg d _ acc ('"' : cs) = takeArg d True ('"' : acc) cs
    takeArg d _ acc (c : cs)
        | c `elem` ("([" :: String) = takeArg (d + 1) False (c : acc) cs
        | c `elem` (")]" :: String) = takeArg (max 0 (d - 1)) False (c : acc) cs
        | c == ' ' && d == 0 = (acc, cs)
        | otherwise = takeArg d False (c : acc) cs

-- ---------------------------------------------------------------------------
-- Stand-in prelude for the GHCi-injected display API
-- ---------------------------------------------------------------------------

{- | Definitions for the @display*@ functions the notebook relies on (injected
into GHCi by 'Sabela.Output.displayPrelude', absent from a standalone module).
Only the ones referenced by the slice are emitted, so there are no unused
bindings. Each prints to stdout — the closest standalone behaviour.
-}
exportPreludeDecls :: [Cell] -> [Text]
exportPreludeDecls slice =
    let src = T.concat (map cellSource slice)
        wanted = [def | (fn, def) <- preludeDefs, fn `T.isInfixOf` src]
     in [preludeHeader <> T.intercalate "\n\n" wanted | not (null wanted)]
  where
    preludeHeader = "-- [sabela:export] standalone stand-ins for the notebook display API\n"
    preludeDefs =
        [ ("displayHtml", sig "displayHtml" <> "displayHtml = putStrLn")
        , ("displayMarkdown", sig "displayMarkdown" <> "displayMarkdown = putStrLn")
        , ("displaySvg", sig "displaySvg" <> "displaySvg = putStrLn")
        , ("displayLatex", sig "displayLatex" <> "displayLatex = putStrLn")
        , ("displayJson", sig "displayJson" <> "displayJson = putStrLn")
        ,
            ( "displayImage"
            , "displayImage :: String -> String -> IO ()\ndisplayImage _ b64 = putStrLn b64"
            )
        ]
    sig n = n <> " :: String -> IO ()\n"

-- ---------------------------------------------------------------------------
-- Prose, outputs, and comment helpers
-- ---------------------------------------------------------------------------

-- | Non-empty prose cells positioned at or before the last slice cell.
sliceProse :: Notebook -> M.Map Int Int -> [Cell] -> [Text]
sliceProse nb posMap slice =
    let maxPos = maximum (1 : mapMaybe (\c -> M.lookup (cellId c) posMap) slice)
     in [ T.strip (cellSource c)
        | c <- nbCells nb
        , cellType c == ProseCell
        , not (T.null (T.strip (cellSource c)))
        , maybe False (<= maxPos) (M.lookup (cellId c) posMap)
        ]

-- | Recorded outputs of the slice cells, keyed by 1-based position.
outputsText :: M.Map Int Int -> [Cell] -> Text
outputsText posMap slice =
    T.intercalate "\n\n" $
        [ "cell "
            <> tShow (M.findWithDefault (cellId c) (cellId c) posMap)
            <> ":\n"
            <> body
        | c <- slice
        , let body =
                T.intercalate
                    "\n"
                    [ T.stripEnd (oiOutput o)
                    | o <- cellOutputs c
                    , not (T.null (T.strip (oiOutput o)))
                    ]
        , not (T.null body)
        ]

proseComment :: [Text] -> Text
proseComment [] = ""
proseComment chunks =
    "{- \n" <> sanitizeComment (T.intercalate "\n\n" chunks) <> "\n-}"

outputsComment :: Text -> Text
outputsComment t
    | T.null (T.strip t) = ""
    | otherwise = "{- Recorded notebook outputs:\n\n" <> sanitizeComment t <> "\n-}"

warningsComment :: [Text] -> Text
warningsComment [] = ""
warningsComment ws = T.intercalate "\n" ws

-- | Neutralize comment delimiters so embedded text can't break out of a block.
sanitizeComment :: Text -> Text
sanitizeComment = T.replace "-}" "- }" . T.replace "{-" "{ -"

assembleSections :: [Text] -> Text
assembleSections =
    (<> "\n") . T.intercalate "\n\n" . filter (not . T.null) . map T.stripEnd

-- ---------------------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------------------

tShow :: (Show a) => a -> Text
tShow = T.pack . show

isSimpleIdent :: Text -> Bool
isSimpleIdent t = case T.uncons t of
    Just (c, rest) -> isLowerStart c && T.all isIdentChar rest
    Nothing -> False
  where
    isLowerStart c = c == '_' || isAsciiLower c

isIdentChar :: Char -> Bool
isIdentChar c =
    isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c == '_'
        || c == '\''
