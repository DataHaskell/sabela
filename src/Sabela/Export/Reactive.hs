{-# LANGUAGE OverloadedStrings #-}

{- | Export a notebook's widget→output dataflow as a runnable, headless
@reactive-banana@ program.

The notebook's reactive model (a widget edit recomputes everything downstream)
is exactly an FRP push network. Rather than lifting every binding into its own
'Behavior', we generate a single @render@ function that takes the widget values
as parameters and contains the reactive cells' code as a @do@-block (built by
"Sabela.Export.Block"), then lift it over the widget source behaviors as one
@Behavior (IO ())@ and @reactimate@ it. Widget binds become parameters;
everything else copies through.

The host is headless: widgets are driven from stdin (lines @name value@) and
one-shot @--name=value@ flags; output goes to stdout. The exported program
carries its own deps in a @{\- cabal: -\}@ header, so Sabela's server never
depends on @reactive-banana@.
-}
module Sabela.Export.Reactive (
    exportReactive,
) where

import Data.Char (toUpper)
import Data.List (intercalate, nubBy)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (collectMetadata, mergedMeta)
import Sabela.Export (
    WidgetBind (..),
    exportPreludeDecls,
    mkTrailingResolver,
    parseWidgetBind,
    widgetDefault,
 )
import Sabela.Export.Analyze (NotebookGraph (..), buildNotebookGraph)
import Sabela.Export.Block (Hoisted (..), programActionExprs, splitProgram)
import Sabela.Model (Cell (..))
import Sabela.Reactivity (haskellCodeCells)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Render (TrailKind (..), renderCabalScriptHeader)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

{- | Export the whole notebook as a headless reactive-banana program. (The
target argument is accepted for endpoint symmetry but unused: the reactive
export always covers every widget and its downstream.)
-}
exportReactive :: App -> Int -> IO Text
exportReactive app _target = do
    nb <- readNotebook (appNotebook app)
    msession <- getHaskellSession (appSessions app)
    let ng = buildNotebookGraph nb
        allCode = haskellCodeCells nb
        reactiveIds = ngReactiveSet ng
        redefIds = ngRedefIds ng
        reactiveCells = filter ((`S.member` reactiveIds) . cellId) allCode
        staticCells =
            filter
                ( \c -> not (S.member (cellId c) reactiveIds) && not (S.member (cellId c) redefIds)
                )
                allCode
        widgets = map mkWInfo (collectWidgets reactiveCells)
        binders = S.fromList (map (wbBinder . wiBind) widgets)
    resolver <-
        mkTrailingResolver
            msession
            (programActionExprs (map cellSource (reactiveCells ++ staticCells)))
    let renderBody = concatMap (reactiveCellStmts resolver binders) reactiveCells
        (_, staticStmts) = splitProgram resolver S.empty (map cellSource staticCells)
        (allH, _) = splitProgram (const TrailUnknown) binders (map cellSource allCode)
        meta = reactiveMeta (mergedMeta (envGlobalDeps (appEnv app)) (collectMetadata nb))
        prelude = exportPreludeDecls (reactiveCells ++ staticCells)
    pure (assemble meta allH prelude widgets renderBody staticStmts)

-- ---------------------------------------------------------------------------
-- Reactive cell bodies
-- ---------------------------------------------------------------------------

{- | The render-do-block statements contributed by one reactive cell. Widget
binds are dropped (their binders become @render@ parameters); a cell with an
unsupported widget (e.g. a composed @liftA2 (slider…) (slider…)@) is emitted
commented-out so the program still compiles.
-}
reactiveCellStmts :: (Text -> TrailKind) -> S.Set Text -> Cell -> [Text]
reactiveCellStmts resolve binders c
    | cellHasUnsupportedWidget c =
        "-- [sabela:export] cell not auto-translated to FRP (composed/unsupported widget):"
            : map ("-- " <>) (T.lines (cellSource c))
    | otherwise = snd (splitProgram resolve binders [cellSource c])

-- ---------------------------------------------------------------------------
-- Widgets
-- ---------------------------------------------------------------------------

data FeedMode = ReadMode | RawMode | UnitMode

data WInfo = WInfo
    { wiBind :: WidgetBind
    , wiBehVar :: Text
    , wiAhVar :: Text
    , wiFireVar :: Text
    , wiSourceCall :: Text
    , wiFeedMode :: FeedMode
    }

collectWidgets :: [Cell] -> [WidgetBind]
collectWidgets cells =
    nubBy (\a b -> wbBinder a == wbBinder b) $
        [wb | c <- cells, ln <- T.lines (cellSource c), Just wb <- [parseWidgetBind ln]]

mkWInfo :: WidgetBind -> WInfo
mkWInfo wb =
    WInfo
        { wiBind = wb
        , wiBehVar = b <> "B"
        , wiAhVar = "ah" <> cb
        , wiFireVar = "fire" <> cb
        , wiSourceCall = call
        , wiFeedMode = mode
        }
  where
    b = wbBinder wb
    cb = capitalize b
    def = fromMaybe "()" (widgetDefault wb)
    (call, mode) = case wbCtor wb of
        "slider" -> ("sliderSource " <> def, ReadMode)
        "checkbox" -> ("boolSource " <> def, ReadMode)
        "dropdown" -> ("stringSource " <> def, RawMode)
        "textInput" -> ("stringSource " <> def, RawMode)
        "button" -> ("buttonSource", UnitMode)
        _ -> ("sliderSource " <> def, ReadMode)

cellHasUnsupportedWidget :: Cell -> Bool
cellHasUnsupportedWidget c = any bad (T.lines (cellSource c))
  where
    bad ln = mentionsWidgetCtor ln && isNothing (parseWidgetBind ln)

mentionsWidgetCtor :: Text -> Bool
mentionsWidgetCtor t =
    any
        (`T.isInfixOf` t)
        ["slider ", "dropdown ", "checkbox ", "textInput ", "button "]

-- ---------------------------------------------------------------------------
-- Program assembly
-- ---------------------------------------------------------------------------

assemble ::
    CabalMeta -> Hoisted -> [Text] -> [WInfo] -> [Text] -> [Text] -> Text
assemble meta allH prelude widgets renderBody staticStmts =
    T.unlines . intercalateBlank $
        filter
            (not . null)
            [ [T.stripEnd (renderCabalScriptHeader meta)]
            , dedup (hPragmas allH)
            , ["module Main where"]
            , fixedImports ++ dedup (filter (`notElem` fixedImports) (hImports allH))
            , runtimePrelude
            , prelude
            , dedup (hTopDecls allH)
            , renderFn
            , mainFn
            ]
  where
    hasWidgets = not (null widgets)

    renderFn
        | not hasWidgets = []
        | otherwise =
            let params = T.intercalate " " (map (wbBinder . wiBind) widgets)
             in ("render " <> params <> " = do")
                    : map ("    " <>) (concatMap T.lines (renderBody ++ ["pure ()"]))

    mainFn
        | not hasWidgets =
            mainHeader
                ++ ["    putStrLn \"[sabela:export] no widgets found; ran the pipeline once.\""]
        | otherwise =
            mainHeader
                ++ map ("    " <>) handlerDecls
                ++ ["    network <- compile $ do"]
                ++ map ("        " <>) sourceDecls
                ++ ["        reactimateSink (" <> liftExpr <> ")"]
                ++ ["    actuate network"]
                ++ feedBlock
                ++ loopBlock

    mainHeader =
        ["main :: IO ()", "main = do", "    hSetBuffering stdout LineBuffering"]
            ++ map ("    " <>) (concatMap T.lines staticStmts)

    handlerDecls =
        ["(" <> wiAhVar w <> ", " <> wiFireVar w <> ") <- newAddHandler" | w <- widgets]
    sourceDecls =
        [wiBehVar w <> " <- " <> wiSourceCall w <> " " <> wiAhVar w | w <- widgets]
    liftExpr = case map wiBehVar widgets of
        [] -> "pure render"
        (b : bs) -> T.unwords (["render", "<$>", b] ++ concatMap (\x -> ["<*>", x]) bs)

    feedBlock =
        ["    let feed name val = case name of"]
            ++ ["          " <> feedClause w | w <- widgets]
            ++ ["          _ -> pure ()"]

    feedClause w =
        "\"" <> wbName (wiBind w) <> "\" -> " <> case wiFeedMode w of
            ReadMode -> "maybe (pure ()) " <> wiFireVar w <> " (readMaybe val)"
            RawMode -> wiFireVar w <> " val"
            UnitMode -> wiFireVar w <> " ()"

    loopBlock =
        [ "    args <- getArgs"
        , "    forM_ args $ \\a -> case break (== '=') (dropWhile (== '-') a) of"
        , "      (n, '=':v) -> feed n v"
        , "      _ -> pure ()"
        , "    let loop = do"
        , "          eof <- isEOF"
        , "          if eof then pure () else do"
        , "            l <- getLine"
        , "            case words l of"
        , "              (n:rest) -> feed n (unwords rest)"
        , "              [] -> pure ()"
        , "            loop"
        , "    loop"
        ]

fixedImports :: [Text]
fixedImports =
    [ "import Reactive.Banana"
    , "import Reactive.Banana.Frameworks"
    , "import Control.Monad (forM_)"
    , "import Control.Monad.IO.Class (liftIO)"
    , "import System.Environment (getArgs)"
    , "import System.IO (BufferMode (..), hSetBuffering, isEOF, stdout)"
    , "import Text.Read (readMaybe)"
    ]

-- | Inlined FRP runtime helpers so the exported program is self-contained.
runtimePrelude :: [Text]
runtimePrelude =
    [ "-- [sabela:export] reactive-banana runtime helpers"
    , "sliderSource :: a -> AddHandler a -> MomentIO (Behavior a)"
    , "sliderSource d ah = fromAddHandler ah >>= stepper d"
    , "stringSource :: String -> AddHandler String -> MomentIO (Behavior String)"
    , "stringSource = sliderSource"
    , "boolSource :: Bool -> AddHandler Bool -> MomentIO (Behavior Bool)"
    , "boolSource = sliderSource"
    , "buttonSource :: AddHandler () -> MomentIO (Behavior (Maybe ()))"
    , "buttonSource ah = fromAddHandler ah >>= stepper Nothing . fmap (const (Just ()))"
    , "reactimateSink :: Behavior (IO ()) -> MomentIO ()"
    , "reactimateSink b = do { i <- valueB b; liftIO i; e <- changes b; reactimate' e }"
    ]

reactiveMeta :: CabalMeta -> CabalMeta
reactiveMeta meta = meta{metaDeps = "reactive-banana" : metaDeps meta}

-- ---------------------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------------------

intercalateBlank :: [[Text]] -> [Text]
intercalateBlank = intercalate [""]

dedup :: [Text] -> [Text]
dedup = go []
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs

capitalize :: Text -> Text
capitalize t = case T.uncons t of
    Just (c, r) -> T.cons (toUpper c) r
    Nothing -> t
