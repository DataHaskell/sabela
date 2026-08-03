{-# LANGUAGE OverloadedStrings #-}

module Sabela.Output where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Output.Scatter (scatterDefs)
import Sabela.Output.Widgets (widgetDefs)

{- | Every name this block looks up is reached through one of its own
qualified aliases, and it imports nothing unqualified, so neither can a user's
cell make it ambiguous nor can it outrank that cell's own @hiding@ clause.
-}
displayPrelude :: Text
displayPrelude =
    T.unlines
        ( baseAliases
            <> [ "import qualified System.IO.Unsafe as SabelaUnsafe"
               , "import qualified Control.Exception"
               , "import qualified Data.Proxy"
               , "import qualified Data.Typeable"
               , ":{"
               , "data Input a = Input {iValue :: SabelaBase.IO a, iShow :: SabelaBase.IO ()}"
               , "instance SabelaBase.Functor Input where"
               , "    fmap f b = Input{iValue = SabelaBase.fmap f (iValue b), iShow = iShow b}"
               , "instance SabelaBase.Applicative Input where"
               , "    pure x = Input{iValue = SabelaBase.pure x, iShow = SabelaBase.pure ()}"
               , "    bf <*> bx = Input{iValue = iValue bf SabelaBase.<*> iValue bx, iShow = iShow bf SabelaBase.>> iShow bx}"
               , "_sabelaWidgetRef :: SabelaIORef.IORef [(SabelaBase.String, SabelaBase.String)]"
               , "_sabelaWidgetRef = SabelaUnsafe.unsafePerformIO (SabelaIORef.newIORef [])"
               , "_sabelaCellIdRef :: SabelaIORef.IORef SabelaBase.String"
               , "_sabelaCellIdRef = SabelaUnsafe.unsafePerformIO (SabelaIORef.newIORef \"0\")"
               , "displayMime_ :: SabelaBase.String -> SabelaBase.String -> SabelaBase.IO ()"
               , "displayMime_ t c = SabelaBase.putStrLn (\"<!-- MIME:\" SabelaBase.++ t SabelaBase.++ \" -->\") SabelaBase.>> SabelaBase.putStrLn c"
               , "displayHtml :: SabelaBase.String -> SabelaBase.IO ()"
               , "displayHtml = displayMime_ \"text/html\""
               , "displayMarkdown :: SabelaBase.String -> SabelaBase.IO ()"
               , "displayMarkdown = displayMime_ \"text/markdown\""
               , "displaySvg :: SabelaBase.String -> SabelaBase.IO ()"
               , "displaySvg = displayMime_ \"image/svg+xml\""
               , "displayLatex :: SabelaBase.String -> SabelaBase.IO ()"
               , "displayLatex = displayMime_ \"text/latex\""
               , "displayJson :: SabelaBase.String -> SabelaBase.IO ()"
               , "displayJson = displayMime_ \"application/json\""
               , "displayImage :: SabelaBase.String -> SabelaBase.String -> SabelaBase.IO ()"
               , "displayImage mime b64 = SabelaBase.putStrLn (\"<!-- MIME:\" SabelaBase.++ mime SabelaBase.++ \";base64 -->\") SabelaBase.>> SabelaBase.putStrLn b64"
               , "widgetGet :: SabelaBase.String -> SabelaBase.IO (SabelaBase.Maybe SabelaBase.String)"
               , "widgetGet name = SabelaBase.fmap (SabelaBase.lookup name) (SabelaIORef.readIORef _sabelaWidgetRef)"
               , "widgetRead :: SabelaBase.Read a => SabelaBase.String -> a -> SabelaBase.IO a"
               , "widgetRead name def = SabelaBase.fmap (SabelaBase.lookup name) (SabelaIORef.readIORef _sabelaWidgetRef) SabelaBase.>>= \\mv -> SabelaBase.pure (case mv of { SabelaBase.Nothing -> def; SabelaBase.Just s -> case SabelaBase.reads s of { [(v,\"\")] -> v; _ -> def } })"
               , "mkWidget :: Input a -> SabelaBase.IO a"
               , "mkWidget b = iShow b SabelaBase.>> iValue b"
               , "currentValue :: Input a -> SabelaBase.IO a"
               , "currentValue = iValue"
               , "showInput :: Input a -> SabelaBase.IO ()"
               , "showInput = iShow"
               , "constInput :: a -> Input a"
               , "constInput x = Input{iValue = SabelaBase.pure x, iShow = SabelaBase.pure ()}"
               , "-- Deprecated aliases; prefer Input / currentValue / showInput."
               , "type Behavior = Input"
               , "sample :: Input a -> SabelaBase.IO a"
               , "sample = iValue"
               , "render :: Input a -> SabelaBase.IO ()"
               , "render = iShow"
               , "exportBridge :: SabelaBase.String -> SabelaBase.String -> SabelaBase.IO ()"
               , "exportBridge name val = SabelaBase.putStrLn (\"<!-- MIME:EXPORT:\" SabelaBase.++ name SabelaBase.++ \" -->\") SabelaBase.>> SabelaBase.putStrLn val SabelaBase.>> SabelaBase.putStrLn \"<!-- MIME:text/plain -->\""
               ]
        )
        <> widgetDefs
        <> scatterDefs
        <> ":}\n"

{- | The base modules this block resolves its ambient names against, all under
one alias. Prelude is deliberately not among them: an import of it here would
sit in the session for good and outrank a user cell's own @hiding@ clause.
-}
baseAliases :: [Text]
baseAliases =
    [ "import qualified " <> m <> " as SabelaBase"
    | m <-
        [ "Control.Applicative"
        , "Control.Monad"
        , "Data.Bool"
        , "Data.Eq"
        , "Data.Function"
        , "Data.Functor"
        , "Data.Int"
        , "Data.List"
        , "Data.Maybe"
        , "Data.String"
        , "GHC.Float"
        , "GHC.Real"
        , "System.IO"
        , "Text.Read"
        , "Text.Show"
        ]
    ]
        <> ["import qualified Data.IORef as " <> ioRefAlias]

{- | The alias the injected block reaches @Data.IORef@ by. Written once, so a
preamble prepended to a user's cell cannot drift from the import behind it.
-}
ioRefAlias :: Text
ioRefAlias = "SabelaIORef"

pureAdmittedMarker, pureIOMarker, pureValueMarker, pureErrorMarker :: Text
pureAdmittedMarker = "---SABELA_PURE_ADMITTED---"
pureIOMarker = "---SABELA_PURE_IO---"
pureValueMarker = "---SABELA_PURE_VALUE---"
pureErrorMarker = "---SABELA_PURE_ERROR---"

pureValueCap :: Int
pureValueCap = 4000

mimeMarkerHtmlPrefix :: Text
mimeMarkerHtmlPrefix = "<!-- MIME:"

mimeMarkerHtmlSuffix :: Text
mimeMarkerHtmlSuffix = " -->"

mimeMarkerPrefix :: Text
mimeMarkerPrefix = "---MIME:"

mimeMarkerSuffix :: Text
mimeMarkerSuffix = "---"

parseMarker :: Text -> Maybe Text
parseMarker l =
    (T.stripPrefix mimeMarkerHtmlPrefix l >>= T.stripSuffix mimeMarkerHtmlSuffix)
        <|> (T.stripPrefix mimeMarkerPrefix l >>= T.stripSuffix mimeMarkerSuffix)

parseMimeOutputs :: Text -> [(Text, Text)]
parseMimeOutputs raw =
    let ls = map (T.dropWhileEnd (== '\r')) (T.lines raw)
        (finalMime, finalLines, acc) = foldl step ("text/plain", [], []) ls
        finalBlock = T.unlines (reverse finalLines)
        result =
            if T.null (T.strip finalBlock)
                then acc
                else (T.strip finalMime, finalBlock) : acc
     in reverse result
  where
    step (curMime, curLines, acc) l =
        case parseMarker l of
            Just mime
                | not (T.null (T.strip mime)) ->
                    let block = T.unlines (reverse curLines)
                        acc' =
                            if T.null (T.strip block)
                                then acc
                                else (T.strip curMime, block) : acc
                     in (mime, [], acc')
            _ -> (curMime, l : curLines, acc)
