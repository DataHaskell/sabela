{-# LANGUAGE OverloadedStrings #-}

module Sabela.Output where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Output.Scatter (scatterDefs)
import Sabela.Output.Widgets (widgetDefs)

displayPrelude :: Text
displayPrelude =
    T.unlines
        [ "import Data.IORef"
        , "import qualified System.IO.Unsafe as SabelaUnsafe"
        , "import qualified Control.Exception"
        , "import qualified Data.Proxy"
        , "import qualified Data.Typeable"
        , ":{"
        , "data Input a = Input {iValue :: IO a, iShow :: IO ()}"
        , "instance Functor Input where"
        , "    fmap f b = Input{iValue = fmap f (iValue b), iShow = iShow b}"
        , "instance Applicative Input where"
        , "    pure x = Input{iValue = pure x, iShow = pure ()}"
        , "    bf <*> bx = Input{iValue = iValue bf <*> iValue bx, iShow = iShow bf >> iShow bx}"
        , "_sabelaWidgetRef :: IORef [(String, String)]"
        , "_sabelaWidgetRef = SabelaUnsafe.unsafePerformIO (newIORef [])"
        , "_sabelaCellIdRef :: IORef String"
        , "_sabelaCellIdRef = SabelaUnsafe.unsafePerformIO (newIORef \"0\")"
        , "displayMime_ :: String -> String -> IO ()"
        , "displayMime_ t c = putStrLn (\"<!-- MIME:\" ++ t ++ \" -->\") >> putStrLn c"
        , "displayHtml :: String -> IO ()"
        , "displayHtml = displayMime_ \"text/html\""
        , "displayMarkdown :: String -> IO ()"
        , "displayMarkdown = displayMime_ \"text/markdown\""
        , "displaySvg :: String -> IO ()"
        , "displaySvg = displayMime_ \"image/svg+xml\""
        , "displayLatex :: String -> IO ()"
        , "displayLatex = displayMime_ \"text/latex\""
        , "displayJson :: String -> IO ()"
        , "displayJson = displayMime_ \"application/json\""
        , "displayImage :: String -> String -> IO ()"
        , "displayImage mime b64 = putStrLn (\"<!-- MIME:\" ++ mime ++ \";base64 -->\") >> putStrLn b64"
        , "widgetGet :: String -> IO (Maybe String)"
        , "widgetGet name = fmap (lookup name) (readIORef _sabelaWidgetRef)"
        , "widgetRead :: Read a => String -> a -> IO a"
        , "widgetRead name def = fmap (lookup name) (readIORef _sabelaWidgetRef) >>= \\mv -> pure $ case mv of { Nothing -> def; Just s -> case reads s of { [(v,\"\")] -> v; _ -> def } }"
        , "display :: Input a -> IO a"
        , "display b = iShow b >> iValue b"
        , "currentValue :: Input a -> IO a"
        , "currentValue = iValue"
        , "showInput :: Input a -> IO ()"
        , "showInput = iShow"
        , "constInput :: a -> Input a"
        , "constInput x = Input{iValue = pure x, iShow = pure ()}"
        , "-- Deprecated aliases; prefer Input / currentValue / showInput."
        , "type Behavior = Input"
        , "sample :: Input a -> IO a"
        , "sample = iValue"
        , "render :: Input a -> IO ()"
        , "render = iShow"
        , "exportBridge :: String -> String -> IO ()"
        , "exportBridge name val = putStrLn (\"<!-- MIME:EXPORT:\" ++ name ++ \" -->\") >> putStrLn val >> putStrLn \"<!-- MIME:text/plain -->\""
        ]
        <> widgetDefs
        <> scatterDefs
        <> ":}\n"

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
    let ls = T.lines raw
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
