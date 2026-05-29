{-# LANGUAGE OverloadedStrings #-}

module Sabela.Output where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Output.Scatter (scatterDefs)

{- | Inline GHCi prelude that defines the Sabela display/widget API.
Safe to re-run before each cell because it uses ':{ :}' blocks rather than
':load', which would reset the entire GHCi context.
-}
displayPrelude :: Text
displayPrelude =
    T.unlines
        [ "import Data.IORef"
        , "import System.IO.Unsafe (unsafePerformIO)"
        , ":{"
        , "data Behavior a = Behavior {bSample :: IO a, bRender :: IO ()}"
        , "instance Functor Behavior where"
        , "    fmap f b = Behavior{bSample = fmap f (bSample b), bRender = bRender b}"
        , "instance Applicative Behavior where"
        , "    pure x = Behavior{bSample = pure x, bRender = pure ()}"
        , "    bf <*> bx = Behavior{bSample = bSample bf <*> bSample bx, bRender = bRender bf >> bRender bx}"
        , "_sabelaWidgetRef :: IORef [(String, String)]"
        , "_sabelaWidgetRef = unsafePerformIO (newIORef [])"
        , "_sabelaCellIdRef :: IORef String"
        , "_sabelaCellIdRef = unsafePerformIO (newIORef \"0\")"
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
        , "displaySlider :: (Show a, Integral a) => String -> a -> a -> a -> IO ()"
        , "displaySlider name lo hi val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml $ \"<input type='range' min='\" ++ show lo ++ \"' max='\" ++ show hi ++ \"' value='\" ++ show val ++ \"' oninput=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\""
        , "displayButton :: String -> String -> IO ()"
        , "displayButton label name = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml $ \"<button onclick=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:'clicked'},'*')\\\">\""
            <> " ++ label ++ \"</button>\""
        , "displaySelect :: String -> [String] -> String -> IO ()"
        , "displaySelect name opts val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml $ \"<select onchange=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\""
            <> " ++ concatMap (\\o -> \"<option\" ++ (if o == val then \" selected\" else \"\") ++ \">\" ++ o ++ \"</option>\") opts ++ \"</select>\""
        , "display :: Behavior a -> IO a"
        , "display b = bRender b >> bSample b"
        , "sample :: Behavior a -> IO a"
        , "sample = bSample"
        , "render :: Behavior a -> IO ()"
        , "render = bRender"
        , "exportBridge :: String -> String -> IO ()"
        , "exportBridge name val = putStrLn (\"<!-- MIME:EXPORT:\" ++ name ++ \" -->\") >> putStrLn val >> putStrLn \"<!-- MIME:text/plain -->\""
        , "slider :: (Show a, Read a, Integral a) => String -> a -> a -> a -> Behavior a"
        , "slider name def lo hi = Behavior { bSample = widgetRead name def, bRender = widgetRead name def >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" $ \"<input type='range' min='\" ++ show lo ++ \"' max='\" ++ show hi ++ \"' value='\" ++ show val ++ \"' oninput=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\""
            <> " }"
        , "dropdown :: String -> [String] -> String -> Behavior String"
        , "dropdown name opts def = Behavior { bSample = fmap (maybe def id) (widgetGet name), bRender = fmap (maybe def id) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" $ \"<select onchange=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\""
            <> " ++ concatMap (\\o -> \"<option\" ++ (if o == val then \" selected\" else \"\") ++ \">\" ++ o ++ \"</option>\") opts ++ \"</select>\" }"
        , "checkbox :: String -> Bool -> Behavior Bool"
        , "checkbox name def = Behavior { bSample = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name)"
            <> ", bRender = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" $ \"<input type='checkbox'\""
            <> " ++ (if val then \" checked\" else \"\") ++ \" onchange=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:this.checked.toString()},'*')\\\">\""
            <> " }"
        , "textInput :: String -> String -> Behavior String"
        , "textInput name def = Behavior { bSample = fmap (maybe def id) (widgetGet name), bRender = fmap (maybe def id) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" $ \"<input type='text' value='\" ++ val ++ \"' oninput=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:this.value,sel:this.selectionStart},'*')\\\">\""
            <> " }"
        , "button :: String -> String -> Behavior (Maybe ())"
        , "button label name = Behavior { bSample = fmap (\\mv -> case mv of { Just \"clicked\" -> Just (); _ -> Nothing }) (widgetGet name)"
            <> ", bRender = readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" $ \"<button onclick=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid ++ \",name:'\" ++ name ++ \"',value:'clicked'},'*')\\\">\""
            <> " ++ label ++ \"</button>\" }"
        ]
        <> scatterDefs
        <> ":}\n"

-- New (valid-HTML) marker shape. Hidden by every Markdown previewer.
mimeMarkerHtmlPrefix :: Text
mimeMarkerHtmlPrefix = "<!-- MIME:"

mimeMarkerHtmlSuffix :: Text
mimeMarkerHtmlSuffix = " -->"

-- Legacy bare-marker shape, kept accepted on read for back-compat with
-- existing on-disk notebooks. Never emitted by current producers.
mimeMarkerPrefix :: Text
mimeMarkerPrefix = "---MIME:"

mimeMarkerSuffix :: Text
mimeMarkerSuffix = "---"

{- | Unwrap one line into its inner @<mime>@ payload. Accepts the new
@<!-- MIME:m -->@ shape and the legacy @---MIME:m---@ form so old
saved notebooks still load.
-}
parseMarker :: Text -> Maybe Text
parseMarker l =
    (T.stripPrefix mimeMarkerHtmlPrefix l >>= T.stripSuffix mimeMarkerHtmlSuffix)
        <|> (T.stripPrefix mimeMarkerPrefix l >>= T.stripSuffix mimeMarkerSuffix)

{- | Return the raw MIME markers as @Text@ — including the @EXPORT:@
prefix bridge values use. 'Sabela.Handlers.Shared.partitionExports'
splits exports from normal outputs; callers that need a typed
'MimeType' convert via 'Sabela.Model.textToMime' on the normal half.
-}
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
