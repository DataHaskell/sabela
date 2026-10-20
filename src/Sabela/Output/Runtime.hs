{-# LANGUAGE OverloadedStrings #-}

{- | The widget runtimes, taken out of a notebook's markdown on the way to disk
and put back on the way in. A rendered widget carries its own runtime so an
export works with nothing to fetch — but a notebook is a file a person reads,
and a table that brings 24 kB of JavaScript with it buries the prose.
-}
module Sabela.Output.Runtime (
    elideRuntimes,
    restoreRuntimes,
) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Output.Fence (runtimeClose, runtimeOpen, wrapRuntime)
import Sabela.Output.Grid (gridWidgetJs)
import Sabela.Output.Scatter (scatterWidgetJs)
import Sabela.Output.Widgets (sabelaWidgetsJs)

{- | The runtimes by the name their markup fences them with, so what is read
back is this binary's runtime rather than the one that was written.
-}
runtimes :: [(Text, Text)]
runtimes =
    [ ("grid", gridWidgetJs)
    , ("scatter", scatterWidgetJs)
    , ("widgets", sabelaWidgetsJs)
    ]

{- | A runtime as a notebook written before the fences carries it: the
function its markup goes on to call ends the runtime, and the definition of
that function is what says a runtime is there at all.
-}
legacyRuntimes :: [(Text, Text, [Text])]
legacyRuntimes =
    [ ("grid", "function sabelaGridMountAll(", ["sabelaGridMountAll();"])
    , ("scatter", "function sabelaScatter(", ["sabelaScatter({"])
    ,
        ( "widgets"
        , "function _sabelaMount("
        ,
            [ "sabelaSlider({"
            , "sabelaDropdown({"
            , "sabelaCheckbox({"
            , "sabelaTextInput({"
            , "sabelaButton({"
            ]
        )
    ]

{- | An output as it is written to a notebook file: the markup and the data it
carries, with each runtime cut back to the comment that opens it. Fenced or
not, so a notebook older than the fences is made readable by saving it.
-}
elideRuntimes :: Text -> Text
elideRuntimes t =
    foldr cutLegacy (foldr (cutFenced . fst) t runtimes) legacyRuntimes

{- | An output as it is read back: every opening comment expanded to the
runtime this binary embeds, which is what makes a notebook saved by an older
one render here. Elides first, so restoring twice restores once.
-}
restoreRuntimes :: Text -> Text
restoreRuntimes t = foldr put (elideRuntimes t) runtimes
  where
    put (name, js) = T.replace (runtimeOpen name) (wrapRuntime name js)

-- | Drop what one runtime's fences enclose, wherever they are, keeping the open.
cutFenced :: Text -> Text -> Text
cutFenced name = go
  where
    open = runtimeOpen name
    go s =
        let (before, rest) = T.breakOn open s
         in if T.null rest
                then s
                else
                    let after = T.drop (T.length open) rest
                        (_, closed) = T.breakOn runtimeClose after
                     in if T.null closed
                            then s
                            else
                                before
                                    <> open
                                    <> go (T.drop (T.length runtimeClose) closed)

{- | Drop an unfenced runtime: the body of a plain @\<script\>@ that defines
the function the markup calls, up to the call itself, which stays.
-}
cutLegacy :: (Text, Text, [Text]) -> Text -> Text
cutLegacy (name, signature, ends) = go
  where
    tag = "<script>"
    go s =
        let (before, rest) = T.breakOn tag s
         in if T.null rest
                then s
                else
                    let body = T.drop (T.length tag) rest
                        (script, _) = T.breakOn "</script>" body
                     in case cutBody script of
                            Nothing -> before <> tag <> go body
                            Just (gap, kept) ->
                                before
                                    <> tag
                                    <> runtimeOpen name
                                    <> gap
                                    <> go (kept <> T.drop (T.length script) body)

    {- The runtime runs from the start of the script to the call, whose own
    indentation is not part of it. A script already carrying a fence has been
    dealt with by the pass before this one. -}
    cutBody script
        | not (signature `T.isInfixOf` script) = Nothing
        | "/*sabela:runtime:" `T.isInfixOf` script = Nothing
        | otherwise = case [c | e <- ends, let c = T.breakOn e script, not (T.null (snd c))] of
            [] -> Nothing
            cs ->
                let (dropped, kept) = minimumOn (T.length . fst) cs
                 in Just (gapBefore dropped, kept)
    minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)

{- | What is left between a runtime and the call that follows it: the one line
break the markup put there, never the blank line a runtime's own last newline
would otherwise add to it.
-}
gapBefore :: Text -> Text
gapBefore dropped
    | T.any (== '\n') (T.takeWhileEnd isSpace dropped) = "\n"
    | otherwise = ""
