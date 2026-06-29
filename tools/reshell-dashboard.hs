#!/usr/bin/env cabal
{- cabal:
build-depends: base, text, aeson
ghc-options: -Wall
-}
{-# LANGUAGE OverloadedStrings #-}

{- | Re-shell a gallery dashboard\/notebook export into the current template.

A dashboard\/notebook export is the template with its @\/*__SABELA_INJECT__*\/@
placeholder replaced by @window.__SABELA_STATIC__ = <json>;@ (plus, for
notebook-mode exports, the render-mode flag and the markdown literal). When the
template's chrome moves on (new theme picker, brand, styling) an old export keeps
its stale chrome. This lifts the injected data out of the old export and drops it
into the current template, so the outputs are preserved exactly with no re-run.

    cabal run tools/reshell-dashboard.hs -- <old-export.html> <template.html> <out.html>

@<template.html>@ is the current bundled template, e.g. @static\/dashboard.html@,
which still contains the @\/*__SABELA_INJECT__*\/@ placeholder. The same logic
ships in the hub as "Hub.Dashboard.Reshell".
-}
module Main (main) where

import Data.Aeson (Value, decodeStrict)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

placeholder :: Text
placeholder = "/*__SABELA_INJECT__*/"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [old, tmplPath, out] -> run old tmplPath out
        _ -> do
            prog <- getProgName
            die (prog <> " <old-export.html> <template.html> <out.html>")

run :: FilePath -> FilePath -> FilePath -> IO ()
run old tmplPath out = do
    src <- TIO.readFile old
    tmpl <- TIO.readFile tmplPath
    case reshell src tmpl of
        Left err -> die err
        Right (rendered, static, mode, md) -> do
            TIO.writeFile out rendered
            putStrLn $
                "re-shelled "
                    <> old
                    <> " -> "
                    <> out
                    <> " (static="
                    <> show (T.length static)
                    <> "B, mode="
                    <> maybe "None" T.unpack mode
                    <> ", md="
                    <> (if isJust md then "yes" else "no")
                    <> ")"

die :: String -> IO ()
die msg = hPutStrLn stderr msg >> exitFailure

{- | Re-shell @src@ (an old export) into @tmpl@ (the current template). 'Left'
on a missing placeholder, an absent @__SABELA_STATIC__@ payload, or a payload
that is not valid JSON. On success returns the rendered template plus the lifted
static payload, render-mode, and markdown literals (for the status line).
-}
reshell :: Text -> Text -> Either String (Text, Text, Maybe Text, Maybe Text)
reshell src tmpl
    | not (placeholder `T.isInfixOf` tmpl) =
        Left ("template has no " ++ T.unpack placeholder ++ " placeholder")
    | otherwise = case extractObject src "__SABELA_STATIC__" of
        Nothing -> Left "no __SABELA_STATIC__ in source export"
        Just static
            | not (validJson static) ->
                Left "__SABELA_STATIC__ is not valid JSON"
            | otherwise ->
                Right (replaceFirst placeholder inject tmpl, static, mode, md)
          where
            mode = extractString src "__SABELA_RENDER_MODE__"
            md = extractString src "__SABELA_MARKDOWN__"
            inject =
                "window.__SABELA_STATIC__ = "
                    <> static
                    <> ";"
                    <> opt "__SABELA_RENDER_MODE__" mode
                    <> opt "__SABELA_MARKDOWN__" md
            opt var =
                maybe "" (\v -> "\nwindow." <> var <> " = " <> v <> ";")

{- | The literal escapes @\<\/@ as @\<\\\/@ for script safety; undo that and the
payload must parse as JSON.
-}
validJson :: Text -> Bool
validJson static =
    isJust (decodeStrict (TE.encodeUtf8 unescaped) :: Maybe Value)
  where
    unescaped = T.replace "<\\/" "</" static

{- | Return the @{...}@ object literal assigned to @window.\<var\>@, verbatim
(brace-balanced, string-aware). 'Nothing' if the anchor is absent or the value
is not an object literal.
-}
extractObject :: Text -> Text -> Maybe Text
extractObject src var = do
    body <- afterAnchor src var
    case T.uncons body of
        Just ('{', _) -> T.pack . reverse <$> scan (T.unpack body) (0 :: Int) False False []
        _ -> Nothing
  where
    scan [] _ _ _ _ = Nothing
    scan (c : cs) depth inStr esc acc
        | inStr = case (esc, c) of
            (True, _) -> scan cs depth True False (c : acc)
            (False, '\\') -> scan cs depth True True (c : acc)
            (False, '"') -> scan cs depth False False (c : acc)
            (False, _) -> scan cs depth True False (c : acc)
        | c == '"' = scan cs depth True False (c : acc)
        | c == '{' = scan cs (depth + 1) False False (c : acc)
        | c == '}' =
            let d = depth - 1
             in if d == 0 then Just (c : acc) else scan cs d False False (c : acc)
        | otherwise = scan cs depth False False (c : acc)

{- | Return the @"..."@ string literal assigned to @window.\<var\>@, verbatim
(escape-aware). 'Nothing' if the anchor is absent or the value is not a string.
-}
extractString :: Text -> Text -> Maybe Text
extractString src var = do
    body <- afterAnchor src var
    case T.uncons body of
        Just ('"', rest) -> T.pack . ('"' :) . reverse <$> scan (T.unpack rest) False []
        _ -> Nothing
  where
    scan [] _ _ = Nothing
    scan (c : cs) esc acc
        | esc = scan cs False (c : acc)
        | c == '\\' = scan cs True (c : acc)
        | c == '"' = Just (c : acc)
        | otherwise = scan cs False (c : acc)

-- | The text following @window.\<var\> = @, with leading whitespace dropped.
afterAnchor :: Text -> Text -> Maybe Text
afterAnchor src var
    | T.null rest = Nothing
    | otherwise = Just (T.dropWhile isSpace (T.drop (T.length anchor) rest))
  where
    anchor = "window." <> var <> " = "
    (_, rest) = T.breakOn anchor src

-- | Replace the first occurrence of @needle@ in @hay@ with @repl@.
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle repl hay
    | T.null rest = hay
    | otherwise = before <> repl <> T.drop (T.length needle) rest
  where
    (before, rest) = T.breakOn needle hay
