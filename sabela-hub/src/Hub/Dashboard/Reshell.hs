{-# LANGUAGE OverloadedStrings #-}

{- | Re-shell a gallery dashboard\/notebook export into the current template.

An export is the template with its @\/*__SABELA_INJECT__*\/@ placeholder
replaced by @window.__SABELA_STATIC__ = <json>;@ (plus, for notebook-mode
exports, the render-mode flag and the markdown literal). When the template's
chrome moves on, an old export keeps its stale chrome. 'reshell' lifts the
injected data out of the old export and drops it into the current template, so
the outputs are preserved exactly with no re-run. Port of the former
@tools\/reshell-dashboard.py@.
-}
module Hub.Dashboard.Reshell (
    reshell,
    extractObject,
    extractString,
    placeholder,
) where

import Data.Aeson (Value, decodeStrict)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

placeholder :: Text
placeholder = "/*__SABELA_INJECT__*/"

{- | Re-shell @src@ (an old export) into @tmpl@ (the current template). 'Left'
on a missing placeholder, an absent @__SABELA_STATIC__@ payload, or a payload
that is not valid JSON.
-}
reshell :: Text -> Text -> Either String Text
reshell src tmpl
    | not (placeholder `T.isInfixOf` tmpl) =
        Left ("template has no " ++ T.unpack placeholder ++ " placeholder")
    | otherwise = case extractObject src "__SABELA_STATIC__" of
        Nothing -> Left "no __SABELA_STATIC__ in source export"
        Just static
            | not (validJson static) ->
                Left "__SABELA_STATIC__ is not valid JSON"
            | otherwise -> Right (replaceFirst placeholder inject tmpl)
          where
            inject =
                "window.__SABELA_STATIC__ = "
                    <> static
                    <> ";"
                    <> opt "__SABELA_RENDER_MODE__"
                    <> opt "__SABELA_MARKDOWN__"
            opt var =
                maybe
                    ""
                    (\v -> "\nwindow." <> var <> " = " <> v <> ";")
                    (extractString src var)

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
