{-# LANGUAGE OverloadedStrings #-}

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

validJson :: Text -> Bool
validJson static =
    isJust (decodeStrict (TE.encodeUtf8 unescaped) :: Maybe Value)
  where
    unescaped = T.replace "<\\/" "</" static

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

afterAnchor :: Text -> Text -> Maybe Text
afterAnchor src var
    | T.null rest = Nothing
    | otherwise = Just (T.dropWhile isSpace (T.drop (T.length anchor) rest))
  where
    anchor = "window." <> var <> " = "
    (_, rest) = T.breakOn anchor src

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle repl hay
    | T.null rest = hay
    | otherwise = before <> repl <> T.drop (T.length needle) rest
  where
    (before, rest) = T.breakOn needle hay
