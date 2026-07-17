{-# LANGUAGE OverloadedStrings #-}

{- | Freeze widget binds in exported notebook source: a @x <- display
(slider "n" def lo hi)@ becomes @x = <current value>@ (or the
constructor's default if no live value is recorded), so the emitted
file is a plain Haskell program. Composed or unrecognised widget
expressions are left alone.
-}
module Sabela.Export.Widget (
    -- * Bind parsing
    WidgetBind (..),
    parseWidgetBind,
    widgetDefault,

    -- * Freezing pipeline
    freezeWidgetSource,

    -- * Pieces (exposed for testing)
    splitArgs,
) where

import Control.Monad ((>=>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Export.Analyze (widgetConstructors)

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
    deriving (Eq, Show)

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
argName args i = argAt args i >>= (T.stripPrefix "\"" >=> T.stripSuffix "\"")

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
