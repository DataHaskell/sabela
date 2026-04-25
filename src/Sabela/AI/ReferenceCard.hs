{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sabela.AI.ReferenceCard (
    apiReferenceCard,
    fullApiReference,
    sliceApiReference,
) where

import Data.FileEmbed (embedStringFile)
import Data.Text (Text)
import qualified Data.Text as T

{- | The small reference card that's cheap enough to live in the cached
system prefix on every request. Covers only Sabela's in-session display and
widget functions (which @:browse@ can't surface because they're injected by
@displayPrelude@ at session start rather than coming from a library module).

For the full @:browse@ output across @DataFrame@, @DataFrame.Functions@,
@DataFrame.Display.Web.Plot@, and @Granite.Svg@, use the @api_reference@ tool
(backed by 'fullApiReference' / 'sliceApiReference').
-}
apiReferenceCard :: Text
apiReferenceCard =
    T.unlines
        [ "## Sabela display + widgets (in scope after session start)"
        , ""
        , "displayHtml     :: String -> IO ()"
        , "displayMarkdown :: String -> IO ()"
        , "displaySvg      :: String -> IO ()"
        , "displayLatex    :: String -> IO ()"
        , "displayJson     :: String -> IO ()"
        , "displayImage    :: String -> String -> IO ()  -- mime, base64"
        , ""
        , "data Behavior a  -- Functor, Applicative"
        , "display :: Behavior a -> IO a"
        , "sample  :: Behavior a -> IO a"
        , "slider    :: (Show a, Read a, Integral a) => String -> a -> a -> a -> Behavior a"
        , "dropdown  :: String -> [String] -> String -> Behavior String"
        , "checkbox  :: String -> Bool -> Behavior Bool"
        , "textInput :: String -> String -> Behavior String"
        , "button    :: String -> String -> Behavior (Maybe ())"
        , ""
        , "For dataframe / granite signatures, call the api_reference tool."
        ]

{- | Full output of `:browse` for DataFrame, DataFrame.Functions,
DataFrame.Display.Web.Plot, and Granite.Svg. Large (tens of KB). Only exposed
to the LLM via the @api_reference@ tool so it doesn't live in the cached
prefix. Regenerate via @tools/gen-api-reference.sh@.
-}
fullApiReference :: Text
fullApiReference = T.pack $(embedStringFile "data/api-reference.txt")

{- | Return only the section for a specific module caption if @mName@
matches one of the @-- <Module>:@ headings. Falls back to the full text.
-}
sliceApiReference :: Text -> Text
sliceApiReference mName
    | T.null mName = fullApiReference
    | otherwise =
        let ls = T.lines fullApiReference
            isHeader l = "-- " `T.isPrefixOf` l && ":" `T.isInfixOf` l
            needle = T.toLower mName
            matches l = needle `T.isInfixOf` T.toLower l
            (_, afterHdr) = break (\l -> isHeader l && matches l) ls
         in case afterHdr of
                [] -> fullApiReference
                (hdr : rest) ->
                    let section = hdr : takeWhile (not . isHeader) rest
                     in T.intercalate "\n" section
