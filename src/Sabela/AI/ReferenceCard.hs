{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ReferenceCard (
    apiReferenceCard,
) where

import Data.Text (Text)
import qualified Data.Text as T

{- | The small reference card covering Sabela's in-session display and widget
functions — which @:browse@ can't surface because they are injected by
@displayPrelude@ at session start rather than exported from a library module.
For dataframe / granite signatures the @api_reference@ tool queries hoogle and
@:browse@ live.
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
        , "data Input a  -- an interactive control; Functor, Applicative"
        , "display      :: Input a -> IO a  -- render the control, return its value"
        , "currentValue :: Input a -> IO a  -- read without re-rendering"
        , "constInput   :: a -> Input a     -- a fixed value, no control"
        , "slider    :: (Show a, Read a, Integral a) => String -> a -> a -> a -> Input a"
        , "dropdown  :: String -> [String] -> String -> Input String"
        , "checkbox  :: String -> Bool -> Input Bool"
        , "textInput :: String -> String -> Input String"
        , "button    :: String -> String -> Input (Maybe ())"
        , ""
        , "For dataframe / granite signatures, call the api_reference tool."
        ]
