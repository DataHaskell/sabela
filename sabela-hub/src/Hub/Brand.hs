{-# LANGUAGE OverloadedStrings #-}

{- | The DataHaskell mark — a plus, a square, a triangle and a circle — as
the favicon every hub page links and as the small inline mark drawn before
the word "Sabela" in page headers.
-}
module Hub.Brand (faviconTag, brandMark, markPurple) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The purple of the DataHaskell wordmark.
markPurple :: Text
markPurple = "#5a287f"

-- | The four shapes on a 24-unit grid, in the given fill.
shapes :: Text -> Text
shapes fill =
    T.concat
        [ "<path fill='"
            <> fill
            <> "' d='M5.6 3h2.8v2.6H11v2.8H8.4V11H5.6V8.4H3V5.6h2.6z'/>"
        , "<rect fill='" <> fill <> "' x='9.4' y='9.4' width='5.6' height='5.6'/>"
        , "<path fill='" <> fill <> "' d='M3 21l4-7 4 7z'/>"
        , "<circle fill='" <> fill <> "' cx='18.6' cy='18.6' r='3'/>"
        ]

-- | A link tag: white shapes on the purple square, as on datahaskell.org.
faviconTag :: Text
faviconTag =
    "<link rel=\"icon\" href=\"data:image/svg+xml,"
        <> encode
            ( "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'>"
                <> "<rect width='24' height='24' fill='"
                <> markPurple
                <> "'/>"
                <> shapes "#fff"
                <> "</svg>"
            )
        <> "\">"
  where
    encode =
        T.replace "#" "%23"
            . T.replace "<" "%3C"
            . T.replace ">" "%3E"
            . T.replace "\"" "'"

-- | The mark inline, purple on whatever is behind it, sized to body text.
brandMark :: Text
brandMark =
    "<svg aria-hidden=\"true\" viewBox=\"0 0 24 24\" width=\"14\" height=\"14\""
        <> " style=\"vertical-align:-2px;margin-right:6px;flex-shrink:0\">"
        <> shapes markPurple
        <> "</svg>"
