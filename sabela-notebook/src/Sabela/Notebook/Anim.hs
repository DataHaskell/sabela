module Sabela.Notebook.Anim (
    animate,
    animateB,
    AnimOpts (..),
    defaultAnim,
    animateWith,
    renderAnimation,
    frameSvgs,
) where

import Data.List (intercalate)
import Sabela.Notebook.Behavior (Behavior, Time, at)
import Sabela.Notebook.Markup (Html (..), Svg (..))
import Sabela.Notebook.Picture (Canvas, Picture, defaultCanvas, renderSvg)

data AnimOpts = AnimOpts
    { animCanvas :: Canvas
    , animFps :: Int
    }

defaultAnim :: AnimOpts
defaultAnim = AnimOpts defaultCanvas 30

animate :: Time -> (Time -> Picture) -> IO ()
animate = animateWith defaultAnim

animateB :: Time -> Behavior Picture -> IO ()
animateB seconds b = animate seconds (at b)

animateWith :: AnimOpts -> Time -> (Time -> Picture) -> IO ()
animateWith opts seconds f = do
    putStrLn "<!-- MIME:text/html -->"
    putStrLn (unHtml (renderAnimation opts seconds f))

frameSvgs :: AnimOpts -> Time -> (Time -> Picture) -> [Svg]
frameSvgs opts seconds f =
    [ renderSvg (animCanvas opts) (f (seconds * fromIntegral i / fromIntegral n))
    | i <- [0 .. n - 1]
    ]
  where
    n = max 1 (round (seconds * fromIntegral (animFps opts)) :: Int)

renderAnimation :: AnimOpts -> Time -> (Time -> Picture) -> Html
renderAnimation opts seconds f =
    Html $
        "<div></div><script>"
            ++ "(function(){var frames=["
            ++ jsArray (map unSvg (frameSvgs opts seconds f))
            ++ "];var fps="
            ++ show (animFps opts)
            ++ ";var host=document.currentScript.previousElementSibling;"
            ++ "var t0=null;function step(t){if(t0===null)t0=t;"
            ++ "var i=Math.floor((t-t0)/1000*fps)%frames.length;"
            ++ "host.innerHTML=frames[i];requestAnimationFrame(step);}"
            ++ "requestAnimationFrame(step);})();"
            ++ "</script>"

jsArray :: [String] -> String
jsArray = intercalate "," . map jsString

jsString :: String -> String
jsString s = '"' : concatMap esc s ++ "\""
  where
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '/' = "\\/"
    esc c = [c]
