{- |
Module      : Sabela.Notebook.Anim
Description : Turn a moving picture into an animation you can watch.

An __animation__ is just a picture that depends on time: give it a moment @t@ and
it hands you the picture at that moment. 'animate' plays one in the notebook.

= How to make an animation

Write a function from time to a 'Picture'. Use the time however you like — to
move something, grow it, spin it:

>>> animate 4 (\t -> fill blue (circle (150 + 100 * sin t, 150) 30))
-- a blue circle sliding left and right, for 4 seconds, looping

If you already have a behaviour (from "Sabela.Notebook.Frp"), use 'animateB':

>>> animateB 4 (fill green . circle (150, 150) <$> (40 + 20 * sin time))
-- a green circle whose radius breathes

= How it works (and what it can't do)

'animate' takes snapshots of your picture at a steady rate (30 per second by
default) and the notebook plays them back in the browser like a flip-book. The
Haskell side is /not/ running during playback, which keeps things smooth.

The trade-off: an animation is a __recording__, not a live simulation. It has a
fixed length and cannot react to the mouse /while it plays/ (for that, change a
widget and the cell re-runs, producing a new recording).
-}
module Sabela.Notebook.Anim (
    -- * Playing an animation
    animate,
    animateB,

    -- * Options
    AnimOpts (..),
    defaultAnim,
    animateWith,

    -- * Building the HTML (also handy for testing)
    renderAnimation,
    frameSvgs,
) where

import Data.List (intercalate)
import Sabela.Notebook.Behavior (Behavior, Time, at)
import Sabela.Notebook.Picture (Canvas, Picture, defaultCanvas, renderSvg)

-- | How to record an animation.
data AnimOpts = AnimOpts
    { animCanvas :: Canvas
    -- ^ The drawing area.
    , animFps :: Int
    -- ^ Snapshots per second (more = smoother, bigger).
    }

-- | The default recording: a 300×300 canvas at 30 snapshots per second.
defaultAnim :: AnimOpts
defaultAnim = AnimOpts defaultCanvas 30

{- | Play a moving picture in the notebook for the given number of seconds.

>>> animate 3 (\t -> fill red (circle (150, 150) (40 + 30 * sin t)))
-}
animate :: Time -> (Time -> Picture) -> IO ()
animate = animateWith defaultAnim

{- | Play a 'Behavior' of pictures (see "Sabela.Notebook.Frp"). Exactly
@'animate' seconds ('at' b)@.
-}
animateB :: Time -> Behavior Picture -> IO ()
animateB seconds b = animate seconds (at b)

-- | Like 'animate', but with your own canvas size and frame rate.
animateWith :: AnimOpts -> Time -> (Time -> Picture) -> IO ()
animateWith opts seconds f = do
    putStrLn "<!-- MIME:text/html -->"
    putStrLn (renderAnimation opts seconds f)

{- | The snapshots an animation is made of: the picture sampled at evenly spaced
moments over @[0, seconds)@, each rendered to an SVG string.
-}
frameSvgs :: AnimOpts -> Time -> (Time -> Picture) -> [String]
frameSvgs opts seconds f =
    [ renderSvg (animCanvas opts) (f (seconds * fromIntegral i / fromIntegral n))
    | i <- [0 .. n - 1]
    ]
  where
    n = max 1 (round (seconds * fromIntegral (animFps opts)) :: Int)

{- | Build the self-contained HTML that plays an animation: the snapshots as a
JavaScript array plus a tiny @requestAnimationFrame@ player that flips through
them, looping. The browser does the playback; the kernel is not involved.
-}
renderAnimation :: AnimOpts -> Time -> (Time -> Picture) -> String
renderAnimation opts seconds f =
    "<div></div><script>"
        ++ "(function(){var frames=["
        ++ jsArray (frameSvgs opts seconds f)
        ++ "];var fps="
        ++ show (animFps opts)
        ++ ";var host=document.currentScript.previousElementSibling;"
        ++ "var t0=null;function step(t){if(t0===null)t0=t;"
        ++ "var i=Math.floor((t-t0)/1000*fps)%frames.length;"
        ++ "host.innerHTML=frames[i];requestAnimationFrame(step);}"
        ++ "requestAnimationFrame(step);})();"
        ++ "</script>"

-- | Render a list of strings as a comma-separated list of JS string literals.
jsArray :: [String] -> String
jsArray = intercalate "," . map jsString

{- | Escape a string as a JavaScript double-quoted literal. Slashes are escaped
(@\/@), which is harmless in JS but means a frame containing @\</script\>@
becomes @\<\\/script\>@ and so cannot close the surrounding @\<script\>@.
-}
jsString :: String -> String
jsString s = '"' : concatMap esc s ++ "\""
  where
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '/' = "\\/"
    esc c = [c]
