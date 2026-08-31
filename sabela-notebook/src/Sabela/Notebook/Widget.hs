{- | Widgets you write in Haskell.

A widget is a pure function from what the reader has done to what they see.
Controls hand back an 'Sabela.Notebook.Event.Event' of their own
occurrences, so state is spelled with the FRP vocabulary rather than with
callbacks, and nothing is stored anywhere except the log of interactions.

Show one in a cell:

> import Sabela.Notebook.Widget
>
> tally :: Ui ()
> tally = do
>   up <- pushButton "+"
>   n  <- sample (accumB (0 :: Int) (fmap (const (+ 1)) up))
>   say ("count: " ++ show n)
>
> count <- mkWidget (htmlWidget "tally" (renderWidget tally))

For ready-made widgets that need no program of your own, see
"Sabela.Notebook.Widget.Kit".
-}
module Sabela.Notebook.Widget (
    module Sabela.Notebook.Widget.Dsl,
    Ui,
    Store,
    renderWidget,
    valueOf,
    describeUi,
    slotsOf,
) where

import Sabela.Notebook.Widget.Dsl
import Sabela.Notebook.Widget.Interpret (
    Store,
    describeUi,
    renderUi,
    slotsOf,
    valueOf,
 )
import Sabela.Notebook.Widget.Runtime (runtimeCss, runtimeJs)
import Sabela.Notebook.Widget.Types (Ui)

{- | Draws a widget program and reports its value. The three trailing arguments
are what Sabela's @htmlWidget@ supplies: the widget's name, the cell it lives
in, and everything the browser has stored for that cell.
-}
renderWidget :: Ui a -> String -> String -> Store -> (String, a)
renderWidget program name cell store = (html, value)
  where
    (body, value) = renderUi name store program
    elId = "sbw_" ++ cell ++ "_" ++ name
    html =
        concat
            [ "<style>"
            , runtimeCss
            , "</style><div id='"
            , elId
            , "' class='sbw'>"
            , body
            , "</div><script>"
            , runtimeJs
            , "sabelaUi({cid:"
            , cell
            , ",root:'"
            , elId
            , "'});"
            , "</script>"
            ]
