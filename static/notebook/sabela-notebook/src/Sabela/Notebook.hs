{- |
Module      : Sabela.Notebook
Description : One import for everything: FRP + pictures (+ animation, widgets).

This is the friendliest starting point. A single

> import Sabela.Notebook

brings into scope:

* __Behaviours and events__ — values that change over time and things that
  happen at moments ('Sabela.Notebook.Frp').
* __Pictures__ — drawings built from simple shapes ('Sabela.Notebook.Picture').

Everything is written for people new to programming, with a runnable example on
every function. Start by reading 'Sabela.Notebook.Behavior' and
'Sabela.Notebook.Picture'.

>>> picture (fill red (circle (150, 150) 80))   -- draw a red circle
-}
module Sabela.Notebook (
    module Sabela.Notebook.Frp,
    module Sabela.Notebook.Picture,
    module Sabela.Notebook.Anim,
) where

import Sabela.Notebook.Anim
import Sabela.Notebook.Frp
import Sabela.Notebook.Picture
