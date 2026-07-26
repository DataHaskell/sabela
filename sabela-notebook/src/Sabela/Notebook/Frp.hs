module Sabela.Notebook.Frp (
    module Sabela.Notebook.Behavior,
    module Sabela.Notebook.Event,
    integral,
    integralFrom,
    derivative,
) where

import Sabela.Notebook.Behavior
import Sabela.Notebook.Event

integral :: Behavior Double -> Behavior Double
integral = integralFrom 0

integralFrom :: Time -> Behavior Double -> Behavior Double
integralFrom t0 b = Behavior area
  where
    dt = 0.005
    area t =
        let n = max 0 (floor ((t - t0) / dt)) :: Int
         in sum [at b (t0 + fromIntegral i * dt) * dt | i <- [0 .. n - 1]]

derivative :: Behavior Double -> Behavior Double
derivative b = Behavior (\t -> (at b (t + h) - at b (t - h)) / (2 * h))
  where
    h = 0.001
