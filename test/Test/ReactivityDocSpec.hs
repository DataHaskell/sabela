{-# LANGUAGE OverloadedStrings #-}

{- | Pins the planner against @reactivity.md@. Each section of that document
has a sibling module; a promise the code does not keep yet is a 'knownGap'.
-}
module Test.ReactivityDocSpec (spec) where

import Test.Hspec (Spec, describe)
import qualified Test.ReactivityDoc.Effects as Effects
import qualified Test.ReactivityDoc.Types as Types
import qualified Test.ReactivityDoc.Values as Values

spec :: Spec
spec = describe "reactivity.md" $ do
    Values.spec
    Types.spec
    Effects.spec
