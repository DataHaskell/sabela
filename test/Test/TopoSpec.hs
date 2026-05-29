{-# LANGUAGE OverloadedStrings #-}

{- | Umbrella spec for 'Sabela.Topo'; the actual cases live in
'Test.TopoSpec.Order', 'Test.TopoSpec.Select', and 'Test.TopoSpec.Names'.
-}
module Test.TopoSpec (spec) where

import Test.Hspec
import qualified Test.TopoSpec.Names as Names
import qualified Test.TopoSpec.Order as Order
import qualified Test.TopoSpec.Select as Select

spec :: Spec
spec = describe "Sabela.Topo" $ do
    Order.spec
    Select.spec
    Names.spec
