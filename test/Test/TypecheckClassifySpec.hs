{-# LANGUAGE OverloadedStrings #-}

{- | Which cells the value type-check gate treats as definitions. Misreading a
comparison as one wraps an expression in @let { … }@, and the gate then
refuses the cell with a parse error on the brace it added itself.
-}
module Test.TypecheckClassifySpec (spec) where

import Sabela.Session.Query (TypecheckInput (..), classifyTypecheckInput)
import Test.Hspec

spec :: Spec
spec = describe "value-subset classification" $ do
    it "reads a comparison as the expression it is, not a definition" $
        map
            classifyTypecheckInput
            [ "print (1 == 1)"
            , "print (parityTotal == 55)"
            , "putStrLn (if (total == 55) then \"A\" else \"B\")"
            , "print (x /= y)"
            , "print (a <= b && c >= d)"
            , "print (xs >>= f)"
            ]
            `shouldBe` replicate 6 ValueExpression

    it "still reads a definition whose body compares as the binding it is" $
        map
            classifyTypecheckInput
            [ "isOk = total == 55"
            , "let isOk = a /= b"
            , "candidate = 1 + 1"
            ]
            `shouldBe` replicate 3 ValueBindings

    it "keeps declaration forms outside the subset" $
        map
            classifyTypecheckInput
            ["data C = C", "import Data.Map", "type C = Int"]
            `shouldBe` replicate 3 OutsideValueSubset

    {- GHCi answers a bind with "not an expression", so the gate can only
    refuse a cell the compile gate handles properly through its bind proxy. -}
    it "keeps a statement bind outside the subset, however it is laid out" $
        map
            classifyTypecheckInput
            [ "idxFiles <- forM idxNames fetchIdx"
            , "probeBind <- pure (1 :: Int)"
            , "idxFiles <-\n    forM idxNames fetchIdx"
            , "(a, b) <- pure (1 :: Int, 2 :: Int)"
            ]
            `shouldBe` replicate 4 OutsideValueSubset

    it "still admits a multi-line expression, which the query wraps" $
        classifyTypecheckInput "print\n  (1 :: Int)" `shouldBe` ValueExpression

    it "still admits a multi-line run of definitions, which it wraps itself" $
        classifyTypecheckInput "a = 1\nb = 2" `shouldBe` ValueBindings
