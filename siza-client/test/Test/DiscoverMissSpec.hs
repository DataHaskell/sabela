{-# LANGUAGE OverloadedStrings #-}

{- | The discover miss protocol after the union-merge redesign: a genuine
absence is a firm, actionable answer (R5.4); a miss names what was consulted
(R1.2); and the retry-loop boilerplate ("retry discover with a different
shape", "Do not invent") is gone (R5.8).
-}
module Test.DiscoverMissSpec (discoverMissSpec) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    HackageInfo (..),
    NotebookEnv (..),
    okAnswer,
    seededBuiltins,
 )
import Siza.Agent.DiscoverTool (packageShaped)
import Test.DiscoverFixtures (hitText, hitsOf, stateOf, textField)

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

emptyAll :: Text -> HackageInfo -> Value
emptyAll q =
    discoverEnvelope
        env0
        (interpret env0 q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]

discoverMissSpec :: Spec
discoverMissSpec = describe "package-aware discover miss (union merge)" $ do
    describe "packageShaped — a query that could name a Hackage package" $ do
        it "accepts lowercase single tokens" $ do
            packageShaped "megaparsec" `shouldBe` True
            packageShaped "parser-combinators" `shouldBe` True
        it "rejects module paths, prose, and type shapes" $ do
            packageShaped "Data.List" `shouldBe` False
            packageShaped "edit distance between strings" `shouldBe` False
            packageShaped "[Int] -> Int" `shouldBe` False

    describe "a package known upstream is a firm absent-known answer (R5.4)" $ do
        it "surfaces the package hit with its exact build-depends line" $ do
            let v = emptyAll "megaparsec" (HackageInfo True ["megaparsec"])
            stateOf v `shouldBe` "found"
            map (hitText "install") (hitsOf v)
                `shouldSatisfy` elem "absent-known"
            map (hitText "cabal") (hitsOf v)
                `shouldSatisfy` elem "-- cabal: build-depends: megaparsec"
        it "the next step names the install action" $ do
            let v = emptyAll "megaparsec" (HackageInfo True ["megaparsec"])
            textField "next" v
                `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: megaparsec"

    describe "a genuine miss is clean and consulted-scoped (R1.2, R5.8)" $ do
        it "stays not_found for a name nothing holds" $ do
            let v = emptyAll "frobwizzle" (HackageInfo True [])
            stateOf v `shouldBe` "not_found"
        it "never emits the retry boilerplate or the invent ban" $ do
            let v = emptyAll "frobwizzle" (HackageInfo True [])
                nxt = textField "next" v
            nxt `shouldSatisfy` (not . T.isInfixOf "retry discover")
            nxt `shouldSatisfy` (not . T.isInfixOf "Do not invent")
        it "does not suggest installing a name hackage does not know" $ do
            let v = emptyAll "frobwizzle" (HackageInfo True [])
            textField "next" v
                `shouldSatisfy` (not . T.isInfixOf "build-depends: frobwizzle")
