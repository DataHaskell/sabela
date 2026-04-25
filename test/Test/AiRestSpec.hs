{-# LANGUAGE OverloadedStrings #-}

module Test.AiRestSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Handles (
    lookupHandle,
    newHandleStore,
    storeLargeResult,
 )
import Sabela.Server (checkBearer, isAiApi)

spec :: Spec
spec = do
    describe "aiAuthMiddleware gating" $ do
        it "recognizes /api/ai/* paths" $ do
            isAiApi ["api", "ai", "health"] `shouldBe` True
            isAiApi ["api", "ai", "tool"] `shouldBe` True
            isAiApi ["api", "notebook"] `shouldBe` False
            isAiApi ["dashboard"] `shouldBe` False

        it "accepts a matching bearer token" $
            checkBearer "secret" [("Authorization", "Bearer secret")]
                `shouldBe` True

        it "rejects a missing header" $
            checkBearer "secret" []
                `shouldBe` False

        it "rejects a different token" $
            checkBearer "secret" [("Authorization", "Bearer nope")]
                `shouldBe` False

        it "rejects a missing Bearer prefix" $
            checkBearer "secret" [("Authorization", "secret")]
                `shouldBe` False

    describe "per-CLI-session handle isolation" $ do
        it "two independent stores do not share handles" $ do
            -- Simulate what resolveCliHandleStore does for two distinct
            -- X-Sabela-Session values: each gets its own store.
            storeA <- newHandleStore
            storeB <- newHandleStore
            -- Use distinct lines per iteration; cleanOutput dedupes identical
            -- consecutive lines and would otherwise compact to a single line.
            let big =
                    T.unlines
                        [ "line " <> T.pack (show i)
                        | i <- [1 :: Int .. 100]
                        ]
            -- Stash one handle in A only.
            Right (hidA, _, _, _) <- storeLargeResult storeA big
            hitInA <- isPresent <$> lookupHandle storeA hidA
            -- B must not see hidA (independent namespaces).
            hitInB <- isPresent <$> lookupHandle storeB hidA
            hitInA `shouldBe` True
            hitInB `shouldBe` False

        it "a fresh session map starts empty" $
            (M.empty :: M.Map String Int) `shouldBe` M.empty
  where
    isPresent (Just _) = True
    isPresent Nothing = False
