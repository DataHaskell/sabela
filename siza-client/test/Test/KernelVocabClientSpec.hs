{-# LANGUAGE OverloadedStrings #-}

{- | R1.7 for the kernel-state surface: the client catalogue's kernel tool
descriptions advertise exactly the closed vocabulary the server emits
("Sabela.AI.KernelVocab") — every await tag and kernel state a caller can
receive is named, so no state string ever arrives undocumented (M14).
-}
module Test.KernelVocabClientSpec (kernelVocabClientSpec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.KernelVocab (
    tagBuilding,
    tagCold,
    tagExecuting,
    tagIdle,
    tagKernelDead,
    tagSettled,
    tagTimedOut,
 )
import Siza.Agent.Tools (catalogue)
import Test.Hspec

-- | The description of the named tool in the live catalogue.
descriptionOf :: Text -> IO Text
descriptionOf name = do
    cat <- catalogue
    let descs =
            [ d
            | Object o <- cat
            , Just (Object f) <- [KM.lookup "function" o]
            , KM.lookup "name" f == Just (String name)
            , Just (String d) <- [KM.lookup "description" f]
            ]
    pure (fromMaybe "" (headMaybe descs))
  where
    headMaybe (x : _) = Just x
    headMaybe [] = Nothing

kernelVocabClientSpec :: Spec
kernelVocabClientSpec = describe "kernel tool descriptions advertise the closed vocabulary" $ do
    it "await_idle names every waited tag it can return" $ do
        d <- descriptionOf "await_idle"
        mapM_
            (\t -> (t, T.isInfixOf t d) `shouldBe` (t, True))
            [tagIdle, tagSettled, tagTimedOut, tagKernelDead]

    it "kernel_status names every state tag it can return" $ do
        d <- descriptionOf "kernel_status"
        mapM_
            (\t -> (t, T.isInfixOf t d) `shouldBe` (t, True))
            [tagCold, tagIdle, tagExecuting, tagBuilding]

    it "await_idle documents the resource runaway line" $ do
        d <- descriptionOf "await_idle"
        T.isInfixOf "resource" d `shouldBe` True
