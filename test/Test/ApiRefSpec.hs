{-# LANGUAGE OverloadedStrings #-}

{- | The @api_reference@ merge core: given results from local Hoogle, live
@:browse@, and typed-hole fits (plus the curated static slice), 'mergeApiRef'
prefers the dynamic sources and falls back to the static card only when every
dynamic source is empty/unusable. 'argKind' routes a type-shaped arg to hole
fits and a module/name arg to Hoogle + @:browse@.
-}
module Test.ApiRefSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec

import Sabela.AI.Capabilities.ApiRef (
    ApiRefSources (..),
    ArgKind (..),
    argKind,
    mergeApiRef,
 )
import Sabela.AI.HoogleResolve (HoogleHit (..))

field :: String -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromString k) o
field _ _ = Nothing

hit :: HoogleHit
hit =
    HoogleHit
        { hhName = "readCsv"
        , hhModule = "DataFrame"
        , hhPackage = "dataframe"
        , hhType = "FilePath -> IO DataFrame"
        , hhDocs = ""
        }

sources :: ApiRefSources
sources = ApiRefSources "DataFrame" [] Nothing Nothing "STATIC CARD"

spec :: Spec
spec = describe "api_reference merge core" $ do
    describe "argKind" $ do
        it "routes a function type to hole fits" $
            argKind "[Int] -> Int" `shouldBe` AsType
        it "routes an explicit hole to hole fits" $
            argKind "_ :: a -> a" `shouldBe` AsType
        it "routes a module/name arg to Hoogle + browse" $ do
            argKind "DataFrame" `shouldBe` AsModuleOrName
            argKind "Granite.Svg" `shouldBe` AsModuleOrName

    describe "mergeApiRef" $ do
        it "falls back to the static card when every dynamic source is empty" $ do
            let v = mergeApiRef sources
            field "source" v `shouldBe` Just (String "static")
            field "reference" v `shouldBe` Just (String "STATIC CARD")
            field "hoogle" v `shouldBe` Nothing

        it "prefers Hoogle hits over the static card" $ do
            let v = mergeApiRef sources{arsHoogle = [hit]}
            field "source" v `shouldBe` Just (String "live")
            field "reference" v `shouldBe` Nothing
            field "hoogle" v `shouldSatisfy` (/= Nothing)

        it "surfaces live :browse exports" $ do
            let v = mergeApiRef sources{arsBrowse = Just "data DataFrame"}
            field "source" v `shouldBe` Just (String "live")
            field "exports" v `shouldBe` Just (String "data DataFrame")

        it "surfaces typed-hole fits" $ do
            let v = mergeApiRef sources{arsHoles = Just "readCsv :: FilePath -> IO DataFrame"}
            field "source" v `shouldBe` Just (String "live")
            field "holeFits" v `shouldSatisfy` (/= Nothing)

        it "treats a not-in-scope/error browse as empty and falls back" $ do
            let v = mergeApiRef sources{arsBrowse = Just "error: Not in scope: DataFrame"}
            field "source" v `shouldBe` Just (String "static")
            field "exports" v `shouldBe` Nothing
