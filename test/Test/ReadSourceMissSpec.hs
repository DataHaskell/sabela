{-# LANGUAGE OverloadedStrings #-}

{- | Every way read_source misses, each pinned to its own message and
fields: bad args, unowned or absent modules, a corrupt archive, version
resolution failing outright, re-exports, and near-miss names.
-}
module Test.ReadSourceMissSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Test.ReadSourceWorld (
    defaultSdist,
    run,
    textAt,
    withWorld,
    withWorldSdist,
 )

spec :: Spec
spec = describe "read_source misses" $ do
    it "bad args report the grammar's own message" $ do
        (isErr, km) <- run (object ["name" .= ("difference" :: Text)])
        isErr `shouldBe` True
        textAt km "error" `shouldSatisfy` T.isInfixOf "module"
    it "an unowned module is an honest miss" $
        withWorld $ do
            (isErr, km) <-
                run (object ["module" .= ("Data.Nonesuch.Thing" :: Text)])
            isErr `shouldBe` True
            textAt km "error" `shouldSatisfy` T.isInfixOf "Data.Nonesuch.Thing"
    it "several owners are named with paste-ready calls" $
        withWorld $ do
            (isErr, km) <- run (object ["module" .= ("Web.Shared" :: Text)])
            isErr `shouldBe` True
            textAt km "error" `shouldSatisfy` T.isInfixOf "widgets-a"
            textAt km "error" `shouldSatisfy` T.isInfixOf "widgets-b"
            textAt km "error"
                `shouldSatisfy` T.isInfixOf "package: \"widgets-a\""
    it "a cache miss with no manager names the caches" $
        withWorld $ do
            (isErr, km) <-
                run
                    ( object
                        [ "module" .= ("Data.HodaTime.Instant" :: Text)
                        , "version" .= ("9.9.9" :: Text)
                        ]
                    )
            isErr `shouldBe` True
            textAt km "error" `shouldSatisfy` T.isInfixOf "sdists"
    it "no version anywhere asks for an explicit one" $
        withWorld $ do
            (isErr, km) <- run (object ["module" .= ("Data.Verless" :: Text)])
            isErr `shouldBe` True
            textAt km "error"
                `shouldSatisfy` T.isInfixOf "no release could be determined"
            textAt km "error" `shouldSatisfy` T.isInfixOf "version"
    it "a module the index lists but the sdist lacks names what is there" $
        withWorld $ do
            (isErr, km) <-
                run (object ["module" .= ("Data.HodaTime.Duration" :: Text)])
            isErr `shouldBe` True
            textAt km "error" `shouldSatisfy` T.isInfixOf "contains no file"
            KM.member "candidates" km `shouldBe` True
            case KM.lookup "modules" km of
                Just (Array ms) ->
                    toList ms
                        `shouldSatisfy` elem (String "Data.HodaTime.Instant")
                _ -> expectationFailure "no modules field"
    it "a corrupt cached tarball reads as a bad archive, not a crash" $
        withWorldSdist (BL.take 100 defaultSdist) $ do
            (isErr, km) <-
                run (object ["module" .= ("Data.HodaTime.Instant" :: Text)])
            isErr `shouldBe` True
            textAt km "error"
                `shouldSatisfy` T.isInfixOf "did not read as an sdist archive"
    it "a re-exported name points at the imports that define it" $
        withWorld $ do
            (isErr, km) <-
                run
                    ( object
                        [ "module" .= ("Data.HodaTime.Compat" :: Text)
                        , "name" .= ("difference" :: Text)
                        ]
                    )
            isErr `shouldBe` True
            textAt km "error" `shouldSatisfy` T.isInfixOf "re-export"
            textAt km "error" `shouldSatisfy` T.isInfixOf "`imports`"
            case KM.lookup "imports" km of
                Just (Array is) ->
                    toList is
                        `shouldSatisfy` elem (String "Data.HodaTime.Instant")
                _ -> expectationFailure "no imports field"
    it "a missing decl answers with candidates" $
        withWorld $ do
            (isErr, km) <-
                run
                    ( object
                        [ "module" .= ("Data.HodaTime.Instant" :: Text)
                        , "name" .= ("diference" :: Text)
                        ]
                    )
            isErr `shouldBe` True
            case KM.lookup "candidates" km of
                Just (Array cs) ->
                    toList cs `shouldSatisfy` elem (String "difference")
                _ -> expectationFailure "no candidates"
  where
    toList = foldr (:) []
