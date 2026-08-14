{-# LANGUAGE OverloadedStrings #-}

module Test.SdistLocateSpec (spec) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Sdist.Locate (
    LocateMiss (..),
    decompressCapped,
    locateModuleFile,
    unlit,
 )
import Test.WorldFixtures (sdistArchive)

world :: BL.ByteString
world =
    sdistArchive
        [
            ( "pkg-1.0/src/Data/Vapour/Cloud.hs"
            , "module Data.Vapour.Cloud where\ncloud :: Int\ncloud = 1\n"
            )
        ,
            ( "pkg-1.0/src/OtherData/Vapour/Cloud.hs"
            , "module OtherData.Vapour.Cloud where\n"
            )
        ,
            ( "pkg-1.0/test/Data/Vapour/Cloud.hs"
            , "module Data.Vapour.Cloud where -- test copy\n"
            )
        ,
            ( "pkg-1.0/lit/Data/Vapour/Lit.lhs"
            , "prose line\n> module Data.Vapour.Lit where\n> lit = ()\n"
            )
        , ("pkg-1.0/Data/Vapour/Binding.hsc", "module Data.Vapour.Binding where\n")
        ]

spec :: Spec
spec = describe "locating a module inside an sdist" $ do
    it "finds the module by its dotted path" $ do
        case locateModuleFile world "Data.Vapour.Cloud" of
            Right (path, body) -> do
                path `shouldBe` "pkg-1.0/src/Data/Vapour/Cloud.hs"
                body `shouldSatisfy` T.isInfixOf "cloud :: Int"
            Left e -> expectationFailure (show e)

    it "does not let a longer path segment shadow the module (anchoring)" $
        -- OtherData/Vapour/Cloud.hs must never satisfy Data.Vapour.Cloud
        case locateModuleFile world "OtherData.Vapour.Cloud" of
            Right (path, _) ->
                path `shouldBe` "pkg-1.0/src/OtherData/Vapour/Cloud.hs"
            Left e -> expectationFailure (show e)

    it "prefers a library path over a test path" $
        case locateModuleFile world "Data.Vapour.Cloud" of
            Right (path, _) -> path `shouldSatisfy` (notElem "test" . segments)
            Left e -> expectationFailure (show e)

    it "reads a .lhs module, unlit, line count preserved" $
        case locateModuleFile world "Data.Vapour.Lit" of
            Right (path, body) -> do
                path `shouldBe` "pkg-1.0/lit/Data/Vapour/Lit.lhs"
                take 2 (T.lines body)
                    `shouldBe` ["", "module Data.Vapour.Lit where"]
                length (T.lines body) `shouldBe` 3
            Left e -> expectationFailure (show e)

    it "reads a .hsc module" $
        case locateModuleFile world "Data.Vapour.Binding" of
            Right (path, _) -> path `shouldBe` "pkg-1.0/Data/Vapour/Binding.hsc"
            Left e -> expectationFailure (show e)

    it "a miss carries the modules the sdist does hold" $
        case locateModuleFile world "Data.Vapour.Nonesuch" of
            Left (NoSuchModule present) -> do
                present `shouldSatisfy` elem "Data.Vapour.Cloud"
                present `shouldSatisfy` elem "Data.Vapour.Lit"
            other -> expectationFailure (show (fmap fst other))

    it "corrupt bytes are an error, not an exception" $
        case locateModuleFile (BL.take 40 world) "Data.Vapour.Cloud" of
            Left (BadArchive _) -> pure ()
            other -> expectationFailure (show (fmap fst other))

    describe "decompressCapped" $ do
        it "reports output past the cap as a bad archive" $ do
            let bomb = GZip.compress (BLC.replicate 4096 'a')
            case decompressCapped 1024 bomb of
                Left (BadArchive e) -> e `shouldSatisfy` T.isInfixOf "guard"
                other -> expectationFailure (show other)
        it "passes output under the cap through whole" $ do
            let small = GZip.compress (BLC.replicate 512 'a')
            fmap BL.length (decompressCapped 1024 small)
                `shouldBe` Right 512

    describe "unlit" $ do
        it "keeps bird-track code and blanks prose, same line count" $ do
            let src = "prose\n> a = 1\nmore prose\n> b = 2"
            unlit src `shouldBe` "\na = 1\n\nb = 2\n"
        it "treats a bare > as an empty code line" $
            unlit ">" `shouldBe` "\n"
        it "keeps a bird track with no following space" $
            unlit ">code" `shouldBe` "code\n"
        it "leaves the line count unchanged for any input" $ do
            let src = "x\n> y\nz\n"
            length (T.lines (unlit src)) `shouldBe` length (T.lines src)
  where
    segments = T.splitOn "/" . T.pack
