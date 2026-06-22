{-# LANGUAGE OverloadedStrings #-}

{- | The Learn You a Haskell gallery collection: its identity and that every
chapter is wired in as a member. Pins the collection the seeder writes to
@/c/1ea40000@ so a chapter dropped from the table (or a renamed cid) is caught.
The per-share WASM-runner splice is exercised by 'Test.RunnerSpec'; here we pin
the collection wiring the seed task delivers.
-}
module Test.SeedSpec (spec) where

import Data.Char (isAscii)
import qualified Data.Text as T
import Hub.Gallery.Lyah (lyahChapterTable)
import Hub.Gallery.Seed (
    Collection (..),
    Curated (..),
    lyahChapters,
    lyahCollection,
 )
import Test.Hspec

spec :: Spec
spec = describe "Hub.Gallery.Seed: Learn You a Haskell collection" $ do
    it "is the collection served at /c/1ea40000" $ do
        colCid lyahCollection `shouldBe` "1ea40000"
        colTitle lyahCollection `shouldBe` "Learn You a Haskell for Great Good!"

    it "groups every chapter in the table as a member" $ do
        length (colMembers lyahCollection) `shouldBe` length lyahChapterTable
        colMembers lyahCollection `shouldBe` map cSlug lyahChapters

    it "numbers the chapter slugs 1ea40001..1ea4000N in reading order" $ do
        let n = length lyahChapterTable
            expected = ["1ea4" <> T.pack (pad4 i) | i <- [1 .. n]]
        map cSlug lyahChapters `shouldBe` expected

    it "sources each chapter from examples/lyah" $
        all (("examples/lyah" `isPrefix`) . cFile) lyahChapters `shouldBe` True

    it "keeps all seeded gallery metadata ASCII" $ do
        let texts =
                colTitle lyahCollection
                    : colDescription lyahCollection
                    : map cAuthor lyahChapters
                        <> map cTitle lyahChapters
        all (T.all isAscii) texts `shouldBe` True
  where
    pad4 i = let s = show i in replicate (4 - length s) '0' <> s
    isPrefix p s = take (length p) s == p
