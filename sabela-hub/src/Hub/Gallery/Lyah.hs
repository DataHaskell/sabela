{-# LANGUAGE OverloadedStrings #-}

module Hub.Gallery.Lyah (
    LyahChapter (..),
    lyahChapterTable,
    convertChapter,
    chapterFrontTitle,
    stripPandocAttrs,
    neededDeps,
    finalizeNotebook,
    convertAll,
    finalizeAll,
) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hub.Gallery.Lyah.Convert (
    chapterFrontTitle,
    convertChapter,
    finalizeNotebook,
    neededDeps,
    stripPandocAttrs,
 )
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeExtension, (</>))

data LyahChapter = LyahChapter
    { lcNum :: Int
    , lcSlug :: Text
    , lcTitle :: Text
    , lcTags :: [Text]
    }

lyahChapterTable :: [LyahChapter]
lyahChapterTable =
    [ LyahChapter 1 "introduction" "Introduction" ["haskell", "tutorial", "intro"]
    , LyahChapter 2 "starting-out" "Starting Out" ["haskell", "tutorial", "basics"]
    , LyahChapter
        3
        "types-and-typeclasses"
        "Types and Typeclasses"
        ["haskell", "types", "typeclasses"]
    , LyahChapter
        4
        "syntax-in-functions"
        "Syntax in Functions"
        ["haskell", "functions", "pattern-matching"]
    , LyahChapter 5 "recursion" "Recursion" ["haskell", "recursion"]
    , LyahChapter
        6
        "higher-order-functions"
        "Higher Order Functions"
        ["haskell", "higher-order", "functions"]
    , LyahChapter 7 "modules" "Modules" ["haskell", "modules"]
    , LyahChapter
        8
        "making-our-own-types-and-typeclasses"
        "Making Our Own Types and Typeclasses"
        ["haskell", "types", "typeclasses"]
    , LyahChapter 9 "input-and-output" "Input and Output" ["haskell", "io"]
    , LyahChapter
        10
        "functionally-solving-problems"
        "Functionally Solving Problems"
        ["haskell", "problem-solving"]
    , LyahChapter
        11
        "functors-applicative-functors-and-monoids"
        "Functors, Applicative Functors and Monoids"
        ["haskell", "functors", "monoids"]
    , LyahChapter 12 "a-fistful-of-monads" "A Fistful of Monads" ["haskell", "monads"]
    , LyahChapter
        13
        "for-a-few-monads-more"
        "For a Few Monads More"
        ["haskell", "monads"]
    , LyahChapter 14 "zippers" "Zippers" ["haskell", "zippers", "data-structures"]
    ]

convertAll :: FilePath -> FilePath -> IO ()
convertAll srcDir outDir = do
    createDirectoryIfMissing True outDir
    forM_ lyahChapterTable $ \ch -> do
        md <- TIO.readFile (srcDir </> T.unpack (lcSlug ch) <> ".md")
        let out = outDir </> pad2 (lcNum ch) <> "-" <> T.unpack (lcSlug ch) <> ".md"
        TIO.writeFile out (convertChapter (lcTitle ch) md)
        putStrLn ("converted " <> T.unpack (lcSlug ch) <> " -> " <> out)
  where
    pad2 n = let s = show n in replicate (2 - length s) '0' <> s

finalizeAll :: FilePath -> IO ()
finalizeAll dir = do
    files <- filter ((== ".md") . takeExtension) <$> listDirectory dir
    forM_ files $ \f -> do
        let p = dir </> f
        txt <- TIO.readFile p
        let (out, n) = finalizeNotebook txt
        TIO.writeFile p out
        putStrLn ("finalized " <> f <> " (" <> show n <> " cells downgraded)")
