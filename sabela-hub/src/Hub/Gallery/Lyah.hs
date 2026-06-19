{-# LANGUAGE OverloadedStrings #-}

{- | Convert a "Learn You a Haskell" chapter (Pandoc Markdown) into a runnable
Sabela notebook. The book is CC BY-NC-SA 3.0 (Miran Lipova\269a); each converted
notebook carries that attribution.

Conversions: strip YAML frontmatter (keep the title); @{.haskell:hs}@ source
blocks become runnable cells; @{.haskell:ghci}@ transcripts split one runnable
cell per @ghci\>@ expression (prompt stripped, transcript output discarded — it
is repopulated by running the notebook through scripths); GHCi meta-commands
(@:t@\/@:k@\/…) and intentional-error examples are kept verbatim as static
@text@ blocks; @{.plain}@ blocks become static @text@; Pandoc heading anchors,
labelled spans, fenced @hintbox@ divs are stripped; image paths are repointed at
the book's public assets.
-}
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

-- | A chapter's identity: ordinal, source\/output slug, title, gallery tags.
data LyahChapter = LyahChapter
    { lcNum :: Int
    , lcSlug :: Text
    , lcTitle :: Text
    , lcTags :: [Text]
    }

-- | The 14 chapters in reading order. The single source of chapter identity.
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

{- | Finalize every notebook in @dir@ in place: downgrade any cell scripths
could not run (its output is a GHC error) to a clean static @text@ block,
dropping the error output.
-}
finalizeAll :: FilePath -> IO ()
finalizeAll dir = do
    files <- filter ((== ".md") . takeExtension) <$> listDirectory dir
    forM_ files $ \f -> do
        let p = dir </> f
        txt <- TIO.readFile p
        let (out, n) = finalizeNotebook txt
        TIO.writeFile p out
        putStrLn ("finalized " <> f <> " (" <> show n <> " cells downgraded)")

{- | Rewrite a run notebook: a @haskell@ cell whose output block is a GHC error
becomes a static @text@ block (code preserved, error output dropped) so a forked
chapter runs top-to-bottom cleanly. The book's intentional-error examples are
already static text from conversion, so those stay visible. Returns the new text
and the number of cells downgraded.
-}
