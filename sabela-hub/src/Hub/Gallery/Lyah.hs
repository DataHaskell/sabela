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

assetBase :: Text
assetBase =
    "https://raw.githubusercontent.com/learnyouahaskell/learnyouahaskell.github.io/main/assets/"

attribution :: Text
attribution =
    "Adapted from **Learn You a Haskell for Great Good!** by Miran Lipova\269a, "
        <> "licensed under [CC BY-NC-SA 3.0](https://creativecommons.org/licenses/by-nc-sa/3.0/). "
        <> "This Sabela port preserves that license."

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

-- | Convert every chapter from @srcDir\/\<slug\>.md@ to @outDir\/NN-\<slug\>.md@.
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
finalizeNotebook :: Text -> (Text, Int)
finalizeNotebook txt =
    let (chunks, n) = go (splitChunks (T.lines txt))
     in (T.intercalate "\n\n" chunks, n)
  where
    go [] = ([], 0)
    go (c : cs) =
        let (rest, n) = go cs
         in case c of
                HaskellCell code (Just out)
                    | isErrorOutput out -> (toStatic code : rest, n + 1)
                    | otherwise -> (renderCell code out : rest, n)
                HaskellCell code Nothing -> (fence "haskell" code : rest, n)
                Verbatim t -> (t : rest, n)
    toStatic = fence "text"
    renderCell code out = fence "haskell" code <> "\n\n" <> out
    fence lang code = "```" <> lang <> "\n" <> code <> "\n```"

-- | A parsed notebook chunk: a haskell cell (+ optional output), or raw text.
data Chunk
    = HaskellCell Text (Maybe Text)
    | Verbatim Text

{- | Split a notebook into chunks, pairing each @haskell@ fence with a following
@\>@ output block (if any). Prose and other fences pass through verbatim.
-}
splitChunks :: [Text] -> [Chunk]
splitChunks = goProse []
  where
    goProse acc [] = flushProse acc []
    goProse acc (l : ls)
        | l == "```haskell" =
            let (code, rest) = break isFenceClose ls
                afterFence = drop 1 rest
                (out, rest') = grabOutput afterFence
             in flushProse acc (HaskellCell (T.intercalate "\n" code) out : goProse [] rest')
        | otherwise = goProse (l : acc) ls
    flushProse [] cs = cs
    flushProse acc cs =
        let t = T.intercalate "\n" (reverse acc)
         in if T.null (T.strip t) then cs else Verbatim t : cs
    -- A quoted output block follows after blank lines.
    grabOutput ls =
        let (blanks, rest) = span (T.null . T.strip) ls
         in case rest of
                (q : _)
                    | ">" `T.isPrefixOf` q ->
                        let (qs, after) = span (">" `T.isPrefixOf`) rest
                         in (Just (T.intercalate "\n" qs), after)
                _ -> (Nothing, blanks ++ rest)

isErrorOutput :: Text -> Bool
isErrorOutput out = any (`T.isInfixOf` out) signals
  where
    signals =
        [ "error:"
        , "<interactive>"
        , "Not in scope"
        , "not in scope"
        , "Could not load module"
        , "Couldn't match"
        , "No instance"
        , "parse error"
        , "cannot construct"
        , "rigid type"
        , "Ambiguous"
        , "hidden package"
        , "Variable not in scope"
        ]

-- ---------------------------------------------------------------------------
-- Chapter conversion
-- ---------------------------------------------------------------------------

{- | Convert one chapter's Markdown, prepending the title + attribution header
and (when the chapter imports beyond @base@) a @-- cabal:@ setup cell.
-}
convertChapter :: Text -> Text -> Text
convertChapter title md =
    T.intercalate "\n\n" (header : setup ++ blocks (T.lines body))
  where
    body = dropDeprecated (dropFrontmatter md)
    header = "# " <> title <> "\n\n" <> attribution
    setup =
        [ "```haskell\n-- cabal: build-depends: base, "
            <> T.intercalate ", " extras
            <> "\n```"
        | not (null extras)
        ]
    extras = neededDeps body

{- | Packages a chapter needs beyond @base@, inferred from the modules it
mentions. Over-inclusion (a module named only in prose) is harmless — these are
all common\/boot packages.
-}
neededDeps :: Text -> [Text]
neededDeps body = dedup [pkg | (needle, pkg) <- table, needle `T.isInfixOf` body]
  where
    table =
        [ ("Data.Map", "containers")
        , ("Data.Set", "containers")
        , ("System.Random", "random")
        , ("Control.Monad.State", "mtl")
        , ("Control.Monad.Writer", "mtl")
        , ("Control.Monad.Reader", "mtl")
        , ("System.Directory", "directory")
        ]
    dedup = foldr (\x acc -> x : filter (/= x) acc) []

{- | Drop @import Control.Monad.Instances@: those instances (e.g. @Functor
((->) r)@) moved into @base@ and the module no longer exists.
-}
dropDeprecated :: Text -> Text
dropDeprecated =
    T.unlines
        . filter ((/= "import Control.Monad.Instances") . T.strip)
        . T.lines

{- | The frontmatter title (@title: "…"@), if present. Exposed for tests; the
driver uses the curated 'lcTitle' instead.
-}
chapterFrontTitle :: Text -> Maybe Text
chapterFrontTitle md = case T.lines md of
    ("---" : rest) ->
        let inFront = takeWhile (/= "---") rest
         in case [v | l <- inFront, Just v <- [T.stripPrefix "title:" l]] of
                (v : _) -> Just (unquote (T.strip v))
                [] -> Nothing
    _ -> Nothing
  where
    unquote = T.dropAround (== '"')

dropFrontmatter :: Text -> Text
dropFrontmatter md = case T.lines md of
    ("---" : rest) -> T.unlines (drop 1 (dropWhile (/= "---") rest))
    _ -> md

-- ---------------------------------------------------------------------------
-- Block machine
-- ---------------------------------------------------------------------------

-- | Walk lines, emitting converted Markdown chunks (prose runs, code cells).
blocks :: [Text] -> [Text]
blocks [] = []
blocks (l : ls)
    | Just lang <- fenceOpen l =
        let (code, rest) = break isFenceClose ls
         in codeChunks lang code ++ blocks (drop 1 rest)
    | T.null (T.strip l) = blocks ls
    | otherwise =
        let (prose, rest) = break (\x -> isFenceLine x || T.null (T.strip x)) ls
            chunk = proseBlock (l : prose)
         in [chunk | not (T.null (T.strip chunk))] ++ blocks rest

isFenceLine :: Text -> Bool
isFenceLine l = "```" `T.isPrefixOf` l

isFenceClose :: Text -> Bool
isFenceClose l = T.strip l == "```"

-- | An opening fence @```{…}@ → its raw attribute string (between the braces).
fenceOpen :: Text -> Maybe Text
fenceOpen l = do
    rest <- T.stripPrefix "```" (T.stripStart l)
    let s = T.strip rest
    if "{" `T.isPrefixOf` s
        then Just (T.dropAround (`elem` ("{}" :: String)) s)
        else Nothing

-- ---------------------------------------------------------------------------
-- Prose
-- ---------------------------------------------------------------------------

{- | Convert a prose run: drop fenced-div markers, strip Pandoc attributes,
repoint image paths. Each line is transformed independently.
-}
proseBlock :: [Text] -> Text
proseBlock =
    T.intercalate "\n"
        . map (rewriteImages . stripPandocAttrs)
        . filter (not . isDivMarker)

isDivMarker :: Text -> Bool
isDivMarker l = ":::" `T.isPrefixOf` T.strip l

rewriteImages :: Text -> Text
rewriteImages = T.replace "](assets/" ("](" <> assetBase)

{- | Drop Pandoc attribute braces — heading anchors @{#…}@, image\/span
attributes @{.…}@ — anywhere on the line. Leaves ordinary prose braces alone
(only braces whose first char is @.@ or @#@ are removed). Trailing space left by
a removed brace is trimmed.
-}
stripPandocAttrs :: Text -> Text
stripPandocAttrs = T.stripEnd . go
  where
    go t = case T.breakOn "{" t of
        (before, rest)
            | T.null rest -> t
            | otherwise -> case T.uncons (T.drop 1 rest) of
                Just (c, _)
                    | c == '.' || c == '#' ->
                        let (_, after) = T.breakOn "}" rest
                         in before <> go (T.drop 1 after)
                _ -> before <> "{" <> go (T.drop 1 rest)

-- ---------------------------------------------------------------------------
-- Code
-- ---------------------------------------------------------------------------

{- | Convert a fenced code block by its Pandoc attribute string into one or more
notebook chunks.
-}
codeChunks :: Text -> [Text] -> [Text]
codeChunks attrs code
    | isGhci attrs = ghciChunks code
    | isPlain attrs = [staticBlock code]
    | otherwise = [haskellCell (T.intercalate "\n" code)]

isGhci :: Text -> Bool
isGhci attrs = "ghci" `T.isInfixOf` attrs

isPlain :: Text -> Bool
isPlain attrs = "plain" `T.isInfixOf` attrs && not ("ghci" `T.isInfixOf` attrs)

haskellCell :: Text -> Text
haskellCell body = "```haskell\n" <> body <> "\n```"

staticBlock :: [Text] -> Text
staticBlock code = "```text\n" <> T.intercalate "\n" code <> "\n```"

-- ---------------------------------------------------------------------------
-- GHCi transcripts
-- ---------------------------------------------------------------------------

{- | Split a GHCi transcript into chunks: one runnable @haskell@ cell per
expression input, with meta-commands and intentional-error examples preserved as
static @text@ blocks. The banner and bare prompts are dropped.
-}
ghciChunks :: [Text] -> [Text]
ghciChunks = go
  where
    go [] = []
    go (l : ls)
        | isPrompt l =
            let input = stripPrompt l
                (out, rest) = break isPrompt ls
             in classify l input out ++ go rest
        | otherwise = go ls -- banner / stray output before the first prompt
    classify raw input out
        | T.null (T.strip input) = [] -- bare "ghci>"
        | isMeta input || looksLikeError out = [staticBlock (raw : out)]
        | otherwise = [haskellCell input]

isPrompt :: Text -> Bool
isPrompt l = "ghci>" `T.isPrefixOf` T.stripStart l

stripPrompt :: Text -> Text
stripPrompt l = T.strip (T.drop (T.length "ghci>") (T.stripStart l))

isMeta :: Text -> Bool
isMeta = T.isPrefixOf ":"

looksLikeError :: [Text] -> Bool
looksLikeError = any (\o -> any (`T.isInfixOf` o) signals)
  where
    signals =
        [ "error:"
        , "<interactive>"
        , "Couldn't match"
        , "No instance"
        , "parse error"
        , "Not in scope"
        , "not in scope"
        , "cannot construct"
        , "rigid type variable"
        ]
