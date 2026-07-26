{-# LANGUAGE OverloadedStrings #-}

module Hub.Gallery.Lyah.Convert (
    convertChapter,
    finalizeNotebook,
    chapterFrontTitle,
    stripPandocAttrs,
    neededDeps,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Hub.Gallery.Lyah.Code (codeChunks)

assetBase :: Text
assetBase =
    "https://raw.githubusercontent.com/learnyouahaskell/learnyouahaskell.github.io/main/assets/"

attribution :: Text
attribution =
    "Adapted from **Learn You a Haskell for Great Good!** by Miran Lipova\269a, "
        <> "licensed under [CC BY-NC-SA 3.0](https://creativecommons.org/licenses/by-nc-sa/3.0/). "
        <> "This Sabela port preserves that license."

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

data Chunk
    = HaskellCell Text (Maybe Text)
    | Verbatim Text

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

dropDeprecated :: Text -> Text
dropDeprecated =
    T.unlines
        . filter ((/= "import Control.Monad.Instances") . T.strip)
        . T.lines

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

fenceOpen :: Text -> Maybe Text
fenceOpen l = do
    rest <- T.stripPrefix "```" (T.stripStart l)
    let s = T.strip rest
    if "{" `T.isPrefixOf` s
        then Just (T.dropAround (`elem` ("{}" :: String)) s)
        else Nothing

proseBlock :: [Text] -> Text
proseBlock =
    T.intercalate "\n"
        . map (rewriteImages . stripPandocAttrs)
        . filter (not . isDivMarker)

isDivMarker :: Text -> Bool
isDivMarker l = ":::" `T.isPrefixOf` T.strip l

rewriteImages :: Text -> Text
rewriteImages = T.replace "](assets/" ("](" <> assetBase)

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
