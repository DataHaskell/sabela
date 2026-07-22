{-# LANGUAGE OverloadedStrings #-}

{- | Parse GHC's valid-hole-fits output into structured fits.

A typed hole (@_ :: T@ for producers of @T@, or @_ x@ for consumers of @x@'s
type) makes GHC report two sections: @Valid hole fits include@, a list of
in-scope @name :: type@ that fit, and @Valid refinement hole fits include@,
partial applications such as @foldl1' (_ :: Int -> Int -> Int)@ that fit once
their own holes are filled. The plain fits are names the model writes directly;
the refinement fits are typed skeletons it completes. We keep both: 'hfWrite' is
what to write at the hole, 'hfRefined' marks the skeletons.

Pinned to a captured real GHC 9.12 blob (see @Test.HoleFitsSpec@); the output
format is version-sensitive, so a large unexpected diff signals the format moved.
-}
module Sabela.AI.HoleFits (
    HoleFit (..),
    parseHoleFits,
    refinementFits,
) where

import Data.Text (Text)
import qualified Data.Text as T

{- | One hole fit. 'hfWrite' is what the model writes at the hole: a bare name
for a plain fit, an application skeleton with typed holes for a refinement fit.
'hfType' is the underlying function's signature. 'hfRefined' is 'True' for the
hole-bearing skeletons.
-}
data HoleFit = HoleFit
    { hfWrite :: Text
    , hfType :: Text
    , hfRefined :: Bool
    }
    deriving (Eq, Show)

-- | Plain fits then refinement skeletons; @[]@ when the blob has no fits header.
parseHoleFits :: Text -> [HoleFit]
parseHoleFits blob = case afterValidHeader (T.lines blob) of
    [] -> []
    body ->
        let (plainLs, refLs) = break isRefinementHeader body
         in plainFits plainLs ++ refinementSkeletons (drop 1 refLs)

afterValidHeader :: [Text] -> [Text]
afterValidHeader = drop 1 . dropWhile (not . isValidHeader)

isValidHeader :: Text -> Bool
isValidHeader = T.isInfixOf "Valid hole fits include"

isRefinementHeader :: Text -> Bool
isRefinementHeader = T.isInfixOf "Valid refinement hole fits include"

{- | Each plain entry starts on the first @::@ line that is not provenance;
deeper type-continuation lines fold into its signature, provenance is skipped.
-}
plainFits :: [Text] -> [HoleFit]
plainFits [] = []
plainFits (l : ls)
    | isEntryStart l =
        let (cont, rest) = span isTypeContinuation ls
            sig = T.unwords (map T.strip (l : cont))
         in maybe id (\(n, t) -> (HoleFit n t False :)) (splitNameType sig) (plainFits rest)
    | otherwise = plainFits ls
  where
    isEntryStart x = "::" `T.isInfixOf` x && not (isProvenance x)

{- | Each refinement entry is the skeleton line at the fit indent; its type
comes from the @where <name> :: <type>@ line in the block beneath it.
-}
refinementSkeletons :: [Text] -> [HoleFit]
refinementSkeletons = map toFit . groupEntries
  where
    toFit (skel, blk) = HoleFit (T.strip skel) (whereType blk) True

{- | Split the section into (entry line, block beneath) pairs: an entry sits at
the shallowest indent, its block is the more-indented lines that follow.
-}
groupEntries :: [Text] -> [(Text, [Text])]
groupEntries lns = case filter (not . blank) lns of
    [] -> []
    (first : _) -> go (indentOf first) (dropWhile blank lns)
  where
    go _ [] = []
    go base (x : xs)
        | blank x = go base xs
        | indentOf x <= base =
            let (blk, rest) = span (\y -> blank y || indentOf y > base) xs
             in (x, blk) : go base rest
        | otherwise = go base xs

-- | The type from a refinement entry's @where … :: …@ line, up to its @with@ line.
whereType :: [Text] -> Text
whereType blk =
    typeAfterColon
        ( T.unwords
            (map T.strip (takeWhile (not . stops) (dropWhile (not . isWhere) blk)))
        )
  where
    isWhere x = "where" `T.isPrefixOf` T.strip x
    stops x = any (`T.isPrefixOf` T.strip x) ["with ", "(imported", "(and "]
    typeAfterColon s = case T.breakOn "::" s of
        (_, r) | not (T.null r) -> T.strip (T.drop 2 r)
        _ -> ""

-- | A wrapped type-signature line: non-blank, no @::@, not provenance.
isTypeContinuation :: Text -> Bool
isTypeContinuation x =
    not (blank x) && not ("::" `T.isInfixOf` x) && not (isProvenance x)

-- | The @with@/@where@/@(imported …)@/@(and …)@ lines GHC appends to a fit.
isProvenance :: Text -> Bool
isProvenance x =
    any (`T.isPrefixOf` T.strip x) ["with ", "where ", "(imported", "(and "]

-- | Split a @name :: type@ signature on its first @::@.
splitNameType :: Text -> Maybe (Text, Text)
splitNameType sig = case T.breakOn "::" sig of
    (name, rest)
        | not (T.null rest) ->
            let n = T.strip name
                t = T.strip (T.drop 2 rest)
             in if T.null n || T.null t then Nothing else Just (n, t)
    _ -> Nothing

blank :: Text -> Bool
blank = T.null . T.strip

indentOf :: Text -> Int
indentOf = T.length . T.takeWhile (== ' ')

{- | GHC's REFINEMENT hole fits — @fn (_ :: ArgTy)@ — as @(fn, ArgTy)@ pairs.
The session queries with @-frefinement-level-hole-fits=2@, so for a
wrong-arity goal GHC names both the right function AND its missing argument's
type (e.g. @takeWhileP (_ :: Maybe String)@). The sub-hole type is captured to
its BALANCED closing paren (qualified names can sit inside nested parens) and
preserved verbatim; the query layer sanitizes.
-}
refinementFits :: Text -> [(Text, Text)]
refinementFits blob = concatMap fitOf afterHeader
  where
    afterHeader =
        drop 1 (dropWhile (not . T.isInfixOf "Valid refinement hole fits") ls)
    ls = T.lines blob
    fitOf l =
        let s = T.strip l
            (nm, rest) = T.breakOn " (_ :: " s
         in [ (nm, ty)
            | not (T.null nm)
            , not (T.any (== ' ') nm)
            , Just ty <- [balancedPrefix (T.drop 7 rest)]
            ]
    -- The chars up to the paren that closes the "(_ :: " opener.
    balancedPrefix = go (0 :: Int) ""
      where
        go d acc s = case T.uncons s of
            Nothing -> Nothing
            Just (c, rest)
                | c == ')' && d == 0 -> Just (T.pack (reverse acc))
                | c `elem` ("([" :: String) -> go (d + 1) (c : acc) rest
                | c `elem` (")]" :: String) -> go (d - 1) (c : acc) rest
                | otherwise -> go d (c : acc) rest
