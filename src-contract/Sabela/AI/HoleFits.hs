{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.HoleFits (
    HoleFit (..),
    dropProvenance,
    packageQualifiers,
    parseHoleFits,
    qualifiedNames,
    refinementFits,
    stripPackageQualifiers,
) where

import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Health (namesHarnessBinder)

data HoleFit = HoleFit
    { hfWrite :: Text
    , hfType :: Text
    , hfRefined :: Bool
    , hfModule :: Maybe Text
    , hfAppliedAt :: [Text]
    }
    deriving (Eq, Show)

{- | GHC emits diagnostics as JSON under -fdiagnostics-as-json, so the fit list
arrives as one string with escaped newlines. Splitting that on real newlines
yields a single element and every fit is lost.
-}
unescapeDiagnostic :: Text -> Text
unescapeDiagnostic blob
    | "\\n" `T.isInfixOf` blob = T.replace "\\\"" "\"" (T.replace "\\n" "\n" blob)
    | otherwise = blob

{- | GHC offers every name in scope, including the wrappers the harness put
there; writing one back would name code the model never wrote.
-}
parseHoleFits :: Text -> [HoleFit]
parseHoleFits raw = filter (not . namesHarnessBinder . hfWrite) (allFits raw)

allFits :: Text -> [HoleFit]
allFits raw = case afterValidHeader (T.lines blob) of
    [] -> []
    body ->
        let (plainLs, refLs) = break isRefinementHeader body
         in plainFits plainLs ++ refinementSkeletons (drop 1 refLs)
  where
    blob = unescapeDiagnostic raw

afterValidHeader :: [Text] -> [Text]
afterValidHeader = drop 1 . dropWhile (not . isValidHeader)

isValidHeader :: Text -> Bool
isValidHeader = T.isInfixOf "Valid hole fits include"

isRefinementHeader :: Text -> Bool
isRefinementHeader = T.isInfixOf "Valid refinement hole fits include"

plainFits :: [Text] -> [HoleFit]
plainFits [] = []
plainFits (l : ls)
    | isEntryStart l =
        let (cont, rest) = span isTypeContinuation ls
            sig = T.unwords (map T.strip (l : cont))
            prov = takeWhile isProvenance rest
            modu = importedModule prov
            applied = appliedTypes prov
         in maybe
                id
                (\(n, t) -> (HoleFit n t False modu applied :))
                (splitNameType sig)
                (plainFits rest)
    | otherwise = plainFits ls
  where
    isEntryStart x = "::" `T.isInfixOf` x && not (isProvenance x)

{- | GHC quotes the module with smart quotes or with a backtick pair depending
on the rendering, and the JSON diagnostics use the backtick form.
-}
importedModule :: [Text] -> Maybe Text
importedModule ls =
    case [ m | l <- ls, (open, close) <- quoteStyles, Just m <- [afterMarker open close l]
         ] of
        (m : _) | not (T.null m) -> Just m
        _ -> Nothing
  where
    quoteStyles = [('\8216', '\8217'), ('`', '\'')]
    afterMarker open close l =
        let marker = "(imported from " <> T.singleton open
            (_, r) = T.breakOn marker (T.strip l)
         in if T.null r
                then Nothing
                else Just (T.takeWhile (/= close) (T.drop (T.length marker) r))

{- | The @f \@T@ line GHC prints under each fit. It is the only thing that
separates a constraint discharged at the goal's own type from one discharged
at a function type, so it must survive parsing.
-}
appliedTypes :: [Text] -> [Text]
appliedTypes ls =
    case [T.strip r | l <- ls, Just r <- [T.stripPrefix "with " (T.strip l)]] of
        (w : _) -> splitApplications (T.dropWhile (/= '@') w)
        [] -> []

splitApplications :: Text -> [Text]
splitApplications t
    | T.null t = []
    | otherwise = case T.uncons t of
        Just ('@', rest) ->
            let (arg, more) = balancedArg rest
             in [arg | not (T.null arg)] <> splitApplications (T.stripStart more)
        _ -> splitApplications (T.dropWhile (/= '@') t)

balancedArg :: Text -> (Text, Text)
balancedArg s = case T.uncons (T.stripStart s) of
    Just ('(', _) -> spanBalanced (T.stripStart s)
    _ -> T.break (== ' ') (T.stripStart s)
  where
    spanBalanced t = go (0 :: Int) 0 t
      where
        go d i u = case T.uncons u of
            Nothing -> (T.take i t, "")
            Just (c, rest)
                | c == '(' -> go (d + 1) (i + 1) rest
                | c == ')' ->
                    if d == 1 then (T.take (i + 1) t, rest) else go (d - 1) (i + 1) rest
                | otherwise -> go d (i + 1) rest

refinementSkeletons :: [Text] -> [HoleFit]
refinementSkeletons = map toFit . groupEntries
  where
    toFit (skel, blk) =
        HoleFit
            (T.strip skel)
            (whereType blk)
            True
            (importedModule blk)
            (appliedTypes blk)

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

whereType :: [Text] -> Text
whereType blk =
    typeAfterColon
        ( T.unwords
            (map T.strip (takeWhile (not . stops) (dropWhile (not . isWhere) blk)))
        )
  where
    isWhere x = "where" `T.isPrefixOf` T.strip x
    stops x = any (`T.isPrefixOf` T.strip x) ("with " : provenanceOpeners)
    typeAfterColon s = case T.breakOn "::" s of
        (_, r) | not (T.null r) -> dropProvenance (T.strip (T.drop 2 r))
        _ -> ""

isTypeContinuation :: Text -> Bool
isTypeContinuation x =
    not (blank x) && not ("::" `T.isInfixOf` x) && not (isProvenance x)

isProvenance :: Text -> Bool
isProvenance x =
    any (`T.isPrefixOf` T.strip x) (["with ", "where "] <> provenanceOpeners)

{- | The parenthesised notes GHC writes beside a fit rather than as part of its
type. One list serves the reader that skips such a line and the reader that
ends a type where one begins.
-}
provenanceOpeners :: [Text]
provenanceOpeners = ["(imported", "(defined", "(bound", "(and"]

{- | A fit's type ends where GHC's note about where the name came from begins:
the compiler prints a lone fit and its provenance on one line, so everything
after the @::@ is not a type.
-}
dropProvenance :: Text -> Text
dropProvenance t = T.stripEnd (T.take (go (0 :: Int) 0) t)
  where
    go d i
        | i >= T.length t = i
        | d == 0 && opens (T.drop i t) = i
        | otherwise = go (depth d (T.index t i)) (i + 1)
    opens r = any (`T.isPrefixOf` r) provenanceOpeners
    depth d c
        | c `elem` ("([" :: String) = d + 1
        | c `elem` (")]" :: String) = d - 1
        | otherwise = d

splitNameType :: Text -> Maybe (Text, Text)
splitNameType sig = case T.breakOn "::" sig of
    (name, rest)
        | not (T.null rest) ->
            let n = T.strip name
                t = dropProvenance (T.strip (T.drop 2 rest))
             in if T.null n || T.null t then Nothing else Just (n, t)
    _ -> Nothing

blank :: Text -> Bool
blank = T.null . T.strip

indentOf :: Text -> Int
indentOf = T.length . T.takeWhile (== ' ')

{- | The @pkg-1.2.3:@ prefixes GHC puts on a name it prints from a module the
reader has not imported. They are part of no type the caller could ever write.
-}
packageQualifiers :: Text -> [Text]
packageQualifiers t =
    nub
        [ unit
        | i <- [0 .. T.length t - 1]
        , T.index t i == ':'
        , not ("::" `T.isPrefixOf` T.drop i t)
        , let unit = T.takeWhileEnd nameChar (T.take i t)
        , versioned unit
        ]
  where
    nameChar c = isAlphaNum c || c `elem` ("-_.'" :: String)
    versioned u = case reverse (T.splitOn "-" u) of
        (v : _ : _) -> not (T.null v) && T.all (\c -> isDigit c || c == '.') v
        _ -> False

-- | The same text with every package qualifier removed.
stripPackageQualifiers :: Text -> Text
stripPackageQualifiers t =
    foldr (\q -> T.replace (q <> ":") "") t (packageQualifiers t)

{- | Every name a text carries through a module path, as that module and the
plain name inside it. GHC writes such a name @pkg-1.2.3:Some.Module.entity@ or
@Some.Module.entity@; the package qualifier is a separate run either way.
-}
qualifiedNames :: Text -> [(Text, Text)]
qualifiedNames t =
    nub
        [ (T.intercalate "." (init parts), last parts)
        | run <- T.groupBy same t
        , T.any isAlphaNum run
        , T.all pathChar run
        , let parts = T.splitOn "." run
        , length parts > 1
        , all moduleSegment (init parts)
        , not (T.null (last parts))
        ]
  where
    pathChar c = isAlphaNum c || c `elem` ("_.'" :: String)
    same a b = pathChar a == pathChar b
    moduleSegment s = maybe False (isUpper . fst) (T.uncons s)

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
    balancedPrefix = go (0 :: Int) ""
      where
        go d acc s = case T.uncons s of
            Nothing -> Nothing
            Just (c, rest)
                | c == ')' && d == 0 -> Just (T.pack (reverse acc))
                | c `elem` ("([" :: String) -> go (d + 1) (c : acc) rest
                | c `elem` (")]" :: String) -> go (d - 1) (c : acc) rest
                | otherwise -> go d (c : acc) rest
