{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | RESULT→USE bridge for @search_capability@: once the SHIP helper names a
candidate PACKAGE, enrich it with the key API the model must actually CALL — a
handful of its exported functions with signatures and the module to import. A
discovery that returns only a package name is half a tool (the qr task found
@qrcode-core@ but never the encoder, so it failed).

The enrichment runs HERE in Haskell (the search side already shells to hoogle
for its fallback). For each package we query the LOCAL hoogle DB filtered to
that package — @hoogle search "+\<pkg\> \<term\>"@ — once per salient query
keyword, union the results, then rank them: value/function items first, scored
by how many query keywords appear in the name/type/docs. So "generate a QR code"
surfaces the encoder, not data-type accessors. The local hoogle ANDs every word
in a query, so a multi-word @+pkg generate a qr code@ matches nothing; querying
one keyword at a time and unioning is what makes the encoder surface.

No network: the public hoogle API is never consulted. On any failure (binary
absent, no database, empty output) a package simply gets an empty @api@ list, so
the tool degrades to the prior package-only behaviour.
-}
module Sabela.AI.Capabilities.CapabilityApi (
    ApiFn (..),
    apiKeywords,
    rankApiFns,
    isValueItem,
    enrichPackages,
    enrichPackageApi,
    usageExample,
    splitArrow,
) where

import Control.Exception (SomeException, try)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Sabela.AI.HoogleResolve (
    HoogleHit (..),
    denoise,
    keywords,
    parseHoogleBlob,
 )

{- | One enriched API entry: a callable export of a discovered package — its
name, the module to import it from, and its type signature. This is what the
model needs to write the cell.
-}
data ApiFn = ApiFn
    { afName :: Text
    , afModule :: Text
    , afType :: Text
    }
    deriving (Eq, Show)

{- | The salient keywords to rank a package's exports by: the denoised query's
keywords (stop-words + language/meta noise removed). Empty query yields none.
-}
apiKeywords :: Text -> [Text]
apiKeywords q
    | T.null (T.strip q) = []
    | otherwise = nub (map T.toLower (keywords (denoise q)))

{- | A hoogle @item@ is a VALUE/function (vs a data/type/class/module/package
declaration) when it carries a @::@ signature. The query implying an action, we
surface these first.
-}
isValueItem :: HoogleHit -> Bool
isValueItem h = not (T.null (T.strip (hhType h)))

{- | Rank a package's candidate exports best-first and keep the top @k@.
Value/function items sort before data/type/class items; within each, more
query-keyword overlap (name, then type, then docs) wins; shorter names break
ties. Deduplicated by (name, module). Items with no name are dropped.
-}
rankApiFns :: Int -> [Text] -> [HoogleHit] -> [ApiFn]
rankApiFns k kws hits =
    take (max 0 k) (nubFns (map toFn (sortOn rankKey keep)))
  where
    keep = filter (not . T.null . hhName) hits
    rankKey h =
        ( not (isValueItem h)
        , Down (overlap h)
        , T.length (hhName h)
        , hhName h
        )
    overlap h =
        let hay = T.toLower (T.intercalate " " [hhName h, hhType h, hhDocs h])
         in length (filter (`T.isInfixOf` hay) kws)
    toFn h = ApiFn (hhName h) (hhModule h) (hhType h)
    nubFns = nubOnKey (\a -> (afName a, afModule a))

-- | Order-preserving dedup keyed by a projection.
nubOnKey :: (Eq b) => (a -> b) -> [a] -> [a]
nubOnKey f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs

{- | Enrich each package (the SHIP helper hits, as @(package, synopsis)@) with up
to @perPkg@ key exports, querying only the top @nPkgs@ to stay concise. Each
returns @(package, synopsis, [ApiFn])@; a package whose API does not surface gets
an empty list.
-}
enrichPackages ::
    Int -> Int -> Text -> [(Text, Text)] -> IO [(Text, Text, [ApiFn])]
enrichPackages nPkgs perPkg query pkgs = do
    let kws = apiKeywords query
        (top, rest) = splitAt (max 0 nPkgs) pkgs
    enriched <- mapM (enrichOne kws) top
    pure (enriched ++ map (\(p, s) -> (p, s, [])) rest)
  where
    enrichOne kws (p, s) = do
        api <- enrichPackageApi perPkg kws p
        pure (p, s, api)

{- | Query the LOCAL hoogle DB filtered to one package for its key exports,
ranked by the query keywords. Runs @hoogle search "+\<pkg\> \<kw\>"@ once per
keyword (the local hoogle ANDs words, so one term at a time is what surfaces
hits), unions the parsed hits, and ranks. Falls back to the package's top
exports under a generic term when keyword queries find nothing. Empty on any
failure — no network.
-}
enrichPackageApi :: Int -> [Text] -> Text -> IO [ApiFn]
enrichPackageApi perPkg kws pkg
    | T.null (T.strip pkg) = pure []
    | otherwise = do
        bin <- fromMaybe "hoogle" <$> lookupEnv "SABELA_HOOGLE_BIN"
        db <- lookupEnv "SABELA_HOOGLE_DB"
        let runKw = hoogleFor bin db pkg
        kwHits <- concat <$> mapM runKw queryTerms
        hits <-
            if null kwHits
                then concat <$> mapM runKw fallbackTerms
                else pure kwHits
        pure (rankApiFns perPkg kws (onlyPkg hits))
  where
    queryTerms = take 5 kws
    fallbackTerms = ["encode", "new", "run", "make", "to", "from"]
    onlyPkg = filter (\h -> hhPackage h == pkg)

{- | Synthesize a short, paste-able usage skeleton for the top exported function:
the import line plus a @let@ binding that calls it with one typed-hole argument
per parameter, so the model sees HOW to wire the function, not just its name.
Empty when there is no usable export. Example for
@encode :: QRCodeOptions -> Text -> Maybe QRImage@ in @Codec.QRCode@:

> import Codec.QRCode (encode)
> -- encode :: QRCodeOptions -> Text -> Maybe QRImage
> let result = encode (_ :: QRCodeOptions) (_ :: Text)
-}
usageExample :: [ApiFn] -> Text
usageExample [] = ""
usageExample (f : _)
    | T.null (T.strip (afName f)) = ""
    | otherwise =
        T.intercalate "\n" (importLine : sigLine : [callLine])
  where
    importLine
        | T.null (T.strip (afModule f)) = "-- import the module above"
        | otherwise = "import " <> afModule f <> " (" <> afName f <> ")"
    sigLine
        | T.null (T.strip (afType f)) = "-- " <> afName f
        | otherwise = "-- " <> afName f <> " :: " <> afType f
    callLine = "let result = " <> T.unwords (afName f : args)
    args = map hole (argTypes (afType f))
    hole t = "(_ :: " <> t <> ")"

{- | The argument types of a signature: drop any @=>@ context and @forall@
prefix, split the remainder on top-level @->@, and keep all but the final
(result) segment. A signature with no arrows (a plain value) yields no args.
-}
argTypes :: Text -> [Text]
argTypes ty =
    case splitArrow (stripContext ty) of
        [] -> []
        segs -> map T.strip (init segs)

{- | Drop a leading @forall … .@ quantifier and a top-level @=>@ class context,
leaving the bare function type. Conservative: only strips a @=>@ that is at
bracket depth zero.
-}
stripContext :: Text -> Text
stripContext ty =
    afterCtx (dropForall (T.strip ty))
  where
    dropForall t
        | "forall" `T.isPrefixOf` t =
            case T.breakOn "." t of
                (_, rest) | not (T.null rest) -> T.strip (T.drop 1 rest)
                _ -> t
        | otherwise = t
    afterCtx t = case breakTopLevel "=>" t of
        Just (_, rhs) -> T.strip rhs
        Nothing -> t

{- | Split a type on top-level @->@ (bracket depth zero), so nested arrows inside
@(a -> b)@ or @[a -> b]@ stay intact. Returns the segments in order.
-}
splitArrow :: Text -> [Text]
splitArrow = go 0 "" []
  where
    flush acc segs = reverse (acc : segs)
    go :: Int -> Text -> [Text] -> Text -> [Text]
    go _ acc segs t
        | T.null t = reverse (acc : segs)
    go d acc segs t
        | "->" `T.isPrefixOf` t && d == 0 = go d "" (acc : segs) (T.drop 2 t)
        | otherwise =
            let c = T.head t
                d' = d + delta c
             in go d' (T.snoc acc c) segs (T.tail t)
    delta c
        | c `elem` ("([{" :: String) = 1
        | c `elem` (")]}" :: String) = -1
        | otherwise = 0

{- | Find a top-level (bracket depth zero) occurrence of a separator and split
once on it. Used for the @=>@ context split.
-}
breakTopLevel :: Text -> Text -> Maybe (Text, Text)
breakTopLevel sep = go (0 :: Int) ""
  where
    go _ _ t | T.null t = Nothing
    go d acc t
        | d == 0 && sep `T.isPrefixOf` t =
            Just (acc, T.drop (T.length sep) t)
        | otherwise =
            let c = T.head t
             in go (d + delta c) (T.snoc acc c) (T.tail t)
    delta c
        | c `elem` ("([{" :: String) = 1
        | c `elem` (")]}" :: String) = -1
        | otherwise = 0

{- | One @hoogle search "+\<pkg\> \<term\>" --json@ run, parsed. The @+pkg@
prefix restricts to the package; a non-clean or empty exit yields no hits.
-}
hoogleFor :: FilePath -> Maybe FilePath -> Text -> Text -> IO [HoogleHit]
hoogleFor bin db pkg term = do
    let dbArg = maybe [] (\p -> ["--database=" ++ p]) db
        q = "+" ++ T.unpack pkg ++ " " ++ T.unpack term
        args = ["search", "--count=12", "--json"] ++ dbArg ++ [q]
    r <- try (readProcessWithExitCode bin args "")
    pure $ case r of
        Left (_ :: SomeException) -> []
        Right (ExitSuccess, out, _)
            | not (null out) -> parseHoogleBlob (T.pack out)
        Right _ -> []
