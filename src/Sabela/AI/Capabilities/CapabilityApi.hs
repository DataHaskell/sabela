{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.CapabilityApi (
    ApiFn (..),
    PackageApi (..),
    apiKeywords,
    rankApiFns,
    isValueItem,
    enrichPackages,
    enrichPackageApi,
    hoogleFor,
    usageExample,
    splitArrow,
) where

import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (
    breakTopLevel,
    queryAllDbs,
    statesDeclaration,
 )
import Sabela.AI.HoogleResolve (
    HoogleHit (..),
    denoise,
    keywords,
 )

data ApiFn = ApiFn
    { afName :: Text
    , afModule :: Text
    , afType :: Text
    }
    deriving (Eq, Show)

{- | A package's answer to a capability query. 'paVersion' is the release the
index documented, which for a package the caller has not installed need not be
the release they would get; empty when the index states none.
-}
data PackageApi = PackageApi
    { paPackage :: Text
    , paSynopsis :: Text
    , paVersion :: Text
    , paApi :: [ApiFn]
    }
    deriving (Eq, Show)

apiKeywords :: Text -> [Text]
apiKeywords q
    | T.null (T.strip q) = []
    | otherwise = nub (map T.toLower (keywords (denoise q)))

{- | Whether an item is something a caller can name in an expression. A
declaration states a type's own shape, so it has no signature to call through
however much text it carries.
-}
isValueItem :: HoogleHit -> Bool
isValueItem h = statesSignature (hhType h)

-- | Whether stated type text is a signature, rather than empty or a shape.
statesSignature :: Text -> Bool
statesSignature t = not (T.null (T.strip t)) && not (statesDeclaration t)

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

nubOnKey :: (Eq b) => (a -> b) -> [a] -> [a]
nubOnKey f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs

enrichPackages ::
    Int -> Int -> Text -> [(Text, Text)] -> IO [PackageApi]
enrichPackages nPkgs perPkg query pkgs = do
    let kws = apiKeywords query
        (top, rest) = splitAt (max 0 nPkgs) pkgs
    enriched <- mapM (enrichOne kws) top
    pure (enriched ++ map (\(p, s) -> PackageApi p s "" []) rest)
  where
    enrichOne kws (p, s) = do
        (api, ver) <- enrichPackageApi perPkg kws p
        pure (PackageApi p s ver api)

-- | A package's callable API, with the release the index documented it from.
enrichPackageApi :: Int -> [Text] -> Text -> IO ([ApiFn], Text)
enrichPackageApi perPkg kws pkg
    | T.null (T.strip pkg) = pure ([], "")
    | otherwise = do
        let runKw = hoogleFor pkg
        kwHits <- concat <$> mapM runKw queryTerms
        hits <-
            if null kwHits
                then concat <$> mapM runKw fallbackTerms
                else pure kwHits
        let mine = onlyPkg hits
        pure (rankApiFns perPkg kws mine, statedVersion mine)
  where
    queryTerms = take 5 kws
    fallbackTerms = ["encode", "new", "run", "make", "to", "from"]
    onlyPkg = filter (\h -> hhPackage h == pkg)
    statedVersion hs = case filter (not . T.null) (map hhVersion hs) of
        (v : _) -> v
        [] -> ""

{- | A call skeleton for the first export a caller can name in an expression.
A declaration carries no signature to call through, so it is passed over
rather than dressed as one.
-}
usageExample :: [ApiFn] -> Text
usageExample fns = case filter callable fns of
    (f : _) -> T.intercalate "\n" [importLine f, sigLine f, callLine f]
    [] -> ""
  where
    callable f =
        not (T.null (T.strip (afName f)))
            && not (statesDeclaration (afType f))
    importLine f
        | T.null (T.strip (afModule f)) = "-- import the module above"
        | otherwise = "import " <> afModule f <> " (" <> afName f <> ")"
    sigLine f
        | T.null (T.strip (afType f)) = "-- " <> afName f
        | otherwise = "-- " <> afName f <> " :: " <> afType f
    callLine f = "let result = " <> T.unwords (afName f : args f)
    args f = map hole (argTypes (afType f))
    hole t = "(_ :: " <> t <> ")"

argTypes :: Text -> [Text]
argTypes ty =
    case splitArrow (stripContext ty) of
        [] -> []
        segs -> map T.strip (init segs)

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

splitArrow :: Text -> [Text]
splitArrow = go 0 "" []
  where
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

{- | A package's hits for one term, across every database in reach. Naming one
database holds the answer to hoogle's own index, which symbol-indexes Stackage
members only, so a package off it is found and then described as having no API.
-}
hoogleFor :: Text -> Text -> IO [HoogleHit]
hoogleFor pkg term =
    queryAllDbs
        ["search", "--count=12", "--json", "+" ++ T.unpack pkg ++ " " ++ T.unpack term]
