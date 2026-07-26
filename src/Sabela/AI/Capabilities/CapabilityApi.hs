{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data ApiFn = ApiFn
    { afName :: Text
    , afModule :: Text
    , afType :: Text
    }
    deriving (Eq, Show)

apiKeywords :: Text -> [Text]
apiKeywords q
    | T.null (T.strip q) = []
    | otherwise = nub (map T.toLower (keywords (denoise q)))

isValueItem :: HoogleHit -> Bool
isValueItem h = not (T.null (T.strip (hhType h)))

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
