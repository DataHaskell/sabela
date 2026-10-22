{-# LANGUAGE OverloadedStrings #-}

{- | Components: a workspace's own cabal package, @components/@, whose
@Components.*@ modules any notebook in the workspace can import. The package
is added to every session the way the support library is, so a notebook
needs only the import. Its cabal file is regenerated from the modules it
holds, so a module is the only thing a person or a tool has to write.
-}
module Sabela.Components (
    componentsDirName,
    componentsPackage,
    componentOverlay,
    Component (..),
    listComponents,
    readComponentSource,
    writeComponent,
    syncComponentsCabal,
    moduleToPath,
    validComponentModule,
    parseHeader,
    Header (..),
    replSupportFor,
    withComponentOverlay,
) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Char (isAlphaNum, isUpper)
import Data.List (isSuffixOf, nub, sort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.FilePath (takeDirectory, takeFileName, (</>))

import Sabela.Session.Project (ReplSupport (..))

componentsDirName :: FilePath
componentsDirName = "components"

componentsPackage :: Text
componentsPackage = "sabela-components"

-- | The package directory to overlay on a session, when the workspace has one.
componentOverlay :: FilePath -> IO (Maybe FilePath)
componentOverlay workDir = do
    let dir = workDir </> componentsDirName
    present <- doesFileExist (dir </> cabalFileName)
    pure (if present then Just dir else Nothing)

cabalFileName :: FilePath
cabalFileName = T.unpack componentsPackage <> ".cabal"

-- | The session's local packages with the workspace's components among them.
withComponentOverlay :: FilePath -> [FilePath] -> IO [FilePath]
withComponentOverlay workDir pkgs = do
    overlay <- componentOverlay workDir
    pure (pkgs ++ [d | Just d <- [overlay], d `notElem` pkgs])

{- | The support a generated project needs: the notebook library, and the
components package when it is among the local packages.
-}
replSupportFor :: [FilePath] -> ReplSupport
replSupportFor pkgs
    | any ((== componentsDirName) . takeFileName) pkgs =
        WithNotebookAnd [componentsPackage]
    | otherwise = WithNotebookSupport

data Component = Component
    { cmModule :: Text
    , cmPath :: FilePath
    -- ^ Relative to the work dir.
    , cmPurpose :: Text
    , cmKind :: Text
    , cmExports :: [Text]
    , cmDeps :: [Text]
    }
    deriving (Eq, Show)

-- | @Components.Retention@ is @Components/Retention.hs@ inside the package.
moduleToPath :: Text -> FilePath
moduleToPath m = T.unpack (T.replace "." "/" m) <> ".hs"

pathToModule :: FilePath -> Maybe Text
pathToModule p
    | ".hs" `isSuffixOf` p =
        Just (T.replace "/" "." (T.pack (take (length p - 3) p)))
    | otherwise = Nothing

-- | A module name under @Components@, every segment a capitalised identifier.
validComponentModule :: Text -> Bool
validComponentModule m = case T.splitOn "." m of
    ("Components" : rest@(_ : _)) -> all segment rest
    _ -> False
  where
    segment s = case T.uncons s of
        Just (c, cs) -> isUpper c && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') cs
        Nothing -> False

-- | Every component module in the workspace, by module name.
listComponents :: FilePath -> IO [Component]
listComponents workDir = do
    let root = workDir </> componentsDirName </> "Components"
    exists <- doesDirectoryExist root
    if not exists
        then pure []
        else do
            files <- walk root
            comps <- forM (sort files) $ \rel -> do
                src <- TIO.readFile (root </> rel)
                pure (component rel src)
            pure (catMaybes comps)
  where
    component rel src = do
        m <- ("Components." <>) <$> pathToModule rel
        let h = parseHeader src
        pure
            Component
                { cmModule = m
                , cmPath = componentsDirName </> "Components" </> rel
                , cmPurpose = hdPurpose h
                , cmKind = hdKind h
                , cmExports = hdExports h
                , cmDeps = hdDeps h
                }

walk :: FilePath -> IO [FilePath]
walk root = go ""
  where
    go rel = do
        entries <- listDirectory (root </> rel)
        fmap concat . forM (sort entries) $ \e -> do
            let relE = if null rel then e else rel </> e
            isDir <- doesDirectoryExist (root </> relE)
            if isDir
                then go relE
                else pure [relE | ".hs" `isSuffixOf` e]

data Header = Header
    { hdPurpose :: Text
    , hdKind :: Text
    , hdExports :: [Text]
    , hdDeps :: [Text]
    }

{- | What a module says about itself: its haddock purpose line, its
@-- Sabela: kind=…@ field, the @-- cabal: build-depends:@ lines it carries
for the package, and its export list.
-}
parseHeader :: Text -> Header
parseHeader src =
    Header
        { hdPurpose = purpose
        , hdKind = fromMaybe "function" (firstJust kindOf ls)
        , hdExports = exportsOf src
        , hdDeps = nub (concatMap depsOf ls)
        }
  where
    ls = T.lines src
    purpose = fromMaybe "" (firstJust purposeOf ls)
    purposeOf l =
        let s = T.strip l
         in case (T.stripPrefix "{- |" s, T.stripPrefix "-- |" s) of
                (Just r, _) -> Just (T.strip (T.replace "-}" "" r))
                (_, Just r) -> Just (T.strip r)
                _ -> Nothing
    kindOf l = T.strip <$> T.stripPrefix "-- Sabela: kind=" (T.strip l)
    depsOf l = case T.stripPrefix "-- cabal: build-depends:" (T.strip l) of
        Just r -> filter (not . T.null) (map T.strip (T.splitOn "," r))
        Nothing -> []
    firstJust f = foldr (\x acc -> f x <|> acc) Nothing

exportsOf :: Text -> [Text]
exportsOf src =
    case T.breakOn "module " src of
        (_, rest)
            | T.null rest -> []
            | otherwise ->
                let header = fst (T.breakOn " where" rest)
                    inside = T.takeWhile (/= ')') (T.drop 1 (T.dropWhile (/= '(') header))
                 in if "(" `T.isInfixOf` header
                        then filter (not . T.null) (map clean (T.splitOn "," inside))
                        else []
  where
    clean = T.strip . T.replace "(..)" "" . T.filter (/= '\n')

readComponentSource :: FilePath -> Text -> IO (Maybe Text)
readComponentSource workDir m
    | not (validComponentModule m) = pure Nothing
    | otherwise = do
        let path = workDir </> componentsDirName </> moduleToPath m
        exists <- doesFileExist path
        if exists then Just <$> TIO.readFile path else pure Nothing

{- | Write a module into the package and bring the cabal file up to date.
Returns the module's path relative to the work dir.
-}
writeComponent :: FilePath -> Text -> Text -> IO FilePath
writeComponent workDir m src = do
    let rel = componentsDirName </> moduleToPath m
        path = workDir </> rel
    createDirectoryIfMissing True (takeDirectory path)
    TIO.writeFile path src
    syncComponentsCabal workDir
    pure rel

-- | The cabal file follows the modules: their names, and the deps they declare.
syncComponentsCabal :: FilePath -> IO ()
syncComponentsCabal workDir = do
    comps <- listComponents workDir
    let dir = workDir </> componentsDirName
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> cabalFileName) (componentsCabal comps)

componentsCabal :: [Component] -> Text
componentsCabal comps =
    T.unlines
        ( [ "cabal-version:      2.4"
          , "name:               " <> componentsPackage
          , "version:            0.1.0.0"
          , "synopsis:           Components extracted from this workspace's notebooks."
          , "build-type:         Simple"
          , ""
          , "library"
          , "    default-language: Haskell2010"
          , "    hs-source-dirs:   ."
          , "    exposed-modules:"
          ]
            ++ ["        " <> cmModule c | c <- comps]
            ++ [ "    build-depends:"
               , "        "
                    <> T.intercalate ",\n        " ("base" : "sabela-notebook" : extraDeps)
               ]
        )
  where
    extraDeps = filter (`notElem` ["base", "sabela-notebook"]) (nub (concatMap cmDeps comps))
