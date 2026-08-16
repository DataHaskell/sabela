{-# LANGUAGE OverloadedStrings #-}

{- | The world the read_source wire specs run in: a temp mirror holding a
synthetic hodatime sdist and a facts index, plus helpers that call the
executor and read its JSON answer.
-}
module Test.ReadSourceWorld (
    defaultSdist,
    manyDeclCount,
    run,
    textAt,
    withWorld,
    withWorldSdist,
) where

import Control.Exception (bracket)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))

import Sabela.AI.Capabilities.ReadSource (readSourceOutcome)
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Test.WorldFixtures (hodatimeFactsRow, sdistArchive, withEnvVars)

instantSource :: Text
instantSource =
    T.unlines
        [ "module Data.HodaTime.Instant (difference) where" -- 1
        , "" -- 2
        , "difference :: Instant -> Instant -> Duration" -- 3
        , "difference a b = Duration" -- 4
        , "" -- 5
        , "data Duration = Duration" -- 6
        ]

-- | The hodatime shape that bit live: exported here, defined in an import.
compatSource :: Text
compatSource =
    T.unlines
        [ "module Data.HodaTime.Compat (difference) where"
        , ""
        , "import Data.HodaTime.Instant"
        , "import qualified Data.HodaTime.Instant as I"
        , "import qualified Data.List as L"
        , ""
        , "compatOnly :: Int"
        , "compatOnly = L.length (L.sort [2, 1])"
        , ""
        , "compatDur :: I.Duration -> Int"
        , "compatDur _ = 0"
        ]

-- | More definitions than one outline shows, so the cap and count differ.
manyDeclCount :: Int
manyDeclCount = 125

manyDeclSource :: Text
manyDeclSource =
    T.unlines $
        "module Data.HodaTime where"
            : ""
            : ["d" <> n <> " = " <> n | i <- [1 .. manyDeclCount], let n = tShow i]

-- | One definition past the content cap, so the cut is disclosed.
bigDeclSource :: Text
bigDeclSource =
    T.unlines $
        [ "module Data.HodaTime.Calendar.Gregorian (bulk) where"
        , ""
        , "bulk :: [Int]"
        , "bulk ="
        , "    [ 0"
        ]
            <> ["    , " <> tShow i | i <- [1 .. 2000 :: Int]]
            <> ["    ]"]

tShow :: Int -> Text
tShow = T.pack . show

{- | The released hodatime archive. The facts row also lists
@Data.HodaTime.Duration@, which this archive deliberately lacks.
-}
defaultSdist :: BL.ByteString
defaultSdist =
    sdistArchive
        [ ("hodatime-0.2.2.1/src/Data/HodaTime.hs", manyDeclSource)
        , ("hodatime-0.2.2.1/src/Data/HodaTime/Instant.hs", instantSource)
        , ("hodatime-0.2.2.1/src/Data/HodaTime/Compat.hs", compatSource)
        ,
            ( "hodatime-0.2.2.1/src/Data/HodaTime/Calendar/Gregorian.hs"
            , bigDeclSource
            )
        ]

factsRows :: Text
factsRows =
    T.unlines
        [ hodatimeFactsRow
        , "widgets-a\t\tWidgets\tWeb.Shared\t1.0"
        , "widgets-b\t\tWidgets\tWeb.Shared\t2.0"
        , "verless\t\tNo release recorded\tData.Verless\t"
        ]

withWorld :: IO a -> IO a
withWorld = withWorldSdist defaultSdist

-- | The standard world, with the given bytes as the hodatime tarball.
withWorldSdist :: BL.ByteString -> IO a -> IO a
withWorldSdist bytes act = bracket acquire removeDirectoryRecursive inWorld
  where
    acquire = do
        tmp <- getTemporaryDirectory
        let root = tmp </> "sabela-read-source-wire-spec"
        createDirectoryIfMissing True (root </> "cabal")
        createDirectoryIfMissing True (root </> "sdists")
        BL.writeFile (root </> "sdists" </> "hodatime-0.2.2.1.tar.gz") bytes
        TIO.writeFile (root </> "facts.tsv") factsRows
        pure root
    inWorld root =
        withEnvVars
            [ ("SABELA_HACKAGE_FACTS", root </> "facts.tsv")
            , ("SABELA_CABAL_PACKAGES_DIR", root </> "cabal")
            , ("SABELA_SDIST_CACHE_DIR", root </> "sdists")
            , ("SABELA_HOOGLE_BIN", root </> "no-hoogle")
            ]
            act

run :: Value -> IO (Bool, KM.KeyMap Value)
run input = do
    o <- readSourceOutcome Nothing input
    pure
        ( toolOutcomeIsError o
        , case toolOutcomeValue o of
            Object km -> km
            _ -> KM.empty
        )

textAt :: KM.KeyMap Value -> Text -> Text
textAt km k = case KM.lookup (K.fromText k) km of
    Just (String t) -> t
    _ -> ""
