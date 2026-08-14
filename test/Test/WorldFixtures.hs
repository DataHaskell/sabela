{-# LANGUAGE OverloadedStrings #-}

{- | The pieces the world-facing specs share: scoped environment variables,
the canonical hodatime facts row, and a gzipped sdist builder.
-}
module Test.WorldFixtures (
    hodatimeFactsRow,
    sdistArchive,
    withEnvVars,
) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarE
import qualified Codec.Compression.GZip as GZip
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv, setEnv, unsetEnv)

import Sabela.AI.FactsRow (PkgFacts (..), renderFactsRow)

{- | Run an action with the vars set, restoring each prior value (or its
absence) afterwards, even when the action throws.
-}
withEnvVars :: [(String, String)] -> IO a -> IO a
withEnvVars vars act = bracket acquire release (const act)
  where
    acquire = mapM setAndSave vars
    setAndSave (k, v) = do
        old <- lookupEnv k
        setEnv k v
        pure (k, old)
    release saved =
        mapM_ (\(k, old) -> maybe (unsetEnv k) (setEnv k) old) (reverse saved)

{- | The canonical hodatime facts row, rendered through the codec so the
fixture and the wire shape cannot drift apart.
-}
hodatimeFactsRow :: Text
hodatimeFactsRow =
    renderFactsRow
        "hodatime"
        PkgFacts
            { pfHomepage = "https://example.invalid/hodatime"
            , pfSynopsis = "A date/time library"
            , pfModules =
                [ "Data.HodaTime"
                , "Data.HodaTime.Instant"
                , "Data.HodaTime.Duration"
                , "Data.HodaTime.Compat"
                , "Data.HodaTime.Calendar.Gregorian"
                ]
            , pfVersion = "0.2.2.1"
            }

-- | A gzipped source tarball holding the given files at the given paths.
sdistArchive :: [(FilePath, Text)] -> BL.ByteString
sdistArchive files = GZip.compress (Tar.write (map entry files))
  where
    entry (path, content) =
        TarE.fileEntry
            (either error id (TarE.toTarPath False path))
            (BLC.pack (T.unpack content))
