{- | The server's reader for the Hackage facts cache: which package exposes
a module, and the release the index documented. Resolved through the same
ladder as the client's, cached per revision of the file.
-}
module Sabela.AI.HackageFacts (
    factsVersion,
    moduleOwners,
    packageModules,
    loadFacts,
) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.FactsCache (exactModuleOwners, loadHackageFacts)
import Sabela.AI.FactsRow (PkgFacts (..))

-- | The release the index documented for a package, when the row states one.
factsVersion :: Text -> IO (Maybe Text)
factsVersion pkg = do
    facts <- loadFacts
    pure $ case M.lookup pkg facts of
        Just f | not (T.null (pfVersion f)) -> Just (pfVersion f)
        _ -> Nothing

-- | The modules the index records for a package, empty when unknown.
packageModules :: Text -> IO [Text]
packageModules pkg =
    maybe [] pfModules . M.lookup (T.strip pkg) <$> loadFacts

-- | The packages whose public library exposes exactly the named module.
moduleOwners :: Text -> IO [(Text, PkgFacts)]
moduleOwners m = exactModuleOwners m <$> loadFacts

loadFacts :: IO (M.Map Text PkgFacts)
loadFacts = loadHackageFacts
