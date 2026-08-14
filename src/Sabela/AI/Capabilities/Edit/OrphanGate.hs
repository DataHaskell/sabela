{-# LANGUAGE OverloadedStrings #-}

{- | Deleting the only cell that declares a dependency leaves every surviving
import of its modules alive only in the session's sticky environment, and
broken on any rebuild. The gate names the orphan up front.
-}
module Sabela.AI.Capabilities.Edit.OrphanGate (
    Orphan (..),
    lostDeclarations,
    orphansAmong,
    deleteOrphans,
    orphanRefusal,
    undeclaredImportNote,
    undeclaredImportPairs,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Parser (CabalMeta (..))

import Sabela.AI.HackageFacts (packageModules)
import Sabela.AI.ImportRepair (importedModules)
import Sabela.AI.PackageIndex (
    installedPackages,
    modulesOfPackage,
    storePackageDb,
 )
import Sabela.Deps (collectMetadata)
import Sabela.Diagnose.Packages (findModulePackage)
import Sabela.Diagnose.Parse (couldNotFindModule)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    blamedCell,
 )
import Sabela.State (App (..), appNotebook)
import Sabela.State.NotebookStore (readNotebook)

-- | A surviving import that only the doomed cell's dependency line resolves.
data Orphan = Orphan
    { orphanCellId :: Int
    , orphanModule :: Text
    , orphanPackage :: Text
    }
    deriving (Eq, Show)

-- | The deps only the doomed cell declares, less what the environment grants.
lostDeclarations :: S.Set Text -> Notebook -> Int -> [Text]
lostDeclarations globalDeps nb cid =
    S.toAscList ((before `S.difference` after) `S.difference` globalDeps)
  where
    before = declared nb
    after = declared nb{nbCells = filter ((/= cid) . cellId) (nbCells nb)}
    declared = S.fromList . metaDeps . collectMetadata

-- | The surviving imports each lost dep's module set covers.
orphansAmong :: [(Text, [Text])] -> Notebook -> Int -> [Orphan]
orphansAmong lostModules nb cid =
    [ Orphan (cellId c) m pkg
    | c <- nbCells nb
    , cellId c /= cid
    , cellType c == CodeCell
    , m <- importedModules (cellSource c)
    , (pkg, mods) <- lostModules
    , m `elem` mods
    ]

-- | Production wiring: a lost dep's modules from the facts index + the store.
deleteOrphans :: S.Set Text -> Notebook -> Int -> IO [Orphan]
deleteOrphans globalDeps nb cid =
    case lostDeclarations globalDeps nb cid of
        [] -> pure []
        lost -> do
            mods <- traverse (\d -> (,) d <$> modulesEverywhere d) lost
            pure (orphansAmong mods nb cid)

modulesEverywhere :: Text -> IO [Text]
modulesEverywhere pkg = do
    fromFacts <- packageModules pkg
    mDb <- storePackageDb
    fromStore <- case mDb of
        Nothing -> pure []
        Just db -> flip modulesOfPackage pkg <$> installedPackages db
    pure (S.toAscList (S.fromList (fromFacts <> fromStore)))

orphanRefusal :: Int -> [Orphan] -> Value
orphanRefusal cid orphans =
    object
        [ "error" .= message
        , "notCommitted" .= ("orphaned-imports" :: Text)
        , "cellId" .= cid
        , "orphanedImports"
            .= [ object ["cellId" .= oc, "module" .= om, "package" .= op]
               | Orphan oc om op <- orphans
               ]
        ]
  where
    message =
        "Deleting cell "
            <> tShow cid
            <> " removes the notebook's only 'build-depends: "
            <> pkgs
            <> "' declaration, and "
            <> uses
            <> ". Those imports resolve now only because the session already \
               \installed the package; a rebuilt notebook cannot. Declare the \
               \dependency on a surviving cell ('-- cabal: build-depends: "
            <> pkgs
            <> "'), then delete."
    pkgs = T.intercalate ", " (nub [op | Orphan _ _ op <- orphans])
    uses =
        T.intercalate
            "; "
            [ "cell " <> tShow oc <> " imports " <> om
            | Orphan oc om _ <- orphans
            ]

{- | The note a cell_replay module-not-found refusal carries when the module's
package is installed but undeclared: the blamed cell is healthy live only,
and the durable fix is a declaration, not a repair of the candidate.
-}
undeclaredImportNote :: [Text] -> Maybe Int -> Text -> Text -> Maybe Text
undeclaredImportNote declaredDeps mCell modName pkg
    | pkg `elem` declaredDeps = Nothing
    | otherwise =
        Just $
            cellRef
                <> " imports "
                <> modName
                <> ", whose package ("
                <> pkg
                <> ") no cell declares. The import resolves live only because "
                <> pkg
                <> " is still installed in the session; every rebuild, this \
                   \gate included, fails on it. Declare '-- cabal: \
                   \build-depends: "
                <> pkg
                <> "' on a durable cell"
                <> home
                <> ", not on this candidate."
  where
    cellRef = maybe "A replayed cell" (("notebook cell " <>) . tShow) mCell
    home = maybe "" (\cid -> " (cell " <> tShow cid <> " itself)") mCell

-- | Production wiring for the note, keyed off the replay failure's own facts.
undeclaredImportPairs :: App -> DisposableResult -> IO [Pair]
undeclaredImportPairs app result
    | Just f <- disposableFailure result
    , failureStage f == StageCellReplay
    , Just m <- couldNotFindModule (failureMessage f) = do
        owner <- findModulePackage m
        case owner of
            Nothing -> pure []
            Just pkg -> do
                nb <- readNotebook (appNotebook app)
                let declared = metaDeps (collectMetadata nb)
                pure
                    [ "undeclaredImport" .= note
                    | Just note <-
                        [ undeclaredImportNote
                            declared
                            (blamedCell result)
                            m
                            pkg
                        ]
                    ]
    | otherwise = pure []

tShow :: Int -> Text
tShow = T.pack . show
