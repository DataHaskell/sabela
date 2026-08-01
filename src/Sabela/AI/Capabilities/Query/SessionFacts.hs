{-# LANGUAGE OverloadedStrings #-}

{- | What the notebook itself shows about a name: which cell imports its module,
which cell defines it, and how those cells last fared. Nothing here claims a
name is in scope — only a compiler can say that.
-}
module Sabela.AI.Capabilities.Query.SessionFacts (
    CellRan (..),
    CellFact (..),
    NotebookFacts (..),
    notebookAliasEnv,
    notebookFacts,
    notebookFactPairs,
    renderNotebookFacts,
    cellRan,
    ranTag,
) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Discover (importedModules)
import Sabela.AI.QualifiedName (AliasEnv, aliasEnvFromSource)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Parse.Declared (declaredNames)

{- | How a cell last fared. @NotRun@ covers a cell edited since its last run —
the notebook cannot say what the session holds for it.
-}
data CellRan = RanClean | RanFailed | NotRun
    deriving (Eq, Show)

data CellFact = CellFact
    { cfCell :: !Int
    , cfRan :: !CellRan
    , cfEvidence :: !Text
    }
    deriving (Eq, Show)

data NotebookFacts = NotebookFacts
    { nfImports :: ![CellFact]
    , nfDefines :: ![CellFact]
    }
    deriving (Eq, Show)

cellRan :: Cell -> CellRan
cellRan c
    | Just _ <- cellError c = RanFailed
    | cellDirty c = NotRun
    | otherwise = RanClean

ranTag :: CellRan -> Text
ranTag RanClean = "clean"
ranTag RanFailed = "failed"
ranTag NotRun = "notRun"

-- | The aliases every code cell in the notebook establishes.
notebookAliasEnv :: Notebook -> AliasEnv
notebookAliasEnv = aliasEnvFromSource . T.unlines . map cellSource . codeCells

{- | The cells that import @mModule@ and the cells that define @name@. Both
lists are computed, so an empty one is the fact that no cell does.
-}
notebookFacts :: Notebook -> Text -> Maybe Text -> NotebookFacts
notebookFacts nb name mModule =
    NotebookFacts
        { nfImports = case mModule of
            Nothing -> []
            Just m -> [fact c l | c <- cells, l <- take 1 (importLines m c)]
        , nfDefines =
            [ fact c ""
            | c <- cells
            , name `Set.member` declaredNames (cellSource c)
            ]
        }
  where
    cells = codeCells nb
    fact c = CellFact (cellId c) (cellRan c)

codeCells :: Notebook -> [Cell]
codeCells nb = [c | c <- nbCells nb, cellType c == CodeCell]

importLines :: Text -> Cell -> [Text]
importLines m c =
    [ T.strip l
    | m `elem` map fst (importedModules (cellSource c))
    , l <- T.lines (cellSource c)
    , "import " `T.isPrefixOf` T.strip l
    , m `elem` T.words (T.strip l)
    ]

notebookFactPairs :: NotebookFacts -> [Pair]
notebookFactPairs f =
    [ "notebook"
        .= object
            [ "importedBy" .= map factJson (nfImports f)
            , "definedBy" .= map factJson (nfDefines f)
            ]
    ]
  where
    factJson cf =
        object
            ( ["cell" .= cfCell cf, "ran" .= ranTag (cfRan cf)]
                <> ["line" .= cfEvidence cf | not (T.null (cfEvidence cf))]
            )

{- | The session clause in prose. Every sentence names the cell it was read
from; when nothing was found the clause says so rather than going silent.
-}
renderNotebookFacts :: Text -> Maybe Text -> NotebookFacts -> Text
renderNotebookFacts name mModule f =
    T.intercalate "\n" (importClause <> defineClause)
  where
    importClause = case (mModule, nfImports f) of
        (Nothing, _) -> []
        (Just m, []) -> ["No notebook cell imports " <> m <> "."]
        (Just _, cs) -> ["Imported by " <> cellList cs <> "."]
    defineClause = case nfDefines f of
        [] -> []
        cs -> [name <> " is also defined by " <> cellList cs <> "."]
    cellList cs =
        T.intercalate
            ", "
            [ "cell " <> T.pack (show (cfCell cf)) <> " (" <> ranTag (cfRan cf) <> ")"
            | cf <- cs
            ]
