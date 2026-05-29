-- | Shared helpers for the Test.TopoSpec.* family.
module Test.TopoSpec.Helpers (mkCell) where

import Data.Text (Text)
import Sabela.Model (Cell (..), CellType (..))
import Sabela.SessionTypes (CellLang (..))

-- | Helper to construct a code cell for testing.
mkCell :: Int -> Text -> Cell
mkCell cid src =
    Cell
        { cellId = cid
        , cellType = CodeCell
        , cellLang = Haskell
        , cellSource = src
        , cellOutputs = []
        , cellError = Nothing
        , cellDirty = False
        }
