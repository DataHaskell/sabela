module Test.TopoSpec.Helpers (mkCell) where

import Data.Text (Text)
import Sabela.Model (Cell (..), CellType (..))
import Sabela.SessionTypes (CellLang (..))

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
