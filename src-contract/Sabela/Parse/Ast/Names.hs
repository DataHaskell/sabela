module Sabela.Parse.Ast.Names (rdrText) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

-- | Convert an 'RdrName' to its bare @OccName@ as 'Text'.
rdrText :: RdrName -> Text
rdrText = T.pack . occNameString . rdrNameOcc
