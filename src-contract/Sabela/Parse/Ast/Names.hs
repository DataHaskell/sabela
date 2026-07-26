module Sabela.Parse.Ast.Names (rdrText) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)

rdrText :: RdrName -> Text
rdrText = T.pack . occNameString . rdrNameOcc
