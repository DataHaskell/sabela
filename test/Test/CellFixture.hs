{-# LANGUAGE OverloadedStrings #-}

{- | One place tests build a 'Cell'.

There were seven of these, mostly positional, so any change to 'Cell' broke them
all at once and each had to be edited by hand. Building them here means a new
field is one edit, and the compiler still finds every caller that needs a
different value.
-}
module Test.CellFixture (
    mkCell,
    proseCell,
    pyCell,
    dirty,
    errored,
    withOutput,
) where

import Data.Text (Text)
import Sabela.Model (
    Cell (..),
    CellType (..),
    MimeType (MimePlain),
    OutputItem (..),
 )
import Sabela.SessionTypes (CellLang (..))

-- | A settled Haskell code cell: it ran, it succeeded, nothing has changed since.
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

proseCell :: Int -> Text -> Cell
proseCell cid src = (mkCell cid src){cellType = ProseCell}

pyCell :: Int -> Text -> Cell
pyCell cid src = (mkCell cid src){cellLang = Python}

-- | Edited since it last ran.
dirty :: Cell -> Cell
dirty c = c{cellDirty = True}

-- | Ran and failed.
errored :: Cell -> Cell
errored c = c{cellError = Just "boom"}

-- | Ran and produced something.
withOutput :: Text -> Cell -> Cell
withOutput out c = c{cellOutputs = [OutputItem MimePlain out]}
