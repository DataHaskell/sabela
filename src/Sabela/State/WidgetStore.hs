module Sabela.State.WidgetStore (
    WidgetStore (..),
    newWidgetStore,
    getWidgetValues,
    setWidget,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import qualified Data.Map.Strict as M
import Data.Text (Text)

newtype WidgetStore = WidgetStore
    { wsValues :: MVar (M.Map Int (M.Map Text Text))
    }

newWidgetStore :: IO WidgetStore
newWidgetStore = WidgetStore <$> newMVar M.empty

getWidgetValues :: WidgetStore -> Int -> IO (M.Map Text Text)
getWidgetValues ws cid =
    withMVar (wsValues ws) $ pure . M.findWithDefault M.empty cid

setWidget :: WidgetStore -> Int -> Text -> Text -> IO ()
setWidget ws cid name val = modifyMVar_ (wsValues ws) $ \wmap ->
    let cellMap = M.findWithDefault M.empty cid wmap
     in pure (M.insert cid (M.insert name val cellMap) wmap)
