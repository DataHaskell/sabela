{-# LANGUAGE BangPatterns #-}

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

{- | Set a widget's value. The bangs keep a high-rate slider drag from
piling nested @M.insert@ thunks in the 'MVar'.
-}
setWidget :: WidgetStore -> Int -> Text -> Text -> IO ()
setWidget ws cid name val = modifyMVar_ (wsValues ws) $ \wmap -> do
    let !cellMap = M.findWithDefault M.empty cid wmap
        !cellMap' = M.insert name val cellMap
        !wmap' = M.insert cid cellMap' wmap
    pure wmap'
