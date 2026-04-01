module Sabela.State.BridgeStore (
    BridgeStore (..),
    newBridgeStore,
    getBridgeValues,
    setBridgeValue,
    modifyBridgeStore,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import qualified Data.Map.Strict as M
import Data.Text (Text)

newtype BridgeStore = BridgeStore
    { bsValues :: MVar (M.Map Text Text)
    }

newBridgeStore :: IO BridgeStore
newBridgeStore = BridgeStore <$> newMVar M.empty

getBridgeValues :: BridgeStore -> IO (M.Map Text Text)
getBridgeValues = readMVar . bsValues

setBridgeValue :: BridgeStore -> Text -> Text -> IO ()
setBridgeValue bs name val =
    modifyMVar_ (bsValues bs) $
        pure . M.insert name val

modifyBridgeStore ::
    BridgeStore -> (M.Map Text Text -> M.Map Text Text) -> IO ()
modifyBridgeStore bs f = modifyMVar_ (bsValues bs) (pure . f)
