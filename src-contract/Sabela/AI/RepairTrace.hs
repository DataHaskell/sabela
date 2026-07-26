{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.RepairTrace (
    RepairEvent (..),
    repairEventJSON,
    repairTracePath,
    recordRepair,
) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

data RepairEvent = RepairEvent
    { reCellId :: Int
    , reCounts :: [(Text, Int)]
    , reWinner :: Maybe Text
    }
    deriving (Eq, Show)

repairEventJSON :: RepairEvent -> Value
repairEventJSON ev =
    object
        [ "cellId" .= reCellId ev
        , "counts" .= object [K.fromText k .= v | (k, v) <- reCounts ev]
        , "winner" .= reWinner ev
        ]

repairTracePath :: FilePath -> FilePath
repairTracePath workDir = workDir </> ".sabela" </> "repair-trace.jsonl"

recordRepair :: FilePath -> RepairEvent -> IO ()
recordRepair workDir ev = void (try go :: IO (Either SomeException ()))
  where
    path = repairTracePath workDir
    go = do
        createDirectoryIfMissing True (takeDirectory path)
        appendFile path (LBS.unpack (encode (repairEventJSON ev)) ++ "\n")
