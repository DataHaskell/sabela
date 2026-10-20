{-# LANGUAGE DeriveGeneric #-}

-- | Wire types for the editor's live-bindings listing (@GET /api/bindings@).
module Sabela.Api.Bindings (
    BindingEntry (..),
    MemoryStats (..),
    BindingsResult (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

{- | One entry of the live GHCi binding listing. @beKind@ is @value@, @data@,
@newtype@, @type@ or @class@; @beValue@ is the shown value of an evaluated
binding and absent for an unevaluated thunk or a declaration.
-}
data BindingEntry = BindingEntry
    { beName :: Text
    , beKind :: Text
    , beType :: Text
    , beValue :: Maybe Text
    , beLive :: Bool
    }
    deriving (Eq, Generic, Show)

instance ToJSON BindingEntry
instance FromJSON BindingEntry

-- | GHC RTS heap figures, in bytes, as reported by @GHC.Stats@ at the last GC.
data MemoryStats = MemoryStats
    { msLiveBytes :: Integer
    , msMaxLiveBytes :: Integer
    , msMemInUseBytes :: Integer
    , msGcs :: Integer
    }
    deriving (Eq, Generic, Show)

instance ToJSON MemoryStats
instance FromJSON MemoryStats

-- | @brStatus@ is @ok@, @busy@, @timeout@ or @no-session@.
data BindingsResult = BindingsResult
    { brBindings :: [BindingEntry]
    , brMemory :: Maybe MemoryStats
    , brStatus :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON BindingsResult
instance FromJSON BindingsResult
