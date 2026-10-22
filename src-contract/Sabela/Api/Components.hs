{-# LANGUAGE DeriveGeneric #-}

{- | Wire types for components: named modules in a workspace's own
@components/@ package, which any notebook in the workspace imports, and
which travel between workspaces as a bundle.
-}
module Sabela.Api.Components (
    ComponentInfo (..),
    ExtractRequest (..),
    ExtractPreview (..),
    ExtractAccept (..),
    ComponentBundle (..),
    InstallRequest (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data ComponentInfo = ComponentInfo
    { ciModule :: Text
    , ciPath :: Text
    , ciPurpose :: Text
    , ciKind :: Text
    , ciExports :: [Text]
    , ciDeps :: [Text]
    }
    deriving (Eq, Generic, Show)

instance ToJSON ComponentInfo
instance FromJSON ComponentInfo

-- | Cells to turn into a component, and the module name to give it.
data ExtractRequest = ExtractRequest
    { erCellIds :: [Int]
    , erName :: Text
    , erPurpose :: Maybe Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON ExtractRequest
instance FromJSON ExtractRequest

{- | What accepting would write: the module and the text the first selected
cell becomes, with the problems the extraction noticed.
-}
data ExtractPreview = ExtractPreview
    { epModule :: Text
    , epPath :: Text
    , epSource :: Text
    , epExports :: [Text]
    , epWarnings :: [Text]
    , epReplacement :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON ExtractPreview
instance FromJSON ExtractPreview

-- | The preview as reviewed, possibly edited, to be written and applied.
data ExtractAccept = ExtractAccept
    { eaCellIds :: [Int]
    , eaModule :: Text
    , eaSource :: Text
    , eaReplacement :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON ExtractAccept
instance FromJSON ExtractAccept

-- | A component as it travels: everything needed to install it elsewhere.
data ComponentBundle = ComponentBundle
    { cbModule :: Text
    , cbPurpose :: Text
    , cbKind :: Text
    , cbExports :: [Text]
    , cbDeps :: [Text]
    , cbSource :: Text
    , cbVersion :: Int
    }
    deriving (Eq, Generic, Show)

instance ToJSON ComponentBundle
instance FromJSON ComponentBundle

data InstallRequest = InstallRequest
    { irBundle :: ComponentBundle
    , irOverwrite :: Bool
    }
    deriving (Eq, Generic, Show)

instance ToJSON InstallRequest
instance FromJSON InstallRequest
