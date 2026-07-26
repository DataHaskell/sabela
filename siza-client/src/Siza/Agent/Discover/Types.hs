module Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    MatchKind (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
    Interpreted (..),
    HackageInfo (..),
    StandingGoal (..),
    emptyScope,
    mkHit,
    okAnswer,
    unavailableAnswer,
    seededBuiltins,
    installText,
    matchKindText,
    hitJson,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.PromptCore (builtinModules, builtinNames)

data InstallState
    = InstBuiltin
    | InstNotebook
    | InstInstalled
    | InstHidden
    | InstAbsentKnown
    | InstAbsentUnknown
    deriving (Bounded, Enum, Eq, Ord, Show)

installText :: InstallState -> Text
installText s = case s of
    InstBuiltin -> "builtin"
    InstNotebook -> "notebook"
    InstInstalled -> "installed"
    InstHidden -> "hidden"
    InstAbsentKnown -> "absent-known"
    InstAbsentUnknown -> "absent-unknown"

data MatchKind
    = MkExact
    | MkPrefix
    | MkModule
    | MkType
    | MkSubstring
    | MkSynonym
    | MkSemantic
    deriving (Bounded, Enum, Eq, Ord, Show)

matchKindText :: MatchKind -> Text
matchKindText k = case k of
    MkExact -> "exact"
    MkPrefix -> "prefix"
    MkModule -> "module"
    MkType -> "type"
    MkSubstring -> "substring"
    MkSynonym -> "synonym"
    MkSemantic -> "semantic"

data DHit = DHit
    { dhName :: Text
    , dhType :: Text
    , dhModule :: Text
    , dhPackage :: Text
    , dhVersion :: Text
    , dhInstall :: InstallState
    , dhKind :: MatchKind
    , dhOrigin :: Text
    , dhCabal :: Maybe Text
    , dhUse :: Maybe Text
    }
    deriving (Eq, Show)

mkHit :: Text -> Text -> Text -> DHit
mkHit n m p =
    DHit n "" m p "" InstAbsentUnknown MkExact "hoogle" Nothing Nothing

data NotebookEnv = NotebookEnv
    { neAliases :: [(Text, Text)]
    , neImports :: [Text]
    , neImportCells :: [(Text, Int)]
    , neBindings :: [Text]
    , neBuiltins :: [Text]
    , neBuiltinModules :: [Text]
    }
    deriving (Eq, Show)

seededBuiltins :: NotebookEnv -> NotebookEnv
seededBuiltins env =
    env{neBuiltins = builtinNames, neBuiltinModules = builtinModules}

data SourceAnswer = SourceAnswer
    { saSource :: Text
    , saOk :: Bool
    , saNote :: Text
    , saHits :: [DHit]
    , saCard :: Maybe Value
    , saPkgModules :: [(Text, [Text])]
    }
    deriving (Eq, Show)

okAnswer :: Text -> [DHit] -> SourceAnswer
okAnswer src hs = SourceAnswer src True "" hs Nothing []

unavailableAnswer :: Text -> Text -> SourceAnswer
unavailableAnswer src why = SourceAnswer src False why [] Nothing []

data Interpreted = Interpreted
    { iRaw :: Text
    , iName :: Text
    , iScope :: Maybe Text
    , iShape :: Text
    , iNote :: Text
    , iTerms :: [Text]
    }
    deriving (Eq, Show)

data Scope = Scope
    { scModule :: Maybe Text
    , scPackage :: Maybe Text
    }
    deriving (Eq, Show)

emptyScope :: Scope
emptyScope = Scope Nothing Nothing

data StandingGoal = StandingGoal
    { sgType :: Text
    , sgConsumer :: Text
    , sgPackage :: Text
    }
    deriving (Eq, Show)

data HackageInfo = HackageInfo
    { hiAvailable :: Bool
    , hiKnown :: [Text]
    }
    deriving (Eq, Show)

hitJson :: DHit -> Value
hitJson h =
    object $
        [ "name" .= dhName h
        , "module" .= orUnknown (dhModule h)
        , "package" .= orUnknown (dhPackage h)
        , "version" .= orUnknown (dhVersion h)
        , "install" .= installText (dhInstall h)
        , "matchKind" .= matchKindText (dhKind h)
        , "origin" .= dhOrigin h
        ]
            <> ["type" .= dhType h | not (T.null (dhType h))]
            <> ["cabal" .= c | Just c <- [dhCabal h]]
            <> ["use" .= u | Just u <- [dhUse h]]
  where
    orUnknown t = if T.null t then "unknown" else t
