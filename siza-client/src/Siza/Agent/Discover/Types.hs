{- | Typed vocabulary of the discover union merge: the four-plus-two install
states (docs/discover/search-api.md section 6), match-kind strata, per-source
answers, and the notebook environment layer. One envelope shape is rendered
from these — no source's raw payload ever crosses to the model.
-}
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

{- | Where a resolved name stands, decided by evidence class (session card,
hackage membership), never by which library it is.
-}
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

-- | How a hit matched the query; strata order exact-first (R3.2).
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

-- | One merged hit with full provenance (R3.5).
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

-- | An exact-kind hit with the given name/module/package and open defaults.
mkHit :: Text -> Text -> Text -> DHit
mkHit n m p =
    DHit n "" m p "" InstAbsentUnknown MkExact "hoogle" Nothing Nothing

{- | The notebook's own environment: alias imports, imported modules (with the
first importing cell), cell-defined names, and the prompt-documented builtins
(R1.5, R1.6).
-}
data NotebookEnv = NotebookEnv
    { neAliases :: [(Text, Text)]
    , neImports :: [Text]
    , neImportCells :: [(Text, Int)]
    , neBindings :: [Text]
    , neBuiltins :: [Text]
    , neBuiltinModules :: [Text]
    }
    deriving (Eq, Show)

{- | Seed an environment's builtin surface from the SAME source the system
prompt derives from, so a documented builtin is structurally undeniable.
-}
seededBuiltins :: NotebookEnv -> NotebookEnv
seededBuiltins env =
    env{neBuiltins = builtinNames, neBuiltinModules = builtinModules}

-- | One consulted source's classified answer; unavailability is per-source.
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

-- | The normalised query: what was actually searched, and why (R2.4).
data Interpreted = Interpreted
    { iRaw :: Text
    , iName :: Text
    , iScope :: Maybe Text
    , iShape :: Text
    , iNote :: Text
    , iTerms :: [Text]
    }
    deriving (Eq, Show)

{- | The request's optional scope filters (R2.7): honoured at the merge or
the call is rejected — never silently ignored.
-}
data Scope = Scope
    { scModule :: Maybe Text
    , scPackage :: Maybe Text
    }
    deriving (Eq, Show)

emptyScope :: Scope
emptyScope = Scope Nothing Nothing

{- | The evidence-derived standing goal (section 8.3): the argument type a
held call-ready consumer signature needs but no held fact produces, with the
consumer's name and package as ledger provenance evidence.
-}
data StandingGoal = StandingGoal
    { sgType :: Text
    , sgConsumer :: Text
    , sgPackage :: Text
    }
    deriving (Eq, Show)

-- | The hackage names source: availability plus the membership answers held.
data HackageInfo = HackageInfo
    { hiAvailable :: Bool
    , hiKnown :: [Text]
    }
    deriving (Eq, Show)

-- | Render one hit; the four provenance fields are always present (R3.5).
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
