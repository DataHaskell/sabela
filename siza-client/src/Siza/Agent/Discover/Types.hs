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
    exportRow,
    exportRowName,
    statedHitType,
    statesDeclaration,
    typeClause,
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
    InstHidden -> "installed-not-loaded"
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
    , dhClash :: Maybe Text
    }
    deriving (Eq, Show)

mkHit :: Text -> Text -> Text -> DHit
mkHit n m p =
    DHit n "" m p "" InstAbsentUnknown MkExact "hoogle" Nothing Nothing Nothing

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
    , saAllOk :: Bool
    , saNote :: Text
    , saHits :: [DHit]
    , saCard :: Maybe Value
    , saPkgModules :: [(Text, [Text])]
    }
    deriving (Eq, Show)

okAnswer :: Text -> [DHit] -> SourceAnswer
okAnswer src hs = SourceAnswer src True True "" hs Nothing []

unavailableAnswer :: Text -> Text -> SourceAnswer
unavailableAnswer src why = SourceAnswer src False False why [] Nothing []

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

{- | How a hit states its type in prose: a declaration states itself, a
signature is introduced by @::@. Announcing a declaration as a signature would
state one no source gave.
-}
typeClause :: Text -> Text
typeClause t
    | T.null t = ""
    | statesDeclaration t = t
    | otherwise = ":: " <> t

{- | Whether text is a whole declaration rather than a signature. One reader of
the shape, so no renderer announces a declaration as a type and no ranker reads
one as a producer.
-}
statesDeclaration :: Text -> Bool
statesDeclaration t = any (\k -> (k <> " ") `T.isPrefixOf` t) declKeywords

{- | The keywords whose declaration names its subject after the keyword. The
listing a card claim is checked against reads its own lines by this list, so a
declaration means one entity to the writer and to the witness.
-}
declKeywords :: [Text]
declKeywords = ["data", "type", "newtype", "class", "pattern"]

{- | The row a card states for one export: a signature under the entity's own
name, a declaration as its source stated it. 'exportRowName' is its left
inverse, so a row and the entity it claims are read from one law.
-}
exportRow :: Text -> Text -> Text
exportRow n ty
    | T.null (T.strip ty) = n
    | statesDeclaration ty = ty
    | otherwise = n <> " :: " <> ty

{- | The entity a stated row names: a signature by its head, a declaration by
the word after its keyword. The listing a claim is checked against reads its
own lines this way, so a row and its witness name the same entity.
-}
exportRowName :: Text -> Text
exportRowName row = case T.words (T.replace "::" " :: " row) of
    (w : _)
        | w `elem` declKeywords -> wordAt 1 (T.words row)
        | otherwise -> unqualify (T.takeWhile (/= ',') w)
    [] -> ""
  where
    wordAt i t = case drop i t of
        (w : _) -> w
        [] -> ""

-- | A name without its module qualifier, as a listing states it.
unqualify :: Text -> Text
unqualify n
    | T.null base = n
    | otherwise = base
  where
    base = T.takeWhileEnd (/= '.') n

{- | The type text a hit may state: a signature, or a declaration whose source
expanded it. A declaration head expands to nothing, so it states no type at all
rather than dressing the hit's own name as its expansion.
-}
statedHitType :: Text -> Text
statedHitType t
    | not (statesDeclaration s) = s
    | T.null (T.strip (T.drop 1 rhs)) = ""
    | otherwise = s
  where
    s = T.strip t
    (_, rhs) = T.breakOn "=" s

{- | A field the harness did not compute is absent, never the placeholder
"unknown": a mandated placeholder spends the envelope budget that
'Siza.Agent.Discover.Envelope.dropLastHit' then reclaims from a real hit.
-}
hitJson :: DHit -> Value
hitJson h =
    object $
        [ "name" .= dhName h
        , "install" .= installText (dhInstall h)
        , "matchKind" .= matchKindText (dhKind h)
        , "origin" .= dhOrigin h
        ]
            <> computed "module" (dhModule h)
            <> computed "package" (dhPackage h)
            <> computed "version" (dhVersion h)
            <> computed "type" (dhType h)
            <> ["cabal" .= c | Just c <- [dhCabal h]]
            <> ["use" .= u | Just u <- [dhUse h]]
            <> ["ambiguousWith" .= c | Just c <- [dhClash h]]
  where
    computed k t = [k .= t | not (T.null t)]
