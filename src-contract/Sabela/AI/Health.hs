{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Health (
    DiagnosticKey (..),
    Health (..),
    healthOfResult,
    healthOfTypeQuery,
    healthOfCellError,
    isClean,
    improvesHealth,
    improvesHealthFor,
    healthMsgsFor,
    normalizeMsg,
    scopeSubject,
) where

import Data.Maybe (listToMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..), bareCellError)

data DiagnosticKey = DiagnosticKey
    { dkLine :: Maybe Int
    , dkCol :: Maybe Int
    , dkMsg :: Text
    }
    deriving (Eq, Ord, Show)

data Health = Health
    { healthCompileOk :: Bool
    , healthDiagnostics :: Set DiagnosticKey
    }
    deriving (Eq, Show)

normalizeMsg :: Text -> Text
normalizeMsg = T.unwords . T.words

diagKey :: CellError -> DiagnosticKey
diagKey ce = DiagnosticKey (ceLine ce) (ceCol ce) (stableMsg (ceMessage ce))

stableMsg :: Text -> Text
stableMsg m
    | "not in scope" `T.isInfixOf` T.toLower n =
        normalizeMsg (fst (T.breakOn "Perhaps" (fst (T.breakOn "::" n))))
    | otherwise = n
  where
    n = normalizeMsg m

healthOfResult :: Either Text ExecutionResult -> Health
healthOfResult (Left e) =
    Health
        False
        ( Set.singleton
            (DiagnosticKey Nothing Nothing (normalizeMsg ("run failed: " <> e)))
        )
healthOfResult (Right er) =
    Health ok (Set.fromList (map diagKey diags))
  where
    holistic = [bareCellError Nothing Nothing m | m <- maybeToList (erError er)]
    diags
        | null (erErrors er) = holistic
        | otherwise = erErrors er
    ok = null holistic && null (erErrors er)

healthOfTypeQuery :: Text -> Health
healthOfTypeQuery out
    | isErr =
        Health False (Set.singleton (DiagnosticKey Nothing Nothing (normalizeMsg out)))
    | otherwise = Health True mempty
  where
    isErr =
        "\"severity\":\"Error\"" `T.isInfixOf` compact
            || "error:" `T.isInfixOf` T.toLower out
    compact = T.filter (/= ' ') out

healthOfCellError :: Maybe Text -> Health
healthOfCellError Nothing = Health True mempty
healthOfCellError (Just e) =
    Health False (Set.singleton (DiagnosticKey Nothing Nothing (normalizeMsg e)))

isClean :: Health -> Bool
isClean h = healthCompileOk h && Set.null (healthDiagnostics h)

improvesHealth :: Health -> Health -> Bool
improvesHealth = improvesHealthFor Set.empty

improvesHealthFor :: Set Text -> Health -> Health -> Bool
improvesHealthFor defined old new =
    isClean new
        || (scopeNew `Set.isSubsetOf` scopeOld && scopeNew /= scopeOld)
        || (scopeNew == scopeOld && dnew `Set.isSubsetOf` dold && dnew /= dold)
  where
    dold = healthMsgsFor defined old
    dnew = healthMsgsFor defined new
    scopeOld = Set.filter scopeKey dold
    scopeNew = Set.filter scopeKey dnew

healthMsgsFor :: Set Text -> Health -> Set Text
healthMsgsFor defined =
    Set.filter (not . knockOn) . Set.map dkMsg . healthDiagnostics
  where
    knockOn m = maybe False (`Set.member` defined) (scopeSubject m)

scopeKey :: Text -> Bool
scopeKey m =
    any
        (`T.isInfixOf` T.toLower m)
        [ "not in scope"
        , "could not find module"
        , "could not load module"
        , "hidden package"
        ]

scopeSubject :: Text -> Maybe Text
scopeSubject m = do
    let low = T.toLower (normalizeMsg m)
        (pre, post) = T.breakOn "not in scope:" low
    if T.null post
        then Nothing
        else do
            let rest = T.drop (T.length pre + T.length ("not in scope:" :: Text)) (normalizeMsg m)
            w <- listToMaybe (T.words rest)
            let name = T.dropAround (`elem` ("`'()" :: String)) w
            if T.null name then Nothing else Just name
