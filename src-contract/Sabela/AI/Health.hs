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
    maskKey,
    normalizeMsg,
    scopeSubject,
    skipScopeDescriptors,
    harnessNames,
    namesHarnessBinder,
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

{- | Binders the harness invents in the code it submits on the model's behalf:
the gate and try wrappers, and the plumbing of the display prelude injected
into every session. The first is the @_sabela@ prefix they are all named with;
the other two predate it.
-}
harnessNames :: [Text]
harnessNames =
    [ "_sabela"
    , "ghciStepIO"
    , "SABELA_PURE_"
    ]

{- | Whether a name, or any text containing one, refers to a binder the
harness invented rather than one the model wrote.
-}
namesHarnessBinder :: Text -> Bool
namesHarnessBinder t = any (`T.isInfixOf` t) harnessNames

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

{- | A masking error stops GHC before it scope-checks the body, so clearing
one is progress however many errors it uncovers — they were always there,
just unreachable. Every other class is judged by set inclusion.
-}
improvesHealthFor :: Set Text -> Health -> Health -> Bool
improvesHealthFor defined old new
    | brokeParsing = False
    | otherwise =
        isClean new
            || cleared maskOld maskNew
            || cleared scopeOld scopeNew
            || (scopeNew == scopeOld && dnew `Set.isSubsetOf` dold && dnew /= dold)
  where
    brokeParsing = anyParseError dnew && not (anyParseError dold)
    cleared before after = after `Set.isSubsetOf` before && after /= before
    dold = healthMsgsFor defined old
    dnew = healthMsgsFor defined new
    scopeOld = Set.filter scopeKey dold
    scopeNew = Set.filter scopeKey dnew
    maskOld = Set.filter maskKey dold
    maskNew = Set.filter maskKey dnew

{- | A cell that no longer parses reports none of its other errors, so by set
inclusion it looks repaired. It is the one regression every other rule would
read as progress, which is why it is judged before them.
-}
anyParseError :: Set Text -> Bool
anyParseError = any isParseError . Set.toList
  where
    isParseError m = "parse error" `T.isInfixOf` T.toLower m

healthMsgsFor :: Set Text -> Health -> Set Text
healthMsgsFor defined =
    Set.filter (not . knockOn) . Set.map dkMsg . healthDiagnostics
  where
    knockOn m = maybe False (`Set.member` defined) (scopeSubject m)

scopeKey :: Text -> Bool
scopeKey m = maskKey m || "not in scope" `T.isInfixOf` T.toLower m

{- | A diagnostic that fails an import, and so masks every error in the body
behind it.
-}
maskKey :: Text -> Bool
maskKey m =
    any
        (`T.isInfixOf` T.toLower m)
        [ "could not find module"
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
            let rest =
                    skipScopeDescriptors
                        ( T.drop
                            (T.length pre + T.length ("not in scope:" :: Text))
                            (normalizeMsg m)
                        )
            w <- listToMaybe (T.words rest)
            let name = T.dropAround (`elem` ("`'()\8216\8217" :: String)) w
            if T.null name then Nothing else Just name

skipScopeDescriptors :: Text -> Text
skipScopeDescriptors t0 = go (T.stripStart t0)
  where
    go t = case [d | d <- descriptors, matches d t] of
        (d : _) -> go (T.stripStart (T.drop (T.length d) t))
        [] -> t
    matches d t = T.toLower d `T.isPrefixOf` T.toLower t
    descriptors =
        ["type constructor or class", "data constructor", "record field"]
