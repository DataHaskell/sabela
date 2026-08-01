{-# LANGUAGE OverloadedStrings #-}

{- | One qualifier/alias seam for both surfaces. The client's search interpreter
and the server's check_type route resolve @Alias.name@ the same way, so a name
that one of them can find is never a miss for the other.
-}
module Sabela.AI.QualifiedName (
    AliasEnv (..),
    emptyAliasEnv,
    aliasEnvOf,
    aliasEnvFromSource,
    QualifiedName (..),
    resolveName,
    resolveLookup,
    resolveSpelling,
    lookupSpelling,
    splitQualifier,
    bareSpelling,
    isPlainName,
) where

import Data.Char (isAlphaNum, isUpper)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Discover (importedModules)
import Sabela.AI.Grammar (ImportStyle (..))

{- | The aliases and module names a notebook's own imports establish.
@aeAliases@ maps a qualifier to the module it names; @aeModules@ is every
module imported under its own name.
-}
data AliasEnv = AliasEnv
    { aeAliases :: [(Text, Text)]
    , aeModules :: [Text]
    }
    deriving (Eq, Show)

emptyAliasEnv :: AliasEnv
emptyAliasEnv = AliasEnv [] []

aliasEnvOf :: [(Text, Text)] -> [Text] -> AliasEnv
aliasEnvOf as ms = AliasEnv as ms

-- | The alias environment a block of Haskell source establishes.
aliasEnvFromSource :: Text -> AliasEnv
aliasEnvFromSource src =
    AliasEnv
        (nub [(p, m) | (m, QualifiedAs p) <- imports, p /= m])
        (nub (map fst imports))
  where
    imports = importedModules src

{- | A name after qualifier resolution: the bare name, the module the qualifier
named (when one could be resolved), and the disclosure of how — empty when the
name carried no qualifier and nothing was inferred.
-}
data QualifiedName = QualifiedName
    { qnBare :: Text
    , qnModule :: Maybe Text
    , qnNote :: Text
    }
    deriving (Eq, Show)

{- | Resolve @Qualifier.name@ against the notebook's imports. An unrecognised
qualifier yields the bare name and says so; it never invents a module.
-}
resolveName :: AliasEnv -> Text -> QualifiedName
resolveName env t = case splitQualifier t of
    Nothing -> QualifiedName t Nothing ""
    Just (qual, nm) -> case lookup qual (aeAliases env) of
        Just m ->
            QualifiedName
                nm
                (Just m)
                ("alias " <> qual <> " = " <> m <> " (notebook import)")
        Nothing
            | T.any (== '.') qual || qual `elem` aeModules env ->
                QualifiedName nm (Just qual) ""
            | otherwise ->
                QualifiedName
                    nm
                    Nothing
                    ( "qualifier '"
                        <> qual
                        <> "' is not a notebook alias; searched the bare name"
                    )

{- | 'resolveName', then the one split it refuses: a qualified type, class or
constructor (an upper-headed last component), which is ambiguous with a module
path. Only the notebook's own imports settle it, so only they resolve it.
-}
resolveLookup :: AliasEnv -> Text -> QualifiedName
resolveLookup env t = case resolveName env t of
    qn
        | Nothing <- qnModule qn
        , Just qn' <- importedQualifier env (qnBare qn) ->
            qn'
    qn -> qn

importedQualifier :: AliasEnv -> Text -> Maybe QualifiedName
importedQualifier env t
    | Just (qual, nm) <- dottedTail t =
        case lookup qual (aeAliases env) of
            Just m ->
                Just
                    ( QualifiedName
                        nm
                        (Just m)
                        ("alias " <> qual <> " = " <> m <> " (notebook import)")
                    )
            Nothing
                | qual `elem` aeModules env ->
                    Just
                        ( QualifiedName
                            nm
                            (Just qual)
                            ("module " <> qual <> " (notebook import)")
                        )
                | otherwise -> Nothing
    | otherwise = Nothing
  where
    dottedTail s
        | T.any (== '.') s
        , not (T.any (== ' ') s)
        , upperHead s
        , (qual, nm) <- breakLast s
        , not (T.null qual)
        , not (T.null nm) =
            Just (qual, nm)
        | otherwise = Nothing

{- | 'resolveName' over a written spelling: an operator in parentheses and a
unit-id prefix are stripped before resolution and the parentheses restored,
so @(M.<+>)@ resolves exactly as @M.<+>@ does.
-}
resolveSpelling :: AliasEnv -> Text -> QualifiedName
resolveSpelling = spellingWith resolveName

-- | 'resolveLookup' over a written spelling, for the index's benefit.
lookupSpelling :: AliasEnv -> Text -> QualifiedName
lookupSpelling = spellingWith resolveLookup

spellingWith ::
    (AliasEnv -> Text -> QualifiedName) -> AliasEnv -> Text -> QualifiedName
spellingWith resolve env t
    | operatorSection = rewrap (resolve env (dropUnit inner))
    | otherwise = resolve env (dropUnit t)
  where
    operatorSection = "(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t && T.length t > 2
    inner = T.init (T.drop 1 t)
    rewrap qn = qn{qnBare = "(" <> qnBare qn <> ")"}

-- | The bare spelling alone, with no environment to resolve aliases against.
bareSpelling :: Text -> Text
bareSpelling = qnBare . resolveSpelling emptyAliasEnv

{- | The split 'resolveName' works on: an upper-headed dotted spelling whose
last component is not itself a module name.
-}
splitQualifier :: Text -> Maybe (Text, Text)
splitQualifier t
    | T.any (== '.') t
    , not (T.any (== ' ') t)
    , upperHead t
    , (qual, nm) <- breakLast t
    , not (T.null nm)
    , not (upperHead nm) =
        Just (qual, nm)
    | otherwise = Nothing

breakLast :: Text -> (Text, Text)
breakLast s =
    let parts = T.splitOn "." s
     in (T.intercalate "." (init parts), last parts)

-- | A single lexeme that could be a Haskell name: no space, no type syntax.
isPlainName :: Text -> Bool
isPlainName t =
    not (T.null t)
        && not (T.any (== ' ') t)
        && not ("::" `T.isInfixOf` t)
        && not ("->" `T.isInfixOf` t)

dropUnit :: Text -> Text
dropUnit t = case T.breakOnEnd ":" t of
    (pre, rest) | not (T.null pre), unitLike (T.init pre) -> rest
    _ -> t
  where
    unitLike u =
        not (T.null u)
            && T.all (\c -> isAlphaNum c || c `elem` ("-.+_" :: String)) u

upperHead :: Text -> Bool
upperHead s = maybe False (isUpper . fst) (T.uncons s)
