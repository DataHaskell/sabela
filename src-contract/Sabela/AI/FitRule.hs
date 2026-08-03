{-# LANGUAGE OverloadedStrings #-}

{- | Separates a hole fit that says something about the goal from one that
inhabits any goal of the same shape. Stated as a property rather than a list of
names: this generalises the `vacuousFit` denylist it replaces.
-}
module Sabela.AI.FitRule (
    Freeness (..),
    classifyFit,
    informativeFits,
    holeFitsJson,
) where

import Data.Aeson (Value, object, (.=))
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.AI.HoleFits (
    HoleFit (..),
    parseHoleFits,
    stripPackageQualifiers,
 )

{- | The fits a caller should show the model: GHC's list with the goal-free
inhabitants removed, then capped.
-}
holeFitsJson :: Int -> Text -> [Value]
holeFitsJson cap = map render . take cap . informativeFits . parseHoleFits
  where
    render f =
        object
            ( [ "write" .= sanitizeTypeText (hfWrite f)
              , "type" .= sanitizeTypeText (stripPackageQualifiers (hfType f))
              , "refined" .= hfRefined f
              ]
                <> ["module" .= m | Just m <- [hfModule f]]
            )

data Freeness
    = Informative
    | FreeProjection
    | FreeArrow
    | FreeAxiom
    deriving (Eq, Show)

informativeFits :: [HoleFit] -> [HoleFit]
informativeFits = filter ((== Informative) . classifyFit)

{- | A fit is goal-free when it returns one of its own arguments unconstrained,
when every constraint it carries is discharged at a function type, or when it
takes nothing and yields a bare variable, which inhabits every goal.
-}
classifyFit :: HoleFit -> Freeness
classifyFit fit
    | hfWrite fit == "undefined" = FreeAxiom
    | projection = FreeProjection
    | arrowStructural = FreeArrow
    | bottom = FreeAxiom
    | otherwise = Informative
  where
    bottom = null args && isTypeVar result
    (constraints, body) = splitConstraints (dropForall (hfType fit))
    parts = splitArrow body
    (args, result) = (init' parts, last' parts)
    projection = result `elem` args && null constraints && isTypeVar result
    constrainedVars = concatMap typeVars constraints
    boundVars = foralls (hfType fit)
    arrowStructural =
        not (null constrainedVars)
            && not (null (hfAppliedAt fit))
            && all atFunctionType constrainedVars
    atFunctionType v = case lookup v (zip boundVars (hfAppliedAt fit)) of
        Just applied -> headIsArrow applied
        Nothing -> False

{- | A type application whose outermost constructor is the function arrow,
written either infix as @(a -> b)@ or prefix as @((->) a)@.
-}
headIsArrow :: Text -> Bool
headIsArrow applied =
    "(->)" `T.isPrefixOf` inner || isArrowInfix
  where
    inner = stripOuterParens (T.strip applied)
    isArrowInfix = case breakTop "->" inner of
        Just _ -> True
        Nothing -> False

stripOuterParens :: Text -> Text
stripOuterParens t
    | Just rest <- T.stripPrefix "(" t
    , Just body <- T.stripSuffix ")" rest
    , balanced body =
        T.strip body
    | otherwise = t
  where
    balanced = go (0 :: Int) . T.unpack
    go d [] = d == 0
    go d (c : cs)
        | c == '(' = go (d + 1) cs
        | c == ')' = d > 0 && go (d - 1) cs
        | otherwise = go d cs

{- | A monomorphic result cannot be a free inhabitant: only a type variable can
be returned without looking at the argument that supplied it.
-}
isTypeVar :: Text -> Bool
isTypeVar t = case T.uncons (T.strip t) of
    Just (c, _) -> c `elem` ['a' .. 'z']
    Nothing -> False

dropForall :: Text -> Text
dropForall t = case T.stripPrefix "forall" (T.stripStart t) of
    Nothing -> t
    Just rest -> T.drop 1 (T.dropWhile (/= '.') rest)

{- | The variables a forall binds, in order, so they pair positionally with the
type applications GHC printed.
-}
foralls :: Text -> [Text]
foralls t = case T.stripPrefix "forall" (T.stripStart t) of
    Nothing -> []
    Just rest -> binders (T.takeWhile (/= '.') rest)
  where
    binders = go . T.strip
    go s
        | T.null s = []
        | otherwise = case T.uncons s of
            Just ('(', _) ->
                let (grp, more) = T.break (== ')') s
                 in T.strip (T.takeWhile (/= ':') (T.drop 1 grp)) : go (T.drop 1 more)
            _ ->
                let (v, more) = T.break isSpace s
                 in [T.strip v | not (T.null (T.strip v))] <> go (T.stripStart more)

splitConstraints :: Text -> ([Text], Text)
splitConstraints t = case breakTop "=>" t of
    Nothing -> ([], t)
    Just (ctx, rest) -> (splitTop "," (unwrap (T.strip ctx)), rest)
  where
    unwrap s = case T.stripPrefix "(" s of
        Just inner | Just body <- T.stripSuffix ")" inner -> body
        _ -> s

typeVars :: Text -> [Text]
typeVars c = drop 1 (T.words (T.strip c))

splitArrow :: Text -> [Text]
splitArrow = map T.strip . splitTop "->"

-- | Splits on a separator that appears at bracket depth zero.
splitTop :: Text -> Text -> [Text]
splitTop sep t = case breakTop sep t of
    Nothing -> [t]
    Just (before, after) -> before : splitTop sep after

breakTop :: Text -> Text -> Maybe (Text, Text)
breakTop sep t = go (0 :: Int) 0
  where
    go d i
        | i >= T.length t = Nothing
        | depthAt d i == 0 && sep `T.isPrefixOf` T.drop i t =
            Just (T.take i t, T.drop (i + T.length sep) t)
        | otherwise = go (depthAt d i) (i + 1)
    depthAt d i = case T.index t i of
        '(' -> d + 1
        '[' -> d + 1
        ')' -> d - 1
        ']' -> d - 1
        _ -> d

init' :: [a] -> [a]
init' xs = if null xs then [] else init xs

last' :: [Text] -> Text
last' xs = if null xs then "" else last xs
