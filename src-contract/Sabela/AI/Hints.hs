{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Hints (
    Hint (..),
    RenameCandidate (..),
    parseHints,
    renameHints,
    extensionHints,
    expectedTypeOf,
    knownExtensions,
    sameNameClass,
) where

import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (listToMaybe, mapMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleFits (stripPackageQualifiers)

data RenameCandidate = RenameCandidate
    { rcName :: Text
    , rcKind :: Text
    , rcProvenance :: Text
    }
    deriving (Eq, Show)

{- | Whether one name may textually replace another. A variable and a
constructor are different syntactic classes, so swapping one for the other
cannot parse where the original stood. Qualified names judge their last part.
-}
sameNameClass :: Text -> Text -> Bool
sameNameClass wrong replacement =
    startsUpper (unqualify wrong) == startsUpper (unqualify replacement)
  where
    unqualify = T.takeWhileEnd (/= '.')
    startsUpper t = maybe False (isUpper . fst) (T.uncons t)

data Hint
    = HintRename Text [RenameCandidate]
    | HintExtension Text
    | HintDeclPlacement Text
    | HintOther Text
    deriving (Eq, Show)

parseHints :: Text -> [Hint]
parseHints err
    | null blocks
    , inlineShaped err =
        classify (contextBefore 0) (T.lines err)
    | otherwise = concat [classify (contextBefore i) b | (i, b) <- blocks]
  where
    ls = T.lines err
    blocks = fixBlocks ls
    contextBefore i = T.unlines (take i ls)
    inlineShaped t =
        "Perhaps use" `T.isInfixOf` t
            || "intended to use" `T.isInfixOf` t
            || "Move the type signature" `T.isInfixOf` t

renameHints :: Text -> [(Text, [RenameCandidate])]
renameHints err = [(w, cs) | HintRename w cs <- parseHints err]

extensionHints :: Text -> [Text]
extensionHints err = [e | HintExtension e <- parseHints err]

fixBlocks :: [Text] -> [(Int, [Text])]
fixBlocks ls =
    [ (i, sameLine rest <> takeWhile deeper (drop (i + 1) ls))
    | (i, l) <- zip [0 ..] ls
    , Just rest <- [T.stripPrefix "Suggested fix:" (T.strip l)]
    , let markerIndent = T.length (T.takeWhile (== ' ') l)
          deeper x =
            not (T.null (T.strip x))
                && T.length (T.takeWhile (== ' ') x) > markerIndent
    ]
  where
    sameLine rest = [T.strip rest | not (T.null (T.strip rest))]

classify :: Text -> [Text] -> [Hint]
classify context blockLines
    | Just ext <- extensionIn body = [HintExtension ext]
    | Just target <- declPlacementIn body = [HintDeclPlacement target]
    | "Perhaps use" `T.isInfixOf` body =
        case (wrongIn context, candidatesIn afterUse) of
            (Just w, cs@(_ : _)) -> [HintRename w cs]
            (Nothing, cs@(_ : _)) -> [HintRename "" cs]
            _ -> [HintOther (T.strip body)]
    | T.null (T.strip body) = []
    | otherwise = [HintOther (T.strip body)]
  where
    body = T.unlines blockLines
    afterUse = case T.breakOn "Perhaps use" body of
        (_, rest) -> T.drop (T.length ("Perhaps use" :: Text)) rest

extensionIn :: Text -> Maybe Text
extensionIn body = do
    rest <- afterInfix "intended to use " body
    let tok = T.takeWhile isAlphaNum (T.dropWhile (not . isUpper) rest)
    if tok `elem` knownExtensions then Just tok else Nothing

declPlacementIn :: Text -> Maybe Text
declPlacementIn body = do
    rest <- afterInfix "Move the type signature to the declaration site of" body
    listToMaybe (quotedTokens rest)

wrongIn :: Text -> Maybe Text
wrongIn context = do
    nearest <-
        listToMaybe
            ( reverse
                [ T.unlines (drop i (T.lines context))
                | (i, l) <- zip [0 ..] (T.lines context)
                , "ot in scope:" `T.isInfixOf` l
                ]
            )
    rest <- afterInfixCI "not in scope:" nearest
    let tok =
            T.takeWhile
                (\c -> c /= ' ' && c /= '\n')
                (T.strip (stripKinds (T.strip rest)))
    if T.null tok then Nothing else Just (dequote tok)
  where
    stripKinds t =
        foldr
            (\p acc -> maybe acc T.strip (T.stripPrefix p acc))
            t
            ["type constructor or class", "data constructor", "record field"]
    dequote t0 =
        let t1 = T.dropAround (\c -> c == '\8216' || c == '\8217') t0
         in case T.uncons t1 of
                Just ('`', rest)
                    | Just (body, '\'') <- T.unsnoc rest -> body
                _ -> t1

candidatesIn :: Text -> [RenameCandidate]
candidatesIn body = case go "\8216" "\8217" body of
    [] -> go "`" "'" body
    cs -> cs
  where
    go open close t = case T.breakOn open t of
        (_, rest) | T.null rest -> []
        (before, rest) ->
            let (nm, r2) = T.breakOn close (T.drop 1 rest)
             in if T.null r2
                    then []
                    else
                        RenameCandidate nm (kindIn before) (provIn (T.drop 1 r2))
                            : go open close (T.drop 1 r2)
    kindIn before
        | "record field" `T.isInfixOf` before = "record-field"
        | "data constructor" `T.isInfixOf` before = "data-constructor"
        | "type constructor" `T.isInfixOf` before = "type"
        | otherwise = "value"
    provIn after = case T.breakOn "(" (T.takeWhile (/= '\n') after) of
        (lead, rest)
            | T.null rest || not (T.null (T.strip lead)) -> ""
            | otherwise -> T.strip (T.takeWhile (/= ')') (T.drop 1 rest))

{- | The type a mismatch says was wanted, in each of the layouts GHC prints it:
the @Expected:@ block, the inline @expected type … with actual type@ sentence
in either quoting, and the bare @Couldn't match type ‘T’ with ‘U’@.
-}
expectedTypeOf :: Text -> Maybe Text
expectedTypeOf err =
    fmap stripPackageQualifiers . listToMaybe $
        expectedBlock err
            <> typeAfter "Couldn't match expected type" " with actual type" err
            <> typeAfter "Couldn't match type" " with" err

-- | GHC's two-line layout, where the wanted type is indented under @Expected:@.
expectedBlock :: Text -> [Text]
expectedBlock err = mapMaybe expectedAt (zip [0 ..] ls)
  where
    ls = T.lines err
    expectedAt (i, l) = do
        rest <- T.stripPrefix "Expected:" (T.strip l)
        let col = T.length (T.takeWhile (== ' ') l)
            continuation =
                takeWhile
                    ( \x ->
                        T.length (T.takeWhile (== ' ') x) > col
                            && not ("Actual:" `T.isPrefixOf` T.strip x)
                    )
                    (drop (i + 1) ls)
            joined =
                T.unwords
                    (concatMap T.words (T.strip rest : map T.strip continuation))
        if T.null joined then Nothing else Just joined

{- | The type standing after a marker, written either inside GHC's own quotes
or after a colon and running to the marker that ends it. Lines are rejoined
first, because GHC wraps a long type onto the following line either way.
-}
typeAfter :: Text -> Text -> Text -> [Text]
typeAfter marker terminator err =
    [ ty
    | rest <- afterEach marker (T.unwords (T.words err))
    , ty <- quotedForm rest <> colonForm rest
    , not (T.null ty)
    ]
  where
    quotedForm rest = [b | Just b <- [quotedHead (T.stripStart rest)]]
    colonForm rest =
        [ T.strip (fst (T.breakOn terminator body))
        | Just body <- [T.stripPrefix ":" (T.stripStart rest)]
        ]

-- | The fragment GHC quoted at the head of some text, in either quoting.
quotedHead :: Text -> Maybe Text
quotedHead t =
    listToMaybe
        [ fst (T.breakOn close rest)
        | (open, close) <- [("\8216", "\8217"), ("`", "'")]
        , Just rest <- [T.stripPrefix open t]
        , close `T.isInfixOf` rest
        ]

afterEach :: Text -> Text -> [Text]
afterEach needle t =
    [ T.drop (i + T.length needle) t
    | i <- [0 .. T.length t - 1]
    , needle `T.isPrefixOf` T.drop i t
    ]

knownExtensions :: [Text]
knownExtensions =
    [ "OverloadedStrings"
    , "TemplateHaskell"
    , "ScopedTypeVariables"
    , "TypeApplications"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "MultiParamTypeClasses"
    , "RankNTypes"
    , "GADTs"
    , "DataKinds"
    , "DeriveFunctor"
    , "DeriveGeneric"
    , "DeriveAnyClass"
    , "GeneralizedNewtypeDeriving"
    , "DerivingStrategies"
    , "StandaloneDeriving"
    , "TupleSections"
    , "LambdaCase"
    , "MultiWayIf"
    , "RecordWildCards"
    , "NamedFieldPuns"
    , "BangPatterns"
    , "ViewPatterns"
    , "ConstraintKinds"
    , "KindSignatures"
    , "PolyKinds"
    , "TypeFamilies"
    , "TypeOperators"
    , "InstanceSigs"
    , "ExistentialQuantification"
    , "FunctionalDependencies"
    , "QuasiQuotes"
    , "BlockArguments"
    , "ImportQualifiedPost"
    , "NumericUnderscores"
    , "OverloadedLabels"
    ]

afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest)
        | T.null rest -> Nothing
        | otherwise -> Just (T.drop (T.length needle) rest)

afterInfixCI :: Text -> Text -> Maybe Text
afterInfixCI needle t =
    (\i -> T.drop (i + T.length needle) t) <$> indexOfCI
  where
    indexOfCI =
        let (pre, rest) = T.breakOn (T.toLower needle) (T.toLower t)
         in if T.null rest then Nothing else Just (T.length pre)

quotedTokens :: Text -> [Text]
quotedTokens t = case T.breakOn "\8216" t of
    (_, rest) | T.null rest -> []
    (_, rest) ->
        let (tok, r2) = T.breakOn "\8217" (T.drop 1 rest)
         in if T.null r2 then [] else tok : quotedTokens (T.drop 1 r2)
