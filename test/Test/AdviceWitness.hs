{-# LANGUAGE OverloadedStrings #-}

{- | What backs each advice category. A category's witness is what the harness
must be able to show for the pair it advised on; a category with no witness
fails, so a new rule cannot be waved through by defaulting to True.
-}
module Test.AdviceWitness (
    categoryWitnesses,
    exemplars,
    preconditionHolds,
    declareImperative,
) where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Unshowable (baseTypeName, unshowableShowType)
import Sabela.Diagnose (Guidance (..), ambiguousTypeMessage)
import Sabela.Diagnose.Packages (resolvePackageToken)
import Sabela.Diagnose.Parse (declaredPackages, hiddenPackage, quotedToken)
import Test.AdviceGen (GenCell (..), located)
import Test.HarnessGen (packageNames)

{- | One (source, diagnostic) pair per category that must produce it. Every
surface is asked for all of them, so a category cannot go unreachable on one
surface — the way `top-level-let` did for every source-blind caller.
-}
exemplars :: [(Text, (Text, Text))]
exemplars =
    [
        ( "top-level-let"
        , ("let x = 5\n", located "parse error on input \8216let\8217")
        )
    ,
        ( "missing-dependency"
        , ( "import Some.Module\n"
          , "<no location info>: error: [GHC-87110]\n\
            \    Could not load module \8216Some.Module\8217.\n\
            \    It is a member of the hidden package \8216somepkg-1.2.3\8217."
          )
        )
    ,
        ( "did-you-mean"
        , ( "answer = lenght xs\n"
          , located "Variable not in scope: lenght\n    Perhaps you meant \8216length\8217"
          )
        )
    ,
        ( "ambiguous-type"
        , ( "answer = show (read \"1\")\n"
          , located "Ambiguous type variable \8216a0\8217 arising from a use of \8216show\8217"
          )
        )
    ,
        ( "type-mismatch"
        , ( "answer = length \"xs\"\n"
          , located "Couldn't match expected type \8216Int\8217 with actual type \8216[Char]\8217"
          )
        )
    ,
        ( "unshowable-result"
        , ( "answer = mystery\n"
          , located "No instance for `Show Wind' arising from a use of `print'"
          )
        )
    ]

categoryWitnesses :: [(Text, GenCell -> Text -> Guidance -> Bool)]
categoryWitnesses =
    [ ("top-level-let", \c _ _ -> cellWritesLet c)
    , ("missing-dependency", namesOnlyGroundedPackages)
    , ("did-you-mean", \_ err g -> quotesFrom err g)
    , ("ambiguous-type", \_ _ g -> isFixedAdvice ambiguousTypeMessage g)
    , ("type-mismatch", \_ err g -> quotesFrom err g)
    , ("unshowable-result", \_ err g -> namesTheUnshowableType err g)
    ]

preconditionHolds :: GenCell -> Text -> Guidance -> Bool
preconditionHolds c err g = case lookup (gCategory g) categoryWitnesses of
    Just holds -> holds c err g
    Nothing -> False

-- | The unconditional form of "declare this package", for a given package.
declareImperative :: Text -> Text
declareImperative pkg =
    "Add this as the FIRST line of the cell: -- cabal: build-depends: " <> pkg

{- | Every package the message names is one the harness really has: declared by
the source, reported by the diagnostic, or the store's answer for the module
the diagnostic names. The imperative may name only one the source lacks.
-}
namesOnlyGroundedPackages :: GenCell -> Text -> Guidance -> Bool
namesOnlyGroundedPackages c err g =
    all grounded (filter (`T.isInfixOf` gMessage g) packageNames)
        && not (any (\p -> declareImperative p `T.isInfixOf` gMessage g) declared)
  where
    declared = declaredPackages (cellText c)
    grounded p = any (p `T.isInfixOf`) known
    known =
        declared
            <> maybeToList (hiddenPackage err)
            <> maybeToList (resolvePackageToken =<< quotedToken err)

{- | Passed-through advice must be GHC's own words: every word of the message
occurs in the diagnostic it was read from.
-}
quotesFrom :: Text -> Guidance -> Bool
quotesFrom err g = all (`elem` T.words err) (T.words (gMessage g))

{- | Fixed advice names nothing from either input, so it can contradict
neither. The witness is that the message really is that constant.
-}
isFixedAdvice :: Text -> Guidance -> Bool
isFixedAdvice constant g = gMessage g == constant

{- | The message names the very type the diagnostic reports as unshowable,
rather than one of the harness's own choosing.
-}
namesTheUnshowableType :: Text -> Guidance -> Bool
namesTheUnshowableType err g = case baseTypeName <$> unshowableShowType err of
    Just ty -> ty `T.isInfixOf` gMessage g
    Nothing -> False
