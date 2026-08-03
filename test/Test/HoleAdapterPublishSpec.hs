{-# LANGUAGE OverloadedStrings #-}

{- | What a repair may say about the names it publishes. Every claim is checked
against the payload's own text rather than against the compiler blobs the
harness read: a name the harness saw is not a name the caller was shown.
-}
module Test.HoleAdapterPublishSpec (spec) where

import Data.Aeson (Value (..), object)
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Sabela.AI.Capabilities.Edit.HoleRewrite (holeFitCap, holeName)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Repair (
    diagnosticRepairPairs,
    repairPairs,
 )
import Test.HoleAdapterGen (
    ArgSource (..),
    genArgSource,
    genPrintedType,
    genQualified,
    genTwo,
    genTypeName,
 )
import Test.HoleAdapterRig (arrow, diagWith)
import Test.HoleRewriteGen (field, genIdent, namedHoleBlob)

spec :: Spec
spec = do
    printedThroughSpec
    fitTypeSpec

-- The module a published name was printed through -----------------------------

printedThroughSpec :: Spec
printedThroughSpec = describe "the modules a repair names as the compiler's" $ do
    prop "names the module of a name the payload prints without one" $
        forAll genArgSource $ \as ->
            forAll genShortened $ \(qualified, modulePath, entity) ->
                forAll genIdent $ \conv -> monadicIO $ do
                    let ty = arrow "Aa" qualified
                    pairs <- run (repairOf as ty [(conv, ty)])
                    assert
                        ( modulesReported pairs == [modulePath]
                            && holeTypeOf pairs == Just (arrow "Aa" entity)
                            && fitTypes pairs == [arrow "Aa" entity]
                        )

    prop "says nothing about a name it read but never printed" $
        forAll genArgSource $ \as ->
            forAll genQualified $ \(qualified, _) ->
                forAll genTwo $ \(conv, ty) -> monadicIO $ do
                    let bound = "\n      Relevant bindings include qq :: " <> qualified
                        goal = arrow "Aa" (T.toUpper (T.take 1 ty) <> T.drop 1 ty)
                    pairs <-
                        run
                            ( repairPairs
                                (const (pure (namedHoleBlob holeName goal [(conv, goal)])))
                                (diagWith as "Ee" "Aa" <> bound)
                                (asSrc as)
                            )
                    assert (null (modulesReported pairs))

    prop "says nothing about a fit it dropped at the cap" $
        forAll genArgSource $ \as ->
            forAll genShortened $ \(qualified, _, _) ->
                forAll genIdent $ \conv -> monadicIO $ do
                    let plain = arrow "Aa" "Bb"
                        kept = [(conv <> T.pack (show i), plain) | i <- [1 .. holeFitCap]]
                        over = (conv, arrow "Aa" qualified)
                    pairs <- run (repairOf as plain (kept <> [over]))
                    assert (null (modulesReported pairs))

    prop "says nothing where the same plain name also stands unqualified" $
        forAll genArgSource $ \as ->
            forAll genShortened $ \(qualified, _, entity) ->
                forAll genIdent $ \conv -> monadicIO $ do
                    let ty = arrow "Aa" qualified
                        twin = (conv <> "z", arrow "Aa" entity)
                    pairs <- run (repairOf as ty [(conv, ty), twin])
                    assert (null (modulesReported pairs))

-- A published fit's type ------------------------------------------------------

fitTypeSpec :: Spec
fitTypeSpec = describe "the type a repair publishes for a fit" $ do
    prop "ends where the compiler's note about the name begins" $
        forAll genTwo $ \(conv, _) ->
            forAll genPrintedType $ \(ty, _) ->
                forAll (choose (1, 40 :: Int)) $ \col ->
                    let note = " (defined at <interactive>:7:" <> T.pack (show col) <> ")"
                        block = fitBlock (conv <> " :: " <> ty <> note)
                     in fitTypes (diagnosticRepairPairs block) === [ty]

    prop "does not run on into the source the compiler echoed" $
        forAll genTwo $ \(conv, binder) ->
            forAll genPrintedType $ \(ty, _) ->
                let echoed = "\n  |\n6 | " <> binder <> " = _\n  |     ^"
                    block = fitBlock (conv <> " :: " <> ty) <> echoed
                 in fitTypes (diagnosticRepairPairs block) === [ty]

-- Rig -------------------------------------------------------------------------

{- | A name the compiler printed through a package, and through a module whose
own naming makes the harness shorten it away. The disclosure has something to
disclose only where the payload really does print the plain name.
-}
genShortened :: Gen (Text, Text, Text)
genShortened = do
    (qualified, modulePath) <- genQualified
    seg <- genTypeName
    entity <- genTypeName
    let unit = fst (T.breakOn ":" qualified)
        path = modulePath <> ".Internal." <> seg
    pure (unit <> ":" <> path <> "." <> entity, path, entity)

-- | The repair a rejection earns once a probe has answered with these fits.
repairOf :: ArgSource -> Text -> [(Text, Text)] -> IO [Pair]
repairOf as ty fits =
    repairPairs
        (const (pure (namedHoleBlob holeName ty fits)))
        (diagWith as "Ee" "Aa")
        (asSrc as)

-- | One rejection carrying a hole fit list, as GHC prints one.
fitBlock :: Text -> Text
fitBlock entry =
    "<interactive>:6:10: error: [GHC-88464]\n\
    \    \8226 Found hole: _ :: Aa\n\
    \      Valid hole fits include\n        "
        <> entry

{- | The types the payload publishes for its fits, wherever the fits stand: a
rewrite's own object, or the top-level list a rejection already carried.
-}
fitTypes :: [Pair] -> [Text]
fitTypes pairs = typesUnder (object pairs) <> maybe [] typesUnder (rewriteOf pairs)
  where
    typesUnder v = case field "holeFits" v of
        Just (Array xs) -> [t | x <- foldr (:) [] xs, Just (String t) <- [field "type" x]]
        _ -> []

holeTypeOf :: [Pair] -> Maybe Text
holeTypeOf pairs = case rewriteOf pairs >>= field "holeType" of
    Just (String t) -> Just t
    _ -> Nothing

rewriteOf :: [Pair] -> Maybe Value
rewriteOf = field "holeRewrite" . object

modulesReported :: [Pair] -> [Text]
modulesReported pairs =
    case field "printedThrough" (object pairs) >>= field "modules" of
        Just (Array xs) -> [m | String m <- foldr (:) [] xs]
        _ -> []
