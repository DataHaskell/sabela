{-# LANGUAGE OverloadedStrings #-}

{- | Guidance must describe the source it is about (harness-truth C1-4b, and
the residual lexical half of C1-4a). Every property here is stated over
generated identifiers, modules and packages, so it holds for any library.
-}
module Test.AdviceTruthSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.Set as Set

import Sabela.AI.Capabilities.Bindings (notebookSource)
import Sabela.AI.Capabilities.Edit.Run (executeGuidance)
import Sabela.AI.Capabilities.Query (guidedOutcome)
import Sabela.AI.Capabilities.Scratchpad (scratchpadPayload)
import Sabela.AI.CellResult (CellOutcome (..), CellResult (..))
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Diagnose (
    Guidance (..),
    cellResultWithExtraGuidance,
    diagnoseWith,
    guidanceForCell,
    topLevelLetMessage,
 )
import Sabela.Diagnose.Parse (declaredPackages)
import Sabela.Errors (scrubHarnessFrames)
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    bareCellError,
 )
import Sabela.Parse (validateCellShape)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..), newApp)
import Sabela.State.NotebookStore (modifyNotebook)
import Test.AdviceGen (
    GenCell (..),
    cabalLine,
    genCell,
    genDiagnostic,
    genFramedDiagnostic,
    genHiddenPackageError,
    genLetParseError,
    harnessFrameBinder,
    located,
 )
import Test.AdviceWitness (
    categoryWitnesses,
    declareImperative,
    exemplars,
    preconditionHolds,
 )
import Test.HarnessGen (genPackageName)

{- | Every surface that returns guidance, as the payload builder production
calls, read back from the payload the model receives. A surface that dropped
the source would fail these properties rather than quietly stop refining.
-}
emitters :: [(String, Text -> Text -> [Guidance])]
emitters =
    [ ("compile gate", diagnoseWith Nothing)
    , ("execute_cell", \src err -> guidanceForCell src (rejecting err))
    ,
        ( "execute_cell (payload)"
        , \src err ->
            guidanceIn (cellResultWithExtraGuidance src [] (rejecting err))
        )
    ,
        ( "check_type"
        , \src err -> guidanceIn (toolOutcomeValue (guidedOutcome src [] err))
        )
    , ("scratchpad", \src err -> guidanceIn (scratchpadFor src err))
    ]

-- | The scratchpad payload as production builds it, with compaction stubbed.
scratchpadFor :: Text -> Text -> Value
scratchpadFor src err =
    runIdentity (scratchpadPayload compact src "" err)
  where
    compact :: Text -> Identity Value
    compact = pure . String

-- | What a payload puts under a key, as the model reads it back off the wire.
fieldIn :: Text -> Value -> Maybe Value
fieldIn k (Object o) = KM.lookup (Key.fromText k) o
fieldIn _ _ = Nothing

-- | The guidance a payload carries, as the model reads it back off the wire.
guidanceIn :: Value -> [Guidance]
guidanceIn (Object o) = case KM.lookup "guidance" o of
    Just (Array gs) -> [g | Object e <- foldr (:) [] gs, Just g <- [entry e]]
    _ -> []
  where
    entry e = case (KM.lookup "category" e, KM.lookup "message" e) of
        (Just (String c), Just (String m)) -> Just (Guidance c m)
        _ -> Nothing
guidanceIn _ = []

rejecting :: Text -> CellResult
rejecting err = CellResult (Rejected [bareCellError Nothing Nothing err]) [] []

{- | A notebook whose prose quotes a @build-depends:@ line, ahead of one code
cell. Prose declares nothing, whatever it quotes.
-}
proseAnd :: Text -> Notebook
proseAnd code =
    Notebook
        "n.md"
        [ Cell 1 ProseCell Haskell "-- cabal: build-depends: proseonly\n" [] Nothing False
        , Cell 2 CodeCell Haskell code [] Nothing False
        ]

{- | The execute_cell surface for real: a notebook holding one cell, and the
guidance the handler builds for a failure in it. Nothing hands the classifier
a source — it reads the cell.
-}
guidanceOnCell :: Text -> Text -> IO [Guidance]
guidanceOnCell src err = do
    app <- newApp "." Set.empty Nothing Nothing []
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = [cell]})
    executeGuidance app 1 (rejecting err) []
  where
    cell = Cell 1 CodeCell Haskell src [] Nothing False

-- | A hidden-package wall, over a package no test declares by accident.
hidden :: Text
hidden =
    "<no location info>: error: [GHC-87110]\n\
    \    Could not load module \8216Some.Module\8217.\n\
    \    It is a member of the hidden package \8216somepkg-1.2.3\8217."

overEmitters :: (String -> (Text -> Text -> [Guidance]) -> Property) -> Property
overEmitters k = conjoin [counterexample name (k name emit) | (name, emit) <- emitters]

spec :: Spec
spec = describe "guidance describes the source it is about" $ do
    it "every category the classifier emits states what backs it (C1-4b)" $
        map fst exemplars `shouldMatchList` map fst categoryWitnesses

    it "every category is reachable on every surface (C1-4b)" $
        mapM_
            ( \(cat, (src, err)) ->
                mapM_
                    ( \(name, emit) ->
                        (name, cat, map gCategory (emit src err))
                            `shouldSatisfy` \(_, _, cats) -> cat `elem` cats
                    )
                    emitters
            )
            exemplars

    it "execute_cell reads the advised source from the notebook (C1-4b)" $ do
        gs <-
            guidanceOnCell "-- cabal: build-depends: somepkg\nimport Some.Module\n" hidden
        map gMessage gs `shouldSatisfy` not . any (T.isInfixOf "FIRST line")
        map gMessage gs `shouldSatisfy` any (T.isInfixOf "somepkg")

    it "reports what a cell declares without auditing those packages" $ do
        let src = "-- cabal: build-depends: somepkg, otherpkg\nimport Some.Module\n"
            gs =
                diagnoseWith
                    Nothing
                    src
                    (located "Could not find module \8216Some.Module\8217")
            joined = T.intercalate " | " (map gMessage gs)
        map gCategory gs `shouldBe` ["missing-dependency"]
        joined `shouldSatisfy` T.isInfixOf "somepkg"
        joined `shouldNotSatisfy` T.isInfixOf "is not exposed by"
        joined `shouldNotSatisfy` T.isInfixOf "does not expose"

    it "execute_cell still reports a real top-level let (C1-4b)" $ do
        gs <-
            guidanceOnCell "let x = 5\n" (located "parse error on input \8216let\8217")
        map gCategory gs `shouldBe` ["top-level-let"]

    prop
        "no advice category fires when the source contradicts it (C1-4b)"
        $ forAll genCell
        $ \c -> forAll genDiagnostic $ \err ->
            overEmitters $ \_ emit ->
                conjoin
                    [ counterexample (show (gCategory g, gMessage g, cellText c)) $
                        property (preconditionHolds c err g)
                    | g <- emit (cellText c) err
                    ]

    prop
        "never tells a cell to declare a package it already declares (C1-4b)"
        $ forAll genPackageName
        $ \pkg -> forAll (genHiddenPackageError pkg) $ \err ->
            forAll genCell $ \c0 ->
                let c = declaring pkg c0
                 in overEmitters $ \_ emit ->
                        conjoin
                            [ counterexample (T.unpack (gMessage g)) $
                                property
                                    (not (declareImperative pkg `T.isInfixOf` gMessage g))
                            | g <- emit (cellText c) err
                            ]

    prop
        "a scratchpad reports the compiler's own stderr and adds nothing (C1-4b)"
        $ forAll genCell
        $ \c -> forAll (oneof [genDiagnostic, genFramedDiagnostic]) $ \err ->
            fieldIn "stderr" (scratchpadFor (cellText c) err)
                === Just (String (scrubHarnessFrames err))

    prop
        "a scratchpad payload names no binder the harness invented (C1-4f)"
        $ forAll genCell
        $ \c -> forAll genFramedDiagnostic $ \err ->
            let shown = T.pack (show (scratchpadFor (cellText c) err))
             in counterexample (T.unpack shown) $
                    property (not (harnessFrameBinder `T.isInfixOf` shown))

    prop
        "list_bindings reads its source from code cells, never prose (C1-4b)"
        $ forAll genCell
        $ \c ->
            declaredPackages (notebookSource (proseAnd (cellText c)))
                === cellDeclares c

    prop
        "let advice agrees with the notebook's own top-level-let check (C1-4a)"
        $ forAll genCell
        $ \c -> forAll genLetParseError $ \err ->
            let fired =
                    "top-level-let"
                        `elem` map gCategory (diagnoseWith Nothing (cellText c) err)
                notebookSays =
                    validateCellShape CodeCell (cellText c) == Just topLevelLetMessage
             in counterexample (T.unpack (cellText c)) (fired === notebookSays)

    prop
        "a cell that writes no let is never told about one (C1-4a)"
        $ forAll genCell
        $ \c -> forAll genLetParseError $ \err ->
            not (cellWritesLet c) ==>
                overEmitters
                    ( \_ emit ->
                        property
                            ( "top-level-let"
                                `notElem` map gCategory (emit (cellText c) err)
                            )
                    )

declaring :: Text -> GenCell -> GenCell
declaring pkg c =
    c
        { cellText =
            T.unlines
                ( cabalLine (pkg : cellDeclares c)
                    <> drop (length (cabalLine (cellDeclares c))) (T.lines (cellText c))
                )
        , cellDeclares = pkg : cellDeclares c
        }
