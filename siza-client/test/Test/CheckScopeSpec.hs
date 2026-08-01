{-# LANGUAGE OverloadedStrings #-}

{- | C1-1 and C1-1b: the covering check is admitted only when it is about
something this turn caused to exist, and only when the harness could actually
ground it. A failure of the verifier's own machinery is not a pass.
-}
module Test.CheckScopeSpec (checkScopeSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Chat.Verify (notebookKey)
import Siza.Agent.Check.Gate (
    CheckRefusal (..),
    referenceGate,
    refusalNote,
 )
import Siza.Agent.Check.Vet (
    CheckScope (..),
    checkScopeFor,
    emptyScope,
    ownedDefines,
    typedScope,
    vetVerdictAgainst,
 )
import Test.TruthGen (
    genCheckOver,
    genIdent,
    genDisjointNames,
    genPerturbableType,
    genUnperturbableType,
 )

checkScopeSpec :: Spec
checkScopeSpec = describe "the covering check's scope (C1-1, C1-1b)" $ do
    it "refuses a check that only reads bindings this turn did not define" $
        property $
            forAll (scopedNotebook 3 3) $ \(users, tasks) ->
                forAll (genCheckOver users) $ \check -> ioProperty $ do
                    scope <- scopeFor (kernel (users <> tasks) check) tasks
                    v <- vetVerdictAgainst (kernel (users <> tasks) check) scope check
                    pure (v `shouldSatisfy` isLeft)

    it "admits a check that reads a binding this turn defined" $
        property $
            forAll (scopedNotebook 3 3) $ \(users, tasks) ->
                forAll (genCheckOver tasks) $ \check -> ioProperty $ do
                    scope <- scopeFor (kernel (users <> tasks) check) tasks
                    v <- vetVerdictAgainst (kernel (users <> tasks) check) scope check
                    pure (v `shouldSatisfy` isRight)

    it "does not depend on which names the notebook listing happened to return first" $
        property $
            forAll (scopedNotebook 20 3) $ \(users, tasks) ->
                forAll (genCheckOver users) $ \check -> ioProperty $ do
                    scope <- scopeFor (kernel (users <> tasks) check) tasks
                    v <- vetVerdictAgainst (kernel (users <> tasks) check) scope check
                    pure (v `shouldSatisfy` isLeft)

    it "refuses every check when the harness typed nothing at all" $
        property $
            forAll (scopedNotebook 3 3) $ \(users, tasks) ->
                forAll (genCheckOver (users <> tasks)) $ \check -> ioProperty $ do
                    v <- vetVerdictAgainst (kernel [] check) emptyScope check
                    pure (v `shouldSatisfy` isLeft)

    it "refuses a check whose binding admits no perturbation" $
        property $
            forAll (scopedNotebook 1 1) $ \(_, tasks) ->
                forAll ((,) <$> genCheckOver tasks <*> genUnperturbableType) $
                    \(check, ty) -> ioProperty $ do
                        let call = kernel tasks check
                        v <- vetVerdictAgainst call (scopeOfType tasks ty) check
                        pure (v `shouldSatisfy` isLeft)

    it "admits a check whose binding a perturbation can falsify" $
        property $
            forAll (scopedNotebook 1 1) $ \(_, tasks) ->
                forAll ((,) <$> genCheckOver tasks <*> genPerturbableType) $
                    \(check, ty) -> ioProperty $ do
                        let call = kernel tasks check
                        v <- vetVerdictAgainst call (scopeOfType tasks ty) check
                        pure (v `shouldSatisfy` isRight)

    it "adding a binding the harness cannot type never turns a refusal into a pass" $
        property $
            forAll (scopedNotebook 3 3) $ \(users, tasks) ->
                forAll (genCheckOver users) $ \check -> ioProperty $ do
                    let call = kernel (users <> tasks) check
                    narrow <- scopeFor call tasks
                    wide <- scopeFor call (tasks <> ["untypable"])
                    a <- vetVerdictAgainst call narrow check
                    b <- vetVerdictAgainst call wide check
                    pure (isLeft a && isLeft b `shouldBe` True)

    describe "C1-1: the reference set is what this turn's own cells define" $ do
        it "reads it from the notebook's listing, never from another cell" $
            property $
                forAll (scopedNotebook 3 3) $ \(users, tasks) ->
                    ownedDefines (ownedIds users tasks) (definingCells users tasks)
                        `shouldBe` tasks

        it "refuses a check about a binding this turn's cells do not define" $
            property $
                forAll (scopedNotebook 3 3) $ \(users, tasks) ->
                    forAll (genCheckOver users) $ \check -> ioProperty $ do
                        let call = kernel (users <> tasks) check
                            names =
                                ownedDefines
                                    (ownedIds users tasks)
                                    (definingCells users tasks)
                        scope <- scopeFor call names
                        v <- vetVerdictAgainst call scope check
                        pure (v `shouldSatisfy` isLeft)

    describe "C1-1b: a refusal names the reason it was refused" $ do
        it "reports an empty reference set as ungrounded" $
            property $
                forAll (scopedNotebook 1 1) $ \(_, tasks) ->
                    forAll (genCheckOver tasks) $ \check ->
                        referenceGate [] check `shouldBe` Just Ungrounded

        it "carries that reason out of the vetting verdict" $
            property $
                forAll (scopedNotebook 1 1) $ \(_, tasks) ->
                    forAll (genCheckOver tasks) $ \check -> ioProperty $ do
                        v <- vetVerdictAgainst (kernel [] check) emptyScope check
                        pure (v `shouldBe` Left (refusalNote Ungrounded))

        it "reports a check that mentions nothing in scope as unreferenced, typed or not" $
            property $
                forAll (scopedNotebook 3 3) $ \(users, tasks) ->
                    forAll ((,) <$> genCheckOver users <*> genPerturbableType) $
                        \(check, ty) -> ioProperty $ do
                            let call = kernel (users <> tasks) check
                            untyped <- vetVerdictAgainst call (CheckScope tasks []) check
                            typed <- vetVerdictAgainst call (scopeOfType tasks ty) check
                            pure
                                ( [untyped, typed]
                                    `shouldBe` replicate 2 (Left (refusalNote NoReference))
                                )

    describe "C2-1f: a check outlives only the notebook it was proposed against" $ do
        it "keys on every cell's id and hash" $
            property $
                forAll (listOf ((,) <$> choose (0, 50) <*> genIdent)) $ \cells ->
                    notebookKey (listCells cells) `shouldBe` cells

        it "changes whenever any cell is added, edited or removed" $
            property $
                forAll (listOf1 ((,) <$> choose (0, 50) <*> genIdent)) $ \cells ->
                    notebookKey (listCells (drop 1 cells))
                        `shouldNotBe` notebookKey (listCells cells)

listCells :: [(Int, Text)] -> Either Text ToolOutcome
listCells cells =
    Right
        ( ToolOk
            ( object
                [ "cells"
                    .= [object ["id" .= i, "hash" .= h] | (i, h) <- cells]
                ]
            )
        )

{- | A notebook listing that puts every name in its own cell: the bindings
that were already there first, then the ones this turn wrote.
-}
definingCells :: [Text] -> [Text] -> Either Text ToolOutcome
definingCells users tasks =
    Right
        ( ToolOk
            ( object
                [ "cells"
                    .= [ object ["id" .= i, "defines" .= [n]]
                       | (i, n) <- zip [(0 :: Int) ..] (users <> tasks)
                       ]
                ]
            )
        )

-- | The cells 'definingCells' attributes to this turn.
ownedIds :: [Text] -> [Text] -> [Int]
ownedIds users tasks = take (length tasks) [length users ..]

scopedNotebook :: Int -> Int -> Gen ([Text], [Text])
scopedNotebook nUser nTask =
    genDisjointNames nUser nTask
        `suchThat` (\(u, t) -> not (null u) && not (null t))

-- | The scope the verifier is handed for this turn.
scopeFor ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    [Text] ->
    IO CheckScope
scopeFor = checkScopeFor

scopeOfType :: [Text] -> Text -> CheckScope
scopeOfType names ty = typedScope [(n, ty) | n <- names]

{- | A notebook that defines @names@, types every one of them as Int, and
answers @try@ True only for the exact check — so a perturbed check fails, as
it must for the mutation gate to mean anything.
-}
kernel ::
    [Text] -> Text -> ToolName -> Value -> IO (Either Text ToolOutcome)
kernel names check tn args =
    pure $ case tn of
        ListCells ->
            Right
                ( ToolOk
                    ( object
                        [ "cells"
                            .= [ object ["id" .= i, "defines" .= [n]]
                               | (i, n) <- zip [(0 :: Int) ..] names
                               ]
                        ]
                    )
                )
        CheckType ->
            let n = argOf "expr" args
             in Right
                    ( ToolOk
                        ( object
                            [ "result" .= (if n `elem` names then n <> " :: Int" else "")
                            ]
                        )
                    )
        Try ->
            Right
                ( ToolOk
                    ( object
                        [ "type" .= ("Bool" :: Text)
                        , "stdout"
                            .= (if argOf "code" args == check then "True" else "False" :: Text)
                        ]
                    )
                )
        _ -> Right (ToolOk (object []))

argOf :: Text -> Value -> Text
argOf k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argOf _ _ = ""
