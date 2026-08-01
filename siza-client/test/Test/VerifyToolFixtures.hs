{-# LANGUAGE OverloadedStrings #-}

{- | The notebooks the verify-tool properties run against, and the checks they
run: a scenario kernel that answers from its own declared bindings, and the
degenerate checks the tool reads but never evaluates.
-}
module Test.VerifyToolFixtures (
    Scenario (..),
    scVerdictReachable,
    genScenario,
    genDegenerateCheck,
    genDegenerateTrue,
    genDegenerateFalse,
    claimsValue,
    scenarioKernel,
    kernel,
    refusingKernel,
    count,
    verdictOf,
    fieldOf,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Sabela.AI.Capabilities.ToolName (ToolName (..), toolWireName)
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Test.TruthGen (genCheckOver, genDisjointNames)

{- | What the tool is asked to check, and what the notebook can answer back:
the bindings it declares, and the grade its scratch cell prints — or nothing
at all, when it refuses to commit that cell.
-}
data Scenario = Scenario
    { scCheck :: Text
    , scDefines :: [Text]
    , scGrade :: Maybe Text
    , scAboutOwn :: Bool
    }
    deriving (Show)

{- | A verdict is reachable only when the check is about a binding the notebook
declares and the scratch cell ran to a grade.
-}
scVerdictReachable :: Scenario -> Bool
scVerdictReachable sc =
    scAboutOwn sc && scGrade sc `elem` [Just "GRADE_PASS", Just "GRADE_FAIL"]

genScenario :: Gen Scenario
genScenario = do
    (others, defines) <-
        genDisjointNames 2 2
            `suchThat` (\(o, d) -> not (null o) && not (null d))
    grade <- elements [Just "GRADE_PASS", Just "GRADE_FAIL", Just "", Nothing]
    (check, aboutOwn) <-
        oneof
            [ (,) <$> genCheckOver defines <*> pure True
            , (,) <$> genCheckOver others <*> pure False
            , (,) <$> elements ["", "   "] <*> pure False
            , (,) <$> genDegenerateCheck <*> pure False
            ]
    pure (Scenario check defines grade aboutOwn)

{- | Checks carrying no lowercase identifier. The tool reads their shape and
never their value, so the constant-true and constant-false ones are the same
input to it.
-}
genDegenerateCheck :: Gen Text
genDegenerateCheck = oneof [genDegenerateTrue, genDegenerateFalse]

genDegenerateTrue :: Gen Text
genDegenerateTrue = do
    n <- tshow <$> (arbitrary :: Gen Int)
    elements ["True", n <> " == " <> n, "(" <> n <> ") >= (" <> n <> ")"]

genDegenerateFalse :: Gen Text
genDegenerateFalse = do
    n <- tshow <$> (arbitrary :: Gen Int)
    elements ["False", n <> " /= " <> n, "(" <> n <> ") > (" <> n <> ")"]

{- | Phrasings that would state the check's truth value — the one fact this
path cannot have, since it returns before the kernel is asked.
-}
claimsValue :: Text -> Bool
claimsValue note = any (`T.isInfixOf` T.toLower note) phrases
  where
    phrases =
        [ "trivially true"
        , "trivially false"
        , "always true"
        , "always false"
        , "is true"
        , "is false"
        ]

tshow :: Int -> Text
tshow = T.pack . show

{- | A notebook that declares the scenario's bindings, types them, answers
@try@ True only for the exact check (so a perturbation can falsify it), and
prints the scenario's grade from the scratch cell it agreed to commit.
-}
scenarioKernel ::
    Scenario -> IO (ToolName -> Value -> IO (Either Text ToolOutcome))
scenarioKernel sc = pure call
  where
    call tn args = pure $ case tn of
        InsertCell -> case scGrade sc of
            Nothing -> Right (ToolErr (object ["notCommitted" .= refused]))
            Just _ -> ok (object ["cellId" .= (1 :: Int)])
        ExecuteCell -> ok (object ["result" .= fromMaybe "" (scGrade sc)])
        ListCells ->
            ok
                ( object
                    [ "cells"
                        .= [ object ["id" .= i, "defines" .= [n]]
                           | (i, n) <- zip [(1 :: Int) ..] (scDefines sc)
                           ]
                    ]
                )
        CheckType -> ok (object ["result" .= typeOf (argOf "expr" args)])
        Try ->
            ok
                ( object
                    [ "type" .= ("Bool" :: Text)
                    , "stdout" .= holds (argOf "code" args)
                    ]
                )
        _ -> ok (object ["cellId" .= (1 :: Int)])
    ok = Right . ToolOk
    refused = "parse error" :: Text
    typeOf n = if n `elem` scDefines sc then n <> " :: Int" else ""
    holds code = if code == scCheck sc then "True" else "False" :: Text

{- | A kernel that answers the marker for @check@ with @grade@, any perturbed
marker with a failure, and a value probe with a number. Records tool names, so
the spec can pin the scratch-cell hygiene.
-}
kernel ::
    Text ->
    Text ->
    IO (ToolName -> Value -> IO (Either Text ToolOutcome), IORef [Text])
kernel check grade = do
    tape <- newIORef []
    lastSrc <- newIORef ""
    let call tn args = do
            modifyIORef' tape (<> [toolWireName tn])
            case tn of
                InsertCell -> do
                    modifyIORef' lastSrc (const (argOf "source" args))
                    ok (object ["cellId" .= (1 :: Int)])
                ExecuteCell -> do
                    src <- readIORef lastSrc
                    ok (object ["result" .= answerFor src])
                Try ->
                    ok
                        ( object
                            [ "type" .= ("Bool" :: Text)
                            , "stdout" .= tryValueFor (argOf "code" args)
                            ]
                        )
                ListCells ->
                    ok
                        ( object
                            [ "cells"
                                .= [ object
                                        [ "id" .= (1 :: Int)
                                        , "defines" .= (["total"] :: [Text])
                                        ]
                                   ]
                            ]
                        )
                CheckType -> ok (object ["result" .= ("total :: Int" :: Text)])
                _ -> ok (object ["cellId" .= (1 :: Int)])
    pure (call, tape)
  where
    ok = pure . Right . ToolOk
    tryValueFor src
        | src == check = if grade == "GRADE_PASS" then "True" else "False"
        | otherwise = "False" :: Text
    answerFor src
        | "CE_" `T.isInfixOf` src = "CE_0"
        | "print (" `T.isPrefixOf` T.strip src = "7"
        | check `T.isInfixOf` src = grade
        | otherwise = "GRADE_FAIL"

{- | A kernel that refuses the scratch cell, as the compile gate does whenever
the check names something not in scope.
-}
refusingKernel ::
    IO (ToolName -> Value -> IO (Either Text ToolOutcome), IORef [Text])
refusingKernel = do
    tape <- newIORef []
    let call tn _ = do
            modifyIORef' tape (<> [toolWireName tn])
            pure $ case tn of
                InsertCell ->
                    Right
                        ( ToolErr
                            (object ["notCommitted" .= ("parse error" :: Text)])
                        )
                Try ->
                    Right
                        ( ToolOk
                            ( object
                                [ "type" .= ("Bool" :: Text)
                                , "stdout" .= ("True" :: Text)
                                ]
                            )
                        )
                ListCells ->
                    Right
                        ( ToolOk
                            ( object
                                [ "cells"
                                    .= [ object
                                            [ "id" .= (99 :: Int)
                                            , "defines" .= (["total"] :: [Text])
                                            ]
                                       ]
                                ]
                            )
                        )
                _ -> Right (ToolOk (object []))
    pure (call, tape)

argOf :: Text -> Value -> Text
argOf k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argOf _ _ = ""

count :: ToolName -> [Text] -> Int
count tn = length . filter (== toolWireName tn)

verdictOf :: ToolOutcome -> Text
verdictOf = fieldOf "verdict"

fieldOf :: Text -> ToolOutcome -> Text
fieldOf k out = case toolOutcomeValue out of
    Object o -> case KM.lookup (K.fromText k) o of
        Just (String s) -> s
        _ -> ""
    _ -> ""
