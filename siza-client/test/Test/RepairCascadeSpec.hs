{-# LANGUAGE OverloadedStrings #-}

{- | R3-T5 end-to-end: the agent's red-cell cascade over a mock two-cell
notebook — a did-you-mean repair is kept when the notebook heals, reverted
and reported attempted-and-reverted when nothing heals (R7.5, R7.7).
-}
module Test.RepairCascadeSpec (repairCascadeSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Repair (repairRedCells)
import Siza.Agent.Repair.Blocking (repairBlockingCell)

repairCascadeSpec :: Spec
repairCascadeSpec =
    describe "the agent cascade end-to-end (verify-and-revert)" $ do
        it "keeps a did-you-mean repair that heals the notebook" $ do
            (disp, lastSrc) <- mockNotebook dymDiag origSrc goodVerdict
            fixes <- repairRedCells disp [(1, dymDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "kept"
            src <- readIORef lastSrc
            src `shouldSatisfy` T.isInfixOf "gust "
        it "reverts and reports attempted-and-reverted when nothing heals" $ do
            (disp, lastSrc) <- mockNotebook dymDiag origSrc (const False)
            fixes <- repairRedCells disp [(1, dymDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "reverted"
            src <- readIORef lastSrc
            src `shouldBe` origSrc
        it "keeps an arity permutation that heals the target (R7.5)" $ do
            (disp, lastSrc) <- mockNotebook arityDiag aritySrc arityHeals
            fixes <- repairRedCells disp [(1, arityDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "kept"
            src <- readIORef lastSrc
            src `shouldBe` "total = plot thePlot vals"
        it "reverts an arity permutation byte-identically when nothing heals" $ do
            (disp, lastSrc) <- mockNotebook arityDiag aritySrc (const False)
            fixes <- repairRedCells disp [(1, arityDiag)]
            fixes `shouldSatisfy` (not . null)
            src <- readIORef lastSrc
            src `shouldBe` aritySrc
        it "confirms a dep-add whose post-restart re-check is clean (R7.3)" $ do
            (disp, lastSrc) <- mockNotebook hiddenDiag depSrc depHeals
            fixes <- repairRedCells disp [(1, hiddenDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) -> do
                reportOf out `shouldSatisfy` T.isInfixOf "kept"
                reportOf out `shouldSatisfy` T.isInfixOf "re-check: cell clean"
                reportOf out
                    `shouldSatisfy` (not . T.isInfixOf "unconfirmed")
            src <- readIORef lastSrc
            src `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
        it "flags a dep-add kept-but-unconfirmed when the cell stays red" $ do
            -- "unvalidated" can no longer silently mean kept-while-red (R7.5).
            (disp, lastSrc) <- mockNotebook hiddenDiag depSrc (const False)
            fixes <- repairRedCells disp [(1, hiddenDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "kept-but-unconfirmed"
            src <- readIORef lastSrc
            src `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
        it "repairs the model-owned blocking cell through a real replacement" $ do
            (disp, calls) <- blockingHoleNotebook producerBlob
            fixed <- repairBlockingCell disp 7
            fixed `shouldSatisfy` maybe False (compiledReplacement 7)
            seen <- readIORef calls
            seen `shouldSatisfy` any (isReplacementOf 7)
        it "does not invent a repair when the producer result is genuinely empty" $ do
            (disp, calls) <- blockingHoleNotebook ""
            repairBlockingCell disp 7 `shouldReturn` Nothing
            seen <- readIORef calls
            seen `shouldSatisfy` (not . any (isReplacementOf 7))
  where
    goodVerdict s = "gust " `T.isInfixOf` s
    arityHeals s = "plot thePlot vals" `T.isInfixOf` s
    depHeals s = "-- cabal:" `T.isInfixOf` s

arityDiag :: Text
arityDiag =
    "• Couldn't match expected type: Plot -> [(Text, Double)] -> Text\n"
        <> "  with actual type: Text"

aritySrc :: Text
aritySrc = "total = plot vals thePlot"

hiddenDiag :: Text
hiddenDiag =
    "Could not load module `Cumulus.Plot'.\n"
        <> "It is a member of the hidden package `cumulus-0.3.1'."

depSrc :: Text
depSrc = "import Cumulus.Plot\ntotal = bars pairs thePlot"

dymDiag :: Text
dymDiag =
    "Variable not in scope: gustt :: Int -> Wind\n"
        <> "  Perhaps use `gust' (imported from Zephyr.Core)"

origSrc :: Text
origSrc = "total = gustt 3"

producerBlob :: Text
producerBlob = "Valid hole fits include\n  mkZephyr :: Zephyr"

compiledReplacement :: Int -> (ToolCall, Either Text ToolOutcome) -> Bool
compiledReplacement cid (call, Right (ToolOk (Object o))) =
    isReplacementOf cid call
        && case KM.lookup "execution" o of
            Just (Object e) -> KM.lookup "ok" e == Just (Bool True)
            _ -> False
compiledReplacement _ _ = False

isReplacementOf :: Int -> ToolCall -> Bool
isReplacementOf cid (ToolCall name (Object args)) =
    name == "replace_cell_source"
        && KM.lookup "cell_id" args == Just (Number (fromIntegral cid))
isReplacementOf _ _ = False

blockingHoleNotebook ::
    Text -> IO (ToolCall -> IO (Either Text ToolOutcome), IORef [ToolCall])
blockingHoleNotebook producers = do
    calls <- newIORef []
    src <- newIORef "rendered = render (_ :: Zephyr)"
    let ok v = pure (Right (ToolOk v))
        disp call@(ToolCall name argv) = do
            modifyIORef' calls (<> [call])
            case name of
                "read_cell" -> do
                    current <- readIORef src
                    ok
                        ( object
                            [ "id" .= (7 :: Int)
                            , "source" .= current
                            , "error" .= holeDiag
                            ]
                        )
                "find_by_type" -> ok (object ["result" .= producers])
                "list_cells" -> do
                    current <- readIORef src
                    ok
                        ( object
                            [ "cells"
                                .= [ object
                                        [ "id" .= (7 :: Int)
                                        , "source" .= current
                                        , "defines" .= (["rendered"] :: [Text])
                                        , "hasError" .= T.isInfixOf "_" current
                                        ]
                                   ]
                            ]
                        )
                "replace_cell_source" -> do
                    let current = argText "new_source" argv
                    modifyIORef' src (const current)
                    ok (object ["execution" .= object ["ok" .= not (T.isInfixOf "_" current)]])
                _ -> pure (Left ("unexpected tool " <> name))
    pure (disp, calls)
  where
    holeDiag :: Text
    holeDiag = "Found hole: _ :: Zephyr"
    argText k (Object o) = case KM.lookup (K.fromText k) o of
        Just (String s) -> s
        _ -> ""
    argText _ _ = ""

reportOf :: Either Text ToolOutcome -> Text
reportOf (Right (ToolOk (Object o))) = case KM.lookup "repair" o of
    Just (String s) -> s
    _ -> ""
reportOf _ = ""

{- | A two-cell mock notebook: cell 1 is red until a replacement satisfying
the verdict lands; cell 2 stays clean. Returns the dispatch plus cell 1's
last-committed source.
-}
mockNotebook ::
    Text ->
    Text ->
    (Text -> Bool) ->
    IO (ToolCall -> IO (Either Text ToolOutcome), IORef Text)
mockNotebook diag initSrc verdict = do
    srcRef <- newIORef initSrc
    let ok v = pure (Right (ToolOk v))
        disp (ToolCall name argv) = case name of
            "read_cell" -> do
                s <- readIORef srcRef
                ok (object ["id" .= (1 :: Int), "source" .= s, "error" .= diag])
            "list_cells" -> do
                s <- readIORef srcRef
                ok
                    ( object
                        [ "cells"
                            .= [ object
                                    [ "id" .= (1 :: Int)
                                    , "source" .= s
                                    , "defines" .= (["total"] :: [Text])
                                    , "hasError" .= not (verdict s)
                                    ]
                               , object
                                    [ "id" .= (2 :: Int)
                                    , "source" .= ("sib = 1" :: Text)
                                    , "defines" .= (["sib"] :: [Text])
                                    , "hasError" .= False
                                    ]
                               ]
                        ]
                    )
            "replace_cell_source" -> do
                let newSrc = argText "new_source" argv
                modifyIORef' srcRef (const newSrc)
                ok
                    ( object
                        [ "execution"
                            .= object ["ok" .= verdict newSrc]
                        ]
                    )
            "find_by_type" -> ok (object ["result" .= ("" :: Text)])
            "discover" ->
                ok
                    ( object
                        [ "state" .= ("found" :: Text)
                        , "hits"
                            .= [ object
                                    [ "name" .= ("gust" :: Text)
                                    , "module" .= ("Zephyr.Core" :: Text)
                                    , "package" .= ("zephyr" :: Text)
                                    ]
                               ]
                        ]
                    )
            _ -> pure (Left ("unexpected tool " <> name))
    pure (disp, srcRef)
  where
    argText k (Object o) = case KM.lookup (K.fromText k) o of
        Just (String s) -> s
        _ -> ""
    argText _ _ = ""
