{-# LANGUAGE OverloadedStrings #-}

{- | Fake notebooks that stand in for a live server behind a 'Dispatch'.
Shared by the repair-cascade specs and the shared-stack specs.
-}
module Test.StackFixtures (
    Fake,
    mockNotebook,
    blockingHoleNotebook,
    recordingNotebook,
    argText,
    hasKey,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))

type Fake = ToolCall -> IO (Either Text ToolOutcome)

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""

hasKey :: Text -> Value -> Bool
hasKey k (Object o) = KM.member (K.fromText k) o
hasKey _ _ = False

ok :: Value -> IO (Either Text ToolOutcome)
ok = pure . Right . ToolOk

{- | A notebook that records every call it is given and decides a write's fate
from its source: @boom@ is rejected, @red@ commits but runs red.
-}
recordingNotebook :: IO (Fake, IORef [ToolCall])
recordingNotebook = do
    calls <- newIORef []
    nextId <- newIORef (100 :: Int)
    let disp call@(ToolCall name argv) = do
            modifyIORef' calls (<> [call])
            case name of
                _ | name `elem` ["insert_cell", "replace_cell_source"] -> write argv
                "delete_cell" ->
                    ok (object ["deleted" .= True, "cellId" .= argInt "cell_id" argv])
                "read_cell" ->
                    ok
                        ( object
                            [ "id" .= argInt "cell_id" argv
                            , "source" .= ("total = boom" :: Text)
                            , "error" .= ("Variable not in scope: boom" :: Text)
                            ]
                        )
                "list_cells" -> ok (object ["cells" .= ([] :: [Value])])
                "discover" ->
                    ok (object ["state" .= ("miss" :: Text), "hits" .= ([] :: [Value])])
                _ -> ok (object ["result" .= ("" :: Text)])
          where
            write argv
                | "blocked" `T.isInfixOf` src
                , name == "insert_cell" =
                    pure
                        ( Right
                            ( ToolErr
                                ( object
                                    [ "notCommitted" .= ("pending-error" :: Text)
                                    , "cellId" .= (7 :: Int)
                                    ]
                                )
                            )
                        )
                | "boom" `T.isInfixOf` src =
                    pure
                        ( Right
                            ( ToolErr
                                ( object
                                    [ "notCommitted" .= src
                                    , "diagnostic" .= ("no instance for boom" :: Text)
                                    ]
                                )
                            )
                        )
                | otherwise = do
                    cid <- case argInt "cell_id" argv of
                        Just n -> pure n
                        Nothing -> do
                            modifyIORef' nextId (+ 1)
                            readIORef nextId
                    ok
                        ( object
                            [ "cellId" .= cid
                            , "execution"
                                .= object ["ok" .= not ("red" `T.isInfixOf` src)]
                            ]
                        )
              where
                src = if T.null s then argText "new_source" argv else s
                s = argText "source" argv
    pure (disp, calls)

argInt :: Text -> Value -> Maybe Int
argInt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (Number n) -> Just (round n)
    _ -> Nothing
argInt _ _ = Nothing

{- | A one-red-cell notebook whose health is decided by @verdict@ applied to
the current source. Returns the dispatch and a ref holding that source.
-}
mockNotebook :: Text -> Text -> (Text -> Bool) -> IO (Fake, IORef Text)
mockNotebook diag initSrc verdict = do
    srcRef <- newIORef initSrc
    let disp (ToolCall name argv) = case name of
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
                ok (object ["execution" .= object ["ok" .= verdict newSrc]])
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

{- | A notebook whose single cell is red on a typed hole, with @producers@
served as the find_by_type fits. Records every call it is given.
-}
blockingHoleNotebook :: [Value] -> IO (Fake, IORef [ToolCall])
blockingHoleNotebook producers = do
    calls <- newIORef []
    src <- newIORef "rendered = render (_ :: Zephyr)"
    let disp call@(ToolCall name argv) = do
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
                "find_by_type" ->
                    ok (object ["fits" .= producers, "shown" .= length producers])
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
                    ok
                        ( object
                            [ "execution"
                                .= object ["ok" .= not (T.isInfixOf "_" current)]
                            ]
                        )
                _ -> pure (Left ("unexpected tool " <> name))
    pure (disp, calls)
  where
    holeDiag :: Text
    holeDiag = "Found hole: _ :: Zephyr"
