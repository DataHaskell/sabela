{-# LANGUAGE OverloadedStrings #-}

module Eval.Repair (substituteAndVerify, repairRedCells) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Eval.HoleFit (goalFromError, parseFitNames, substituteName)
import Eval.Ollama (ToolCall (..))
import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))

type Dispatch = ToolCall -> IO (Either Text ToolOutcome)

repairRedCells ::
    Dispatch -> [(CellId, Text)] -> IO [(ToolCall, Either Text ToolOutcome)]
repairRedCells disp =
    fmap catMaybes . mapM (uncurry (substituteAndVerify disp))

substituteAndVerify ::
    Dispatch -> CellId -> Text -> IO (Maybe (ToolCall, Either Text ToolOutcome))
substituteAndVerify disp cid errText = case goalFromError errText of
    Nothing -> pure Nothing
    Just (wrong, ty) -> do
        blob <- queryHoleFits disp ty
        case parseFitNames blob of
            [] -> pure Nothing
            (fit : _) -> do
                msrc <- readCellSource disp cid
                case msrc of
                    Nothing -> pure Nothing
                    Just src
                        | newSrc == src -> pure Nothing
                        | otherwise -> do
                            let call = replaceCall cid newSrc
                            out <- disp call
                            pure (Just (call, out))
                      where
                        newSrc = substituteName wrong fit src

queryHoleFits :: Dispatch -> Text -> IO Text
queryHoleFits disp ty = do
    out <-
        disp
            ( ToolCall
                "ghci_query"
                (object ["op" .= ("holefits" :: Text), "arg" .= ("_ :: " <> ty)])
            )
    pure (strField "result" out)

readCellSource :: Dispatch -> CellId -> IO (Maybe Text)
readCellSource disp cid = do
    out <- disp (ToolCall "read_cell" (object ["cell_id" .= cid]))
    pure $ case out of
        Right (ToolOk (Object o)) -> Just (lookupText "source" o)
        _ -> Nothing

replaceCall :: CellId -> Text -> ToolCall
replaceCall cid src =
    ToolCall "replace_cell_source" (object ["cell_id" .= cid, "new_source" .= src])

strField :: Text -> Either Text ToolOutcome -> Text
strField k (Right (ToolOk (Object o))) = lookupText k o
strField _ _ = ""

lookupText :: Text -> KM.KeyMap Value -> Text
lookupText k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
