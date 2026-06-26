{-# LANGUAGE OverloadedStrings #-}

module Eval.Repair (substituteAndVerify, repairRedCells) where

import Control.Monad (unless, void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Eval.HoleFit (goalFromError, substituteName)
import Eval.Ollama (ToolCall (..))
import Sabela.AI.CellResult (CellId)
import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)
import Sabela.AI.Types (ToolOutcome (..))

type Dispatch = ToolCall -> IO (Either Text ToolOutcome)

repairRedCells ::
    Dispatch -> [(CellId, Text)] -> IO [(ToolCall, Either Text ToolOutcome)]
repairRedCells disp =
    fmap catMaybes . mapM (uncurry (substituteAndVerify disp))

{- | Repair a red cell by name substitution from GHC hole-fits: query the goal
the error implies, then try each plain fit in turn, keeping the first whose
re-run compiles (a verify-and-backtrack search, not a first-fit guess). When no
fit compiles, restore the original source and give up. Refinement skeletons are
left for the sketch-completion path.
-}
substituteAndVerify ::
    Dispatch -> CellId -> Text -> IO (Maybe (ToolCall, Either Text ToolOutcome))
substituteAndVerify disp cid errText = case goalFromError errText of
    Nothing -> pure Nothing
    Just (wrong, ty) -> do
        blob <- queryHoleFits disp ty
        msrc <- readCellSource disp cid
        case msrc of
            Nothing -> pure Nothing
            Just src -> do
                let names = [hfWrite f | f <- parseHoleFits blob, not (hfRefined f)]
                    subs = nub [s | n <- names, let s = substituteName wrong n src, s /= src]
                hit <- firstGreen disp cid subs
                case hit of
                    Just out -> pure (Just out)
                    Nothing -> do
                        unless (null subs) (void (disp (replaceCall cid src)))
                        pure Nothing

{- | Replace the cell with each candidate source in turn, returning the first
whose re-run compiled; 'Nothing' if none did.
-}
firstGreen ::
    Dispatch -> CellId -> [Text] -> IO (Maybe (ToolCall, Either Text ToolOutcome))
firstGreen _ _ [] = pure Nothing
firstGreen disp cid (newSrc : rest) = do
    let call = replaceCall cid newSrc
    out <- disp call
    if compiled out then pure (Just (call, out)) else firstGreen disp cid rest

-- | True when a cell-execution outcome reports a successful run (@execution.ok@).
compiled :: Either Text ToolOutcome -> Bool
compiled (Right (ToolOk (Object o))) = case KM.lookup "execution" o of
    Just (Object e) -> KM.lookup "ok" e == Just (Bool True)
    _ -> False
compiled _ = False

queryHoleFits :: Dispatch -> Text -> IO Text
queryHoleFits disp ty = do
    out <-
        disp
            ( ToolCall
                "find_by_type"
                (object ["goal" .= ("_ :: " <> ty)])
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
