{-# LANGUAGE OverloadedStrings #-}

{- | The prose the planner produces when a notebook cannot run: duplicate
definitions and dependency cycles. Cells are named by position, because that is
what the editor's gutter shows.
-}
module Sabela.Reactivity.Errors (
    cellPositionMap,
    redefinitionErrorMsg,
    cycleErrorMsg,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Model (Cell (..), Notebook (..))
import qualified Sabela.Topo as Topo

cellPositionMap nb =
    M.fromList (zip (map cellId (nbCells nb)) [1 ..])

redefinitionErrorMsg ::
    M.Map Text Int ->
    M.Map Int Int ->
    Int ->
    [Text] ->
    Text
redefinitionErrorMsg defMap posMap _cid names =
    let msgs =
            [ "'"
                <> name
                <> "' is already defined in cell "
                <> T.pack (show (M.findWithDefault origCid origCid posMap))
                <> " (which takes precedence)"
            | name <- names
            , Just origCid <- [M.lookup name defMap]
            ]
     in "Duplicate definition"
            <> (if length names > 1 then "s" else "")
            <> ": "
            <> T.intercalate "; " msgs
            <> ". Remove the duplicate to resolve this conflict."

cycleErrorMsg ::
    M.Map Int Int ->
    S.Set Int ->
    [Cell] ->
    M.Map Text Int ->
    Text
cycleErrorMsg posMap cycleIds cells defMap =
    let cids = S.toList cycleIds
        positions =
            map (\c -> T.pack (show (M.findWithDefault c c posMap))) cids
        cycleList = T.intercalate ", " positions
        vars = cycleVariables cycleIds cells defMap
        varLine =
            if null vars
                then ""
                else
                    " Variables forming the cycle: {"
                        <> T.intercalate ", " vars
                        <> "}."
     in "This cell is part of a circular dependency and cannot execute. "
            <> "Cells in the cycle (by position): ["
            <> cycleList
            <> "]."
            <> varLine
            <> " To resolve: (1) rename one of those variables in the cell that"
            <> " introduces the loop, (2) delete one of the mutually-referencing"
            <> " cells, or (3) merge the definitions into a single cell."
            <> " Tokens inside string literals / comments are NOT counted, so"
            <> " this is a real reference loop in the code."

cycleVariables :: S.Set Int -> [Cell] -> M.Map Text Int -> [Text]
cycleVariables cycleIds cells defMap =
    let cellById = M.fromList [(cellId c, c) | c <- cells]
        cycleCells = [c | cid <- S.toList cycleIds, Just c <- [M.lookup cid cellById]]
        nameCreatesCycleEdge name =
            case M.lookup name defMap of
                Nothing -> False
                Just definerCid -> S.member definerCid cycleIds
        namesForCell c =
            let (_, uses) = Topo.cellNames (cellSource c)
             in S.filter
                    ( \n ->
                        nameCreatesCycleEdge n
                            && M.lookup n defMap /= Just (cellId c)
                    )
                    uses
        allVars = S.unions (map namesForCell cycleCells)
     in S.toAscList allVars
