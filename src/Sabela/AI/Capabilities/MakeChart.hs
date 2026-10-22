{-# LANGUAGE OverloadedStrings #-}

{- | @make_chart@: a spec in, a canonical chart cell out, landed through the
same insert and replace paths as any other cell, so the compile gate, the
acknowledgement and the run all apply unchanged.
-}
module Sabela.AI.Capabilities.MakeChart (
    execMakeChart,
    chartCallSource,
) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit (execInsertCell)
import Sabela.AI.Capabilities.Edit.Ack (withNote)
import Sabela.AI.Capabilities.Edit.Replace (execReplaceCellSource)
import Sabela.AI.Capabilities.Util (fieldInt, fieldText)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ToolOutcome (..), errOutcome)
import Sabela.Api (errorJsonWith)
import Sabela.Api.Viz (VizSpec (..))
import Sabela.Api.Viz.Source (specToSource)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.LLM.Cancel (CancelToken)
import Sabela.State (App)

{- | The cell text for a call, or why the call does not fit the chart
language. The call's own keys (@data@, @cell_id@, …) are set aside before
the spec is decoded, so an unknown key is reported by name.
-}
chartCallSource :: Value -> Either Text Text
chartCallSource input = case input of
    Object o ->
        let d = T.strip (fieldText "data" input)
            specObj = Object (foldr KM.delete o callKeys)
         in if T.null d
                then
                    Left
                        "data is required: a Haskell expression for the rows, usually a binding name."
                else case fromJSON specObj of
                    Error e -> Left (T.pack e)
                    Success spec -> Right (specToSource d (withSelect spec))
    _ -> Left "make_chart takes an object."
  where
    callKeys = ["data", "cell_id", "after_cell_id", "select", "select_name"]
    withSelect spec = case (fieldText "select" input, fieldText "select_name" input) of
        ("", _) -> spec
        (kind, name) -> spec{vsSelect = Just (if T.null name then "selection" else name, kind)}

execMakeChart ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execMakeChart app store rn cancelTok input =
    case chartCallSource input of
        Left why ->
            pure
                ( errOutcome
                    ( errorJsonWith
                        "make_chart arguments do not fit the chart language."
                        ["hint" .= why, "grammar" .= grammarHint]
                    )
                )
        Right src -> do
            out <- case fieldInt "cell_id" input of
                Just cid ->
                    execReplaceCellSource
                        app
                        store
                        rn
                        cancelTok
                        (object ["cell_id" .= cid, "new_source" .= src])
                Nothing ->
                    execInsertCell
                        app
                        store
                        rn
                        cancelTok
                        ( object
                            ( ("source" .= src)
                                : ["after_cell_id" .= v | Just v <- [fieldInt "after_cell_id" input]]
                            )
                        )
            pure (withNote landedNote (dataHint out))
  where
    landedNote =
        "make_chart wrote a canonical DataFrame.Display.Web.Chart.Typed cell; tweak it with make_chart cell_id=N, or replace_cell_source to edit by hand."
    dataHint out = case out of
        ToolErr v
            | any
                (`T.isInfixOf` T.pack (show v))
                ["TypedDataFrame", "not found in schema", "No instance for"] ->
                withNote
                    "`data` must be a TypedDataFrame: load a file with $(DT.deriveSchemaFromCsvFile \"Name\" path) then name <- TCSV.readCsv @Name path, or freeze a frame with $(DT.deriveSchema \"Df\" df) then tdf <- freezeOrThrow @Df df (from DataFrame.Typed.Freeze); a column the schema lacks is named in the error."
                    out
        _ -> out

grammarHint :: Text
grammarHint =
    "mark: bar|line|point|area|tick|rect|arc|text|boxplot; channels x,y,color,size,shape,opacity,theta,row,column each {field, aggregate?, bin?, type?, sort?, scale?, title?, stack?}; tooltip is a list of those; layers is a list of {mark, channels}."
