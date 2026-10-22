{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | @POST /api/viz/source@: the canonical Haskell for a chart spec, and
whether the cell it is meant for is the builder's to rewrite. One
implementation serves the Design panel and the model's chart tool.
-}
module Sabela.Server.Viz (
    vizSourceH,
    vizSourceFor,
    vizColumnsH,
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Servant (Handler)

import Data.Aeson (decodeStrict')
import Data.Char (isAlphaNum, isLower)
import qualified Data.Text.Encoding as TE
import Sabela.AI.Doc (cellHash)
import Sabela.Api.Viz
import Sabela.Api.Viz.Columns (VizColumns (..))
import Sabela.Api.Viz.Source
import Sabela.Model (Cell (..), Notebook, OutputItem (..), lookupCell)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

vizSourceH :: App -> VizSourceRequest -> Handler VizSourceResult
vizSourceH app req = liftIO $ do
    nb <- readNotebook (appNotebook app)
    pure (vizSourceFor nb req)

-- | The pure decision, given the notebook as it stands.
vizSourceFor :: Notebook -> VizSourceRequest -> VizSourceResult
vizSourceFor nb req = case vsrCellId req >>= (`lookupCell` nb) of
    Nothing ->
        let d = maybe "_data" T.strip (vsrData req)
         in VizSourceResult
                { vsSource = specToSource d (vsrSpec req)
                , vsOwnership = OwnNew
                , vsMessage = "A new chart cell."
                , vsDataExpr = d
                , vsCellHash = Nothing
                }
    Just cell ->
        let d = maybe "_data" T.strip (vsrData req <|> dataExprOf (cellSource cell))
            carried = firstJust (map (specFromOutput . oiOutput) (cellOutputs cell))
            own = case carried of
                Just spec -> ownership spec (cellSource cell)
                Nothing
                    | T.null (T.strip (cellSource cell)) -> OwnCanonical
                    | otherwise -> OwnEdited
         in VizSourceResult
                { vsSource = specToSource d (vsrSpec req)
                , vsOwnership = own
                , vsMessage = message own
                , vsDataExpr = d
                , vsCellHash = Just (cellHash cell)
                }
  where
    firstJust xs = case catMaybes xs of
        (x : _) -> Just x
        [] -> Nothing

message :: VizOwnership -> Text
message o = case o of
    OwnCanonical -> "This chart cell is managed by the builder; it is rewritten in place."
    OwnEdited ->
        "This cell was edited by hand beyond the chart language, so the builder "
            <> "inserts a fresh chart cell after it and leaves this one untouched."
    OwnNew -> "A new chart cell."

{- | @GET /api/viz/columns?binding=df@: the columns and types of a frame the
kernel holds, typed or not, so a builder can offer them before any chart or
table has shown them.
-}
vizColumnsH :: App -> Maybe Text -> Handler VizColumns
vizColumnsH app mBinding = liftIO $ case mBinding of
    Just b | validBinding b -> do
        mSess <- getHaskellSession (appSessions app)
        case mSess of
            Nothing -> pure (failed b "no kernel is running")
            Just backend -> do
                (out, err) <-
                    ST.sbRunBlock
                        backend
                        ("SabelaBase.putStrLn (_sabelaColumnsJson " <> b <> ")")
                case decodeStrict' (TE.encodeUtf8 (T.strip out)) of
                    Just (pairs :: [[Text]]) ->
                        pure
                            VizColumns
                                { vcBinding = b
                                , vcCols = [n | (n : _) <- pairs]
                                , vcTypes = [t | (_ : t : _) <- pairs]
                                , vcError = Nothing
                                }
                    Nothing -> pure (failed b (firstLine (if T.null err then out else err)))
    Just b -> pure (failed b "not a binding name")
    Nothing -> pure (failed "" "binding is required")
  where
    failed b why = VizColumns b [] [] (Just why)
    firstLine t = case T.lines (T.strip t) of
        (l : _) -> l
        [] -> "the kernel said nothing"
    validBinding b = case T.uncons b of
        Just (c, rest) ->
            (isLower c || c == '_')
                && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') rest
        Nothing -> False
