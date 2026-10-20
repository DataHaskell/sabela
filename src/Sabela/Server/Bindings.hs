{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The info panel's live binding listing and heap figures.
module Sabela.Server.Bindings (
    bindingsH,
    notebookDefines,
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Servant (Handler)

import Sabela.Api.Bindings (BindingsResult (..))
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Parse (cellNames)
import Sabela.Session.Query.BindingsView (
    parseBindings,
    parseRtsStats,
    rtsStatsStatement,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

{- | Skips the kernel while a cell runs: both queries would queue behind the
run lock, and the frontend refreshes again once execution finishes.
-}
bindingsH :: App -> Handler BindingsResult
bindingsH app = liftIO $ do
    mSess <- getHaskellSession (appSessions app)
    nb <- readNotebook (appNotebook app)
    case mSess of
        Nothing -> pure (BindingsResult [] Nothing "no-session")
        Just backend -> do
            busy <- ST.sbBusy backend
            if busy
                then pure (BindingsResult [] Nothing "busy")
                else collect backend (notebookDefines nb)

collect :: ST.SessionBackend -> [Text] -> IO BindingsResult
collect backend defined = do
    r <- try $ do
        raw <- ST.sbQueryBindings backend
        (out, _) <- ST.sbRunBlock backend rtsStatsStatement
        pure (raw, out)
    pure $ case r of
        Left (_ :: SomeException) -> BindingsResult [] Nothing "no-session"
        Right (raw, out)
            | "*** query timed out ***" `T.isInfixOf` raw ->
                BindingsResult [] Nothing "timeout"
            | otherwise ->
                BindingsResult (parseBindings defined raw) (parseRtsStats out) "ok"

-- | Every name a current Haskell code cell defines.
notebookDefines :: Notebook -> [Text]
notebookDefines nb =
    [ n
    | c <- nbCells nb
    , cellType c == CodeCell
    , cellLang c == ST.Haskell
    , n <- Set.toList (fst (cellNames (cellSource c)))
    ]
