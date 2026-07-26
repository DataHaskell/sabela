{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Query (
    execCheckType,
    execFindByType,
    execDescribeFunction,
    execExploreResult,
    execPeekData,
    runExplore,
    ExploreOp (..),
    parseExploreOp,
    guidedOutcome,
    typeConstructors,
    recordDecl,
    instanceClasses,
    withBackend,
) where

import Control.Exception (try)
import Control.Exception.Base (IOException)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)
import System.FilePath (normalise, splitDirectories, (</>))

import Sabela.AI.Capabilities.Query.Explore (
    ExploreOp (..),
    execExploreResult,
    parseExploreOp,
    runExplore,
 )
import Sabela.AI.Capabilities.Query.IndexAnswer (
    consultedSources,
    fillSilence,
    viaSessionInfo,
    viaSessionType,
 )
import Sabela.AI.Capabilities.Util (fieldInt, fieldText)
import Sabela.AI.HoleFits (holeFitsJson)
import Sabela.AI.LeakShape (leakyLine)
import Sabela.AI.PeekData (peekData, peekResultJSON)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.AI.VerifierDistill (answerVerdict, distillInfo, distillTypeAnswer)
import Sabela.Api (errorJson)
import Sabela.Diagnose (diagnose, guidancePairs)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (getHaskellSession)

withBackend :: App -> (SessionBackend -> IO ToolOutcome) -> IO ToolOutcome
withBackend app k = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing ->
            pure
                ( errOutcome
                    (errorJson "No live Haskell session — run a cell first to start GHCi.")
                )
        Just backend -> k backend

guidedOutcome :: [Pair] -> Text -> ToolOutcome
guidedOutcome fields result =
    okOutcome $
        object (fields <> ["result" .= result] <> guidancePairs (diagnose result))

execCheckType :: App -> Value -> IO ToolOutcome
execCheckType app input = do
    let expr = T.strip (fieldText "expr" input)
    if T.null expr
        then
            pure
                ( errOutcome
                    (errorJson "expr required (an expression, value, type, or class name)")
                )
        else withBackend app $ \backend -> do
            (via, result) <- dispatchCheckType backend expr
            (via', result') <- fillSilence app expr via result
            pure
                ( guidedOutcome
                    [ "expr" .= expr
                    , "via" .= via'
                    , "consulted" .= consultedSources
                    , "verdict" .= answerVerdict result'
                    ]
                    result'
                )

dispatchCheckType :: SessionBackend -> Text -> IO (Text, Text)
dispatchCheckType backend expr
    | length (T.words expr) > 1 =
        (,) viaSessionType . distillTypeAnswer <$> sbQueryType backend expr
    | otherwise = do
        ty <- distillTypeAnswer <$> sbQueryType backend expr
        if looksResolved ty
            then do
                struct <- typeStructure backend ty
                pure (viaSessionType, if T.null struct then ty else ty <> "\n\n" <> struct)
            else do
                raw <- sbQueryInfo backend expr
                pure (viaSessionInfo, withInstances raw (distillTypeAnswer raw))

withInstances :: Text -> Text -> Text
withInstances raw answer = case instanceClasses raw of
    [] -> answer
    cs -> answer <> "\n\ninstances: " <> T.intercalate ", " cs

looksResolved :: Text -> Bool
looksResolved t =
    let lt = T.toLower t
     in not (T.null (T.strip t))
            && not ("not in scope" `T.isInfixOf` lt)
            && not ("error:" `T.isInfixOf` lt)
            && not ("illegal term-level" `T.isInfixOf` lt)

typeStructure :: SessionBackend -> Text -> IO Text
typeStructure backend = go . take 4 . candidates
  where
    go [] = pure ""
    go (c : cs) = do
        decl <- recordDecl . distillInfo <$> sbQueryInfo backend c
        maybe (go cs) pure decl
    candidates = concatMap variants . typeConstructors
    variants t =
        let bare = lastSeg t
         in if bare == t then [t] else [t, bare]

instanceClasses :: Text -> [Text]
instanceClasses info =
    nubKeep
        [ cls
        | l <- map T.strip (T.lines info)
        , Just rest <- [T.stripPrefix "instance " l]
        , cls : _ <- [T.words (dropContext (fst (T.breakOn "--" rest)))]
        , maybe False (isUpper . fst) (T.uncons cls)
        ]
  where
    dropContext r = case T.breakOn "=>" r of
        (_, m) | not (T.null m) -> T.strip (T.drop 2 m)
        _ -> r
    nubKeep = foldr (\x acc -> x : filter (/= x) acc) []

lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."

typeConstructors :: Text -> [Text]
typeConstructors s = nubKeep (filter isCtorAtom atoms)
  where
    rhs = case T.breakOn "::" s of
        (_, r) | not (T.null r) -> T.drop 2 r
        _ -> s
    atoms =
        filter (not . T.null) $
            T.split (`elem` (" \t\n[]()->,!{}=|" :: String)) rhs
    isCtorAtom t = maybe False (isUpper . fst) (T.uncons (lastSeg t))
    nubKeep = foldr (\x acc -> x : filter (/= x) acc) []

recordDecl :: Text -> Maybe Text
recordDecl info
    | hasAdt && hasField = Just kept
    | otherwise = Nothing
  where
    kept = T.intercalate "\n" (filter keep (T.lines info))
    keep l =
        let t = T.strip l
         in not (T.null t)
                && not (leakyLine t)
                && not ("instance " `T.isPrefixOf` t)
                && not ("-- Defined in" `T.isInfixOf` t)
    hasAdt = "data " `T.isInfixOf` kept || "newtype " `T.isInfixOf` kept
    hasField = "{" `T.isInfixOf` kept

execFindByType :: App -> Value -> IO ToolOutcome
execFindByType app input = do
    let goal = T.strip (fieldText "goal" input)
    if T.null goal
        then
            pure
                (errOutcome (errorJson "goal required (a type like \"[Int] -> Int\")"))
        else withBackend app $ \backend -> do
            let hole = if "_" `T.isPrefixOf` goal then goal else "_ :: " <> goal
            result <- sbQueryHoleFits backend hole
            let fits = holeFitsJson findByTypeCap result
            pure
                ( okOutcome
                    ( object
                        [ "goal" .= goal
                        , "fits" .= fits
                        , "shown" .= length fits
                        , "probe" .= ("typecheck-only; nothing was committed" :: Text)
                        ]
                    )
                )

findByTypeCap :: Int
findByTypeCap = 8

execDescribeFunction :: App -> Value -> IO ToolOutcome
execDescribeFunction app input = do
    let name = T.strip (fieldText "name" input)
    if T.null name
        then pure (errOutcome (errorJson "name required"))
        else withBackend app $ \backend -> do
            result <- sbQueryDoc backend name
            pure (guidedOutcome ["name" .= name] result)

defaultPeekRows :: Int
defaultPeekRows = 10

execPeekData :: App -> Value -> IO ToolOutcome
execPeekData app input = do
    let relPath = T.unpack (fieldText "path" input)
        n = fromMaybe defaultPeekRows (fieldInt "n" input)
        workDir = envWorkDir (appEnv app)
    if null relPath
        then pure (errOutcome (errorJson "path required"))
        else do
            canon <- canonicalizePath (workDir </> relPath)
            if not (isWithinPath workDir canon)
                then pure (errOutcome (errorJson "Path escapes the work directory."))
                else do
                    eText <- try (TIO.readFile canon) :: IO (Either IOException Text)
                    case eText of
                        Left e ->
                            pure
                                ( errOutcome
                                    (errorJson ("Could not read file: " <> T.pack (show e)))
                                )
                        Right raw ->
                            pure $
                                okOutcome $
                                    object ["path" .= relPath, "peek" .= peekResultJSON (peekData n raw)]

isWithinPath :: FilePath -> FilePath -> Bool
isWithinPath parent child =
    let p = splitDirectories (normalise parent)
        c = splitDirectories (normalise child)
     in p == take (length p) c
