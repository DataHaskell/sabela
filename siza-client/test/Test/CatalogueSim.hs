{-# LANGUAGE OverloadedStrings #-}

{- | A simulated WORLD over the synthetic catalogue: session visibility is
controlled separately from the hoogle universe (so an in-session install is
representable), @find_by_type@ answers hole-fit blobs, and the capability
channel answers hoogle-style result-type queries (@:: T@). Shared by the
world-change ledger tests and the constructibility-facet tests.
-}
module Test.CatalogueSim (
    SimWorld (..),
    simWorldCall,
    runWorld,
    runWorldArgs,
    worldBlob,
    producerPkgs,
    resultLike,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Test.DiscoverFixtures (
    SynPkg (..),
    argText,
    simCapability,
    simSession,
 )

{- | One world: what the live session can see vs what the hoogle universe
knows. An install event moves a package from universe-only into the session.
-}
data SimWorld = SimWorld
    { swSession :: [SynPkg]
    , swUniverse :: [SynPkg]
    }

-- | The simulated dispatch of a world; unknown tools answer unsupported.
simWorldCall :: SimWorld -> ToolName -> Value -> IO (Either Text ToolOutcome)
simWorldCall w tn args = pure $ case tn of
    ListCells -> ok cellsJson
    FindFunction ->
        ok (simSession (swSession w) (argText "query" args) (argText "module" args))
    SearchCapability ->
        let q = argText "query" args
         in case T.stripPrefix ":: " (T.strip q) of
                Just goal -> ok (typeBuckets (swUniverse w) goal)
                Nothing -> ok (simCapability (swUniverse w) q)
    FindByType -> ok (worldBlob (inScopePkgs w) (argText "goal" args))
    _ -> Left "unsupported tool in world sim"
  where
    ok = Right . ToolOk
    -- Hole fits are in-scope evidence: hidden packages cannot contribute.
    inScopePkgs = filter (not . spHidden) . swSession

cellsJson :: Value
cellsJson =
    object
        [ "cells"
            .= [ object
                    [ "source" .= ("import qualified Zephyr.Core as Z" :: Text)
                    , "defines" .= ([] :: [Text])
                    ]
               ]
        ]

-- | A GHC-shaped hole-fit blob for @_ :: T@ over the session-visible exports.
worldBlob :: [SynPkg] -> Text -> Value
worldBlob pkgs goalArg =
    object ["goal" .= goal, "result" .= blob]
  where
    goal = T.strip (T.replace "_ ::" "" goalArg)
    fits =
        [ "  " <> n <> " :: " <> ty
        | p <- pkgs
        , (_, es) <- spModules p
        , (n, ty) <- es
        , resultLike goal ty
        ]
    blob
        | null fits = "" :: Text
        | otherwise = T.unlines ("Valid hole fits include" : fits)

-- | Does @ty@ produce the goal: equal to it, or returning it as final result?
resultLike :: Text -> Text -> Bool
resultLike goal ty =
    norm ty == g || ("-> " <> g) `T.isSuffixOf` norm ty
  where
    g = norm goal
    norm = T.unwords . T.words

-- | Hoogle-style result-type search: capability buckets of producers of @T@.
typeBuckets :: [SynPkg] -> Text -> Value
typeBuckets universe goal =
    object ["query" .= (":: " <> goal), "hits" .= map bucket matching]
  where
    matching = [p | p <- universe, not (null (producers p))]
    producers p =
        [ (m, n, ty)
        | (m, es) <- spModules p
        , (n, ty) <- es
        , resultLike goal ty
        ]
    bucket p =
        object
            [ "package" .= spName p
            , "version" .= spVersion p
            , "synopsis" .= ("synthetic package" :: Text)
            , "cabal" .= ("-- cabal: build-depends: " <> spName p)
            , "modules" .= map fst (spModules p)
            , "api"
                .= [ object ["name" .= n, "module" .= m, "type" .= ty]
                   | (m, n, ty) <- producers p
                   ]
            ]

-- | Run a discover call (query only) against a world.
runWorld :: SimWorld -> Text -> IO Value
runWorld w q = runWorldArgs w q (object [])

-- | Run a discover call with raw arguments against a world.
runWorldArgs :: SimWorld -> Text -> Value -> IO Value
runWorldArgs w q args = do
    out <- runDiscoverCall True (simWorldCall w) q args
    pure $ case out of
        ToolOk v -> v
        ToolErr v -> v

{- | A producer catalogue with structure held constant and package names
substitutable (the library-name-invariance grid): a plotting package with a
nullary default, a frame package with constructors-and-producers, and a
hidden styling package whose only producer needs an argument.
-}
producerPkgs :: (Text, Text, Text) -> [SynPkg]
producerPkgs (pA, pB, pC) =
    [ SynPkg
        pA
        "1.0.0"
        False
        [
            ( "Aero.Plot"
            ,
                [ ("bars", "[(Text, Double)] -> Plot -> Text")
                , ("defaultPlot", "Plot")
                , ("mkPlot", "Text -> Plot")
                , ("plotLike", "Int -> Plot")
                ]
            )
        ]
    , SynPkg
        pB
        "2.0.0"
        False
        [
            ( "Aero.Frame"
            ,
                [ ("emptyFrame", "Frame")
                , ("MkFrame", "[Row] -> Frame")
                , ("addRow", "Row -> Frame -> Frame")
                ]
            )
        ]
    , SynPkg
        pC
        "0.5.0"
        True
        [("Aero.Style", [("mkStyle", "Int -> Style")])]
    ]
