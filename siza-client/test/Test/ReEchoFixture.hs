{-# LANGUAGE OverloadedStrings #-}

{- | The run-20260720-085948 symbolicRegression-off re-echo fixture: one ~1.6k
cell source transmitted verbatim ten times across model echoes plus the
self_heal/read_cell/list_cells injected surfaces, as a 26-message sequence.
-}
module Test.ReEchoFixture (
    Role (..),
    fixtureMsgs,
    srcPostHeal,
    srcPreHeal,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.SelfHeal (attachSelfHeal, selfHealNote)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Tools (renderOutcome)

data Role = System | User | Assistant | Tool
    deriving (Eq, Show)

-- | The episode's evolving cell source, final pre-heal version (msg 17).
srcPreHeal :: Text
srcPreHeal = srcWithSig "(resultExpr, resultError) :: (String, Double)"

-- | The post-heal source echoed by self_heal, read_cell, and list_cells.
srcPostHeal :: Text
srcPostHeal = srcWithSig "(resultText, resultError) :: (String, Double)"

srcWithSig :: Text -> Text
srcWithSig sig =
    T.intercalate
        "\n"
        [ "-- cabal: build-depends: base"
        , "import Data.List (minimumBy, nub)"
        , "import Data.Ord (comparing)"
        , ""
        , "-- Simple expression data type"
        , "data Expr = Var | Const Double | Add Expr Expr | Mul Expr Expr deriving (Eq, Show)"
        , ""
        , "-- Evaluate an expression for a given x"
        , "eval :: Expr -> Double -> Double"
        , "eval Var x = x"
        , "eval (Const c) _ = c"
        , "eval (Add e1 e2) x = eval e1 x + eval e2 x"
        , "eval (Mul e1 e2) x = eval e1 x * eval e2 x"
        , ""
        , "-- Pretty-print an expression"
        , "showExpr :: Expr -> String"
        , "showExpr Var = \"x\""
        , "showExpr (Const c) = if c == fromInteger (round c)"
        , "                     then show (round c)"
        , "                     else show c"
        , "showExpr (Add e1 e2) = \"(\" ++ showExpr e1 ++ \"+\" ++ showExpr e2 ++ \")\""
        , "showExpr (Mul e1 e2) = \"(\" ++ showExpr e1 ++ \"*\" ++ showExpr e2 ++ \")\""
        , ""
        , "-- Generate all expressions up to a given depth"
        , "constants :: [Double]"
        , "constants = [0,1,2,3]"
        , ""
        , "generate :: Int -> [Expr]"
        , "generate 0 = Var : map Const constants"
        , "generate n = let smaller = generate (n-1)"
        , "             in smaller ++ [Add e1 e2 | e1 <- smaller, e2 <- smaller]"
        , "                ++ [Mul e1 e2 | e1 <- smaller, e2 <- smaller]"
        , ""
        , "points :: [(Double, Double)]"
        , "points = [(1.0,1.0),(2.0,4.0),(3.0,9.0),(4.0,16.0)]"
        , ""
        , "sse :: Expr -> Double"
        , "sse e = sum [ (eval e x - y) ^ 2 | (x,y) <- points ]"
        , ""
        , "allExprs :: [Expr]"
        , "allExprs = nub $ concatMap generate [1..2] -- depth up to 2"
        , ""
        , "best :: Expr"
        , "best = minimumBy (comparing sse) allExprs"
        , ""
        , "-- Output as a pair: expression string and error"
        , sig
        , "resultExpr = showExpr best"
        , "resultError = sse best"
        , ""
        , "(resultExpr, resultError)"
        ]

toolOk :: Value -> Text
toolOk = renderOutcome . Right . ToolOk

-- | An assistant turn carrying a replace_cell_source call, as raw JSON.
assistantWrite :: Text -> Text
assistantWrite src =
    TE.decodeUtf8 . LBS.toStrict . encode $
        object
            [ "role" .= ("assistant" :: Text)
            , "content" .= ("" :: Text)
            , "tool_calls"
                .= [ object
                        [ "function"
                            .= object
                                [ "name" .= ("replace_cell_source" :: Text)
                                , "arguments"
                                    .= object
                                        [ "cell_id" .= (0 :: Int)
                                        , "new_source" .= src
                                        ]
                                ]
                        ]
                   ]
            ]

outputsPair :: Value
outputsPair =
    object
        [ "oiMime" .= ("text/plain" :: Text)
        , "oiOutput"
            .= ( "(\"Best expression: (x*x)\\nTotal squared error: 0.0\",0.0)\n(\"(x*x)\",0.0)\n" ::
                    Text
               )
        ]

healMsg :: Text
healMsg =
    toolOk
        ( attachSelfHeal
            (selfHealNote srcPreHeal srcPostHeal)
            ( object
                [ "cellId" .= (0 :: Int)
                , "execution"
                    .= object
                        [ "ok" .= True
                        , "outcome" .= object ["tag" .= ("Succeeded" :: Text)]
                        , "outputs" .= [outputsPair]
                        , "warnings" .= ([] :: [Text])
                        ]
                , "staleBindings" .= (["resultText"] :: [Text])
                ]
            )
        )

readCellMsg :: Text
readCellMsg =
    toolOk
        ( object
            [ "error" .= Null
            , "id" .= (0 :: Int)
            , "lang" .= ("Haskell" :: Text)
            , "outputs" .= [outputsPair]
            , "source" .= srcPostHeal
            , "type" .= ("CodeCell" :: Text)
            ]
        )

listCellsMsg :: Text
listCellsMsg =
    toolOk
        ( object
            [ "cells"
                .= [ object
                        [ "defines"
                            .= ( [ "Add"
                                 , "Expr"
                                 , "allExprs"
                                 , "best"
                                 , "eval"
                                 , "resultError"
                                 , "resultExpr"
                                 ] ::
                                    [Text]
                               )
                        , "hasError" .= False
                        , "id" .= (0 :: Int)
                        , "position" .= (1 :: Int)
                        , "source" .= srcPostHeal
                        , "type" .= ("CodeCell" :: Text)
                        ]
                   ]
            , "title" .= ("Untitled.md" :: Text)
            ]
        )

smallOutcome :: Text -> Text
smallOutcome diag =
    toolOk
        ( object
            [ "cellId" .= (0 :: Int)
            , "execution"
                .= object ["ok" .= False, "outcome" .= object ["message" .= diag]]
            ]
        )

{- | The 26-message sequence with the run's roles: assistants at the odd
positions 3..25 (12 chat calls), the injected tool surfaces between them.
-}
fixtureMsgs :: [(Role, Text)]
fixtureMsgs =
    [ (System, T.replicate 50 "system prompt line; ")
    , (User, "Fit a symbolic expression to the points and report it.")
    , (Assistant, assistantWrite (srcWithSig "resultText :: String"))
    , (Tool, smallOutcome "parse error on input `++'")
    , (Assistant, assistantWrite (srcWithSig "resultMarkdown :: String"))
    , (Tool, smallOutcome "Variable not in scope: resultMarkdown")
    , (Assistant, assistantWrite (srcWithSig "res1 :: String"))
    , (Tool, smallOutcome "parse error on input `++'")
    , (Assistant, assistantWrite (srcWithSig "res2 :: String"))
    , (Tool, smallOutcome "Variable not in scope: resultText")
    , (Assistant, assistantWrite (srcWithSig "res3 :: String"))
    , (Tool, smallOutcome "parse error (possibly incorrect indentation)")
    , (Assistant, "I will fix the cell and re-run it.")
    , (Tool, "The task is not done: the deliverable's check still fails.")
    , (Assistant, assistantWrite (srcWithSig "res4 :: String"))
    , (Tool, smallOutcome "Variable not in scope: res4")
    , (Assistant, assistantWrite srcPreHeal)
    , (Tool, healMsg)
    , (Assistant, "The heal changed my signature; let me read the cell.")
    , (Tool, readCellMsg)
    , (Assistant, "Only one cell; let me list all cells to be sure.")
    , (Tool, listCellsMsg)
    , (Assistant, "Re-running the cell to confirm the output.")
    ,
        ( Tool
        , toolOk
            (object ["cellId" .= (0 :: Int), "ok" .= True, "outputs" .= [outputsPair]])
        )
    , (Assistant, "Checking the live bindings for stale values.")
    , (Tool, toolOk (object ["result" .= T.replicate 20 "binding :: Type = value\n"]))
    ]
