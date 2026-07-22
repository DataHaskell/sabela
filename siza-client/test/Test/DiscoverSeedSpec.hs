{-# LANGUAGE OverloadedStrings #-}

{- | Turn-0 environment seeding (search-api.md sections 2/11, R1.5, R1.6) over
GENERATED notebooks: any module imported by a cell and any
'Sabela.AI.PromptCore.builtinNames' entry is never deniable, and module-shaped
queries answer from the notebook env ("imported by cell 0 as D") — structural
impossibility, proven with every backend answering empty.
-}
module Test.DiscoverSeedSpec (discoverSeedSpec) where

import Control.Monad (forM, replicateM)
import Data.Aeson (Value, object, (.=))
import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.PromptCore (builtinNames)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.DiscoverTool (runDiscoverTool)
import Test.DiscoverFixtures (hitText, hitsOf, installNamesFile, stateOf)

discoverSeedSpec :: Spec
discoverSeedSpec =
    beforeAll_ installNamesFile $
        describe "turn-0 seeding (R1.5/R1.6): the environment is undeniable" $ do
            it "every imported module answers from the env on every generated notebook" $ do
                let notebooks = concat [replicateM n cellPool | n <- [1 .. 3]]
                bad <- concat <$> mapM checkNotebook notebooks
                bad `shouldBe` []
            it "no builtinNames entry is deniable even with every backend empty" $ do
                bad <- fmap concat . forM builtinNames $ \n -> do
                    ToolOk v <- runDiscoverTool False (nbCall []) n
                    pure
                        [ n
                        | stateOf v /= "found"
                            || notElem
                                "builtin"
                                (map (hitText "install") (hitsOf v))
                        ]
                bad `shouldBe` []
            it "a module-shaped query answers 'imported by cell 0 as D'" $ do
                let cells = [AliasImport "DataFrame" "D"]
                ToolOk v <- runDiscoverTool False (nbCall cells) "DataFrame"
                stateOf v `shouldBe` "found"
                let use = T.concat (map (hitText "use") (hitsOf v))
                use `shouldSatisfy` T.isInfixOf "imported by cell 0"
                use `shouldSatisfy` T.isInfixOf "as D"

data CellT = AliasImport Text Text | PlainImport Text | Binding | Prose

cellPool :: [CellT]
cellPool =
    [ AliasImport "Alpha.Beta" "A"
    , PlainImport "Gamma"
    , PlainImport "Delta.Epsilon"
    , Binding
    , Prose
    ]

cellSrcT :: CellT -> Text
cellSrcT (AliasImport m a) = "import qualified " <> m <> " as " <> a
cellSrcT (PlainImport m) = "import " <> m
cellSrcT Binding = "total = 1"
cellSrcT Prose = "-- notes"

-- | Each imported module with its first importing cell and any alias.
firstImports :: [CellT] -> [(Text, Int, Maybe Text)]
firstImports cells =
    nubBy (\(m, _, _) (m', _, _) -> m == m') $
        [ (m, i, aliasFor m)
        | (i, c) <- zip [0 ..] cells
        , Just m <- [moduleOfT c]
        ]
  where
    moduleOfT (AliasImport m _) = Just m
    moduleOfT (PlainImport m) = Just m
    moduleOfT _ = Nothing
    aliasFor m =
        case [a | AliasImport m' a <- cells, m' == m] of
            (a : _) -> Just a
            [] -> Nothing

-- | Every backend empty: only the environment layer can answer.
nbCall :: [CellT] -> ToolName -> Value -> IO (Either Text ToolOutcome)
nbCall cells ListCells _ =
    pure . Right . ToolOk $ object ["cells" .= map cellJson cells]
  where
    cellJson c =
        object
            [ "source" .= cellSrcT c
            , "defines" .= (["total" | Binding <- [c]] :: [Text])
            ]
nbCall _ FindFunction _ =
    pure (Right (ToolOk (object ["matches" .= ([] :: [Value])])))
nbCall _ SearchCapability _ =
    pure (Right (ToolOk (object ["hits" .= ([] :: [Value])])))
nbCall _ _ _ = pure (Left "unsupported")

checkNotebook :: [CellT] -> IO [Text]
checkNotebook cells =
    fmap concat . forM (firstImports cells) $ \(m, i, mAlias) -> do
        ToolOk v <- runDiscoverTool False (nbCall cells) m
        let envHits =
                [ h
                | h <- hitsOf v
                , hitText "name" h == m
                , hitText "install" h == "notebook"
                ]
            use = T.concat (map (hitText "use") envHits)
            ok =
                stateOf v == "found"
                    && not (null envHits)
                    && ("imported by cell " <> T.pack (show i)) `T.isInfixOf` use
                    && maybe True (\a -> ("as " <> a) `T.isInfixOf` use) mAlias
        pure [m <> " in " <> T.pack (show (map cellSrcT cells)) | not ok]
