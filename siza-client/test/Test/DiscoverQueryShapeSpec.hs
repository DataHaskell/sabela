{-# LANGUAGE OverloadedStrings #-}

{- | C1-9a: a query ending in @::@ with nothing to its right is a NAME lookup.
C2-smaller-cardquery: a module probe carries the caller's terms.
-}
module Test.DiscoverQueryShapeSpec (discoverQueryShapeSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Fetch (probeHidden, queryVariants)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Types (
    Interpreted (..),
    NotebookEnv (..),
    SourceAnswer (..),
    okAnswer,
    seededBuiltins,
 )

env0 :: NotebookEnv
env0 =
    seededBuiltins
        (NotebookEnv [("D", "Data.Map")] ["Data.Map", "Data.Aeson"] [] [] [] [])

genIdent :: Gen Text
genIdent =
    oneof
        [ elements
            [ "insertWith"
            , "foldl'"
            , "splitOn"
            , "decodeStrict'"
            , "mapAccumL"
            , "alterF"
            , "unionWith"
            ]
        , elements ["Data.Map.alter", "D.lookup", "Data.List.foldl'"]
        , elements ["(.:?)", "(<|>)"]
        , arbitraryLower
        ]
  where
    arbitraryLower = do
        c <- elements "abcdefghijklmnopqrstuvwxyz"
        rest <- listOf (elements "abcdefghijklmnopqrstuvwxyz0123456789'_")
        pure (T.pack (c : take 8 rest))

genSigSuffix :: Gen Text
genSigSuffix = elements [" ::", "::", " :: ", "  ::  ", " ::   "]

genTypeText :: Gen Text
genTypeText =
    elements
        [ "Int -> Int"
        , "Map k v -> Map k v"
        , "Value"
        , "Ord a => [a] -> [a]"
        ]

discoverQueryShapeSpec :: Spec
discoverQueryShapeSpec = do
    describe "a trailing :: asks for a name's signature (C1-9a)" $ do
        prop "the interpreted name is the name, not the decorated query" $
            forAll genIdent $ \n ->
                forAll genSigSuffix $ \s ->
                    iName (interpret env0 (n <> s)) === iName (interpret env0 n)
        prop "the interpreted shape is the bare name's shape" $
            forAll genIdent $ \n ->
                forAll genSigSuffix $ \s ->
                    iShape (interpret env0 (n <> s)) === iShape (interpret env0 n)
        prop "the session variants include the bare name" $
            forAll genIdent $ \n ->
                forAll genSigSuffix $ \s ->
                    queryVariants (n <> s) `shouldContain'` iName (interpret env0 n)
        prop "a :: with a type to its right is still a type query" $
            forAll genIdent $ \n ->
                forAll genTypeText $ \t ->
                    iShape (interpret env0 (n <> " :: " <> t)) === "type"
        it "the reinterpretation is disclosed, never silent" $
            iNote (interpret env0 "col ::")
                `shouldSatisfy` (not . T.null)

    describe "a module probe carries the caller's terms (C2-smaller-cardquery)" $ do
        it "probeHidden asks the module for the interpreted query" $ do
            args <- probeArgs "parquetish" "Syn.Frame"
            map (argText "module") args `shouldBe` ["Syn.Frame"]
            map (argText "query") args `shouldBe` ["parquetish"]
        it "a module-shaped query probes with the module name itself" $ do
            args <- probeArgs "Syn.Frame" "Syn.Frame"
            map (argText "query") args `shouldBe` ["Syn.Frame"]

shouldContain' :: [Text] -> Text -> Property
shouldContain' vs n =
    counterexample (show vs <> " lacks " <> show n) (property (n `elem` vs))

probeArgs :: Text -> Text -> IO [Value]
probeArgs q modName = do
    ref <- newIORef []
    let call tn args = do
            modifyIORef' ref (++ [(tn, args)])
            pure (Right (ToolOk (object ["matches" .= ([] :: [Value])])))
        answers =
            [(okAnswer "hoogle" []){saPkgModules = [("synpkg", [modName])]}]
    _ <- probeHidden call (interpret env0 q) answers
    calls <- readIORef ref
    pure [a | (tn, a) <- calls, tn == FindFunction]

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""
