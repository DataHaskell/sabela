{-# LANGUAGE OverloadedStrings #-}

{- | Truthfulness reaches every arm (search-api.md sections 0/2/7, R2-T3):
arm-independence over the WHOLE synthetic catalogue (the lever changes
enrichment only, never answerability) and stage-0 exact-name findability with
every fuzzy channel disabled. Ledger properties live in
Test.DiscoverLedgerSpec; turn-0 seeding in Test.DiscoverSeedSpec.
-}
module Test.DiscoverTruthSpec (discoverTruthSpec) where

import Control.Monad (forM)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.DiscoverTool (runDiscoverTool)
import Test.DiscoverFixtures

discoverTruthSpec :: Spec
discoverTruthSpec =
    beforeAll_ installNamesFile $
        describe "discover truthfulness on every arm (R2-T3)" $ do
            armIndependenceSpec
            stageZeroSpec

-- Arm independence (section 2) ---------------------------------------------

armIndependenceSpec :: Spec
armIndependenceSpec = describe "arm independence: the lever never changes answerability" $ do
    it "every catalogue verdict is byte-identical with the lever on and off" $ do
        diffs <- concat <$> mapM armDiff discoverables
        diffs `shouldBe` []
    it "the off arm still consults the lexical hoogle channel" $ do
        ToolOk v <- runDiscoverTool False simCall "gust"
        consultedStatus "hoogle" v `shouldBe` "ok"
    it "both arms call the capability channel; only the semantic flag differs" $ do
        onArgs <- recordSemantic True
        offArgs <- recordSemantic False
        onArgs `shouldBe` [Just True]
        offArgs `shouldBe` [Just False]

armDiff :: Text -> IO [Text]
armDiff q = do
    ToolOk on <- runDiscoverTool True simCall q
    ToolOk off <- runDiscoverTool False simCall q
    pure [q | on /= off]

consultedStatus :: Text -> Value -> Text
consultedStatus src v =
    T.concat . take 1 $
        [ textField "status" c
        | Just (Array cs) <- [field "consulted" v]
        , c <- toList cs
        , textField "source" c == src
        ]

{- | The @semantic@ flag of each non-stage-0 capability call made for a
prose query. Stage-0's exact-tier lookups (@exact: true@) run semantic-off
on EVERY arm by design (section 2) and are excluded from the lever check.
-}
recordSemantic :: Bool -> IO [Maybe Bool]
recordSemantic lever = do
    ref <- newIORef []
    let call SearchCapability args = do
            modifyIORef' ref (++ [(exactArg args, semanticArg args)])
            simCall SearchCapability args
        call tn args = simCall tn args
    _ <- runDiscoverTool lever call "parse digits from a string"
    calls <- readIORef ref
    [Just False] `shouldBe` nub [s | (e, s) <- calls, e == Just True]
    pure [s | (e, s) <- calls, e /= Just True]
  where
    semanticArg (Object o) = case KM.lookup "semantic" o of
        Just (Bool b) -> Just b
        _ -> Nothing
    semanticArg _ = Nothing
    exactArg (Object o) = case KM.lookup "exact" o of
        Just (Bool b) -> Just b
        _ -> Nothing
    exactArg _ = Nothing

-- Stage-0 exact-name channel (R3.1, section 7) ------------------------------

{- | Fuzzy channels OFF: name scans and non-exact capability calls answer
empty; module browses and the exact-name capability lookup still work.
-}
lookupOnly :: ToolName -> Value -> IO (Either Text ToolOutcome)
lookupOnly FindFunction args
    | moduleShapedQ (argText "query" args) = simCall FindFunction args
    | otherwise = pure (Right (ToolOk (emptyArr "matches")))
lookupOnly SearchCapability args
    | argBoolT "exact" args = simCall SearchCapability args
    | otherwise = pure (Right (ToolOk (emptyArr "hits")))
lookupOnly tn args = simCall tn args

-- | The hoogle channel fully down as well: only the session lookup answers.
sessionOnly :: ToolName -> Value -> IO (Either Text ToolOutcome)
sessionOnly SearchCapability _ = pure (Right (ToolOk (emptyArr "hits")))
sessionOnly tn args = lookupOnly tn args

moduleShapedQ :: Text -> Bool
moduleShapedQ t =
    not (T.any (== ' ') t)
        && maybe False ((`elem` ['A' .. 'Z']) . fst) (T.uncons t)

argBoolT :: Text -> Value -> Bool
argBoolT k v = case field k v of
    Just (Bool b) -> b
    _ -> False

emptyArr :: Text -> Value
emptyArr k = object [K.fromText k .= ([] :: [Value])]

stageZeroSpec :: Spec
stageZeroSpec = describe "stage-0 exact-name channel carries findability alone (R3.1)" $ do
    it "every catalogue export is top-3 with every fuzzy channel disabled" $ do
        bad <- fmap concat . forM catalogueExports $ \n -> do
            ToolOk v <- runDiscoverTool True lookupOnly n
            let top3 = map (hitText "name") (take 3 (hitsOf v))
            pure [n | n `notElem` top3]
        bad `shouldBe` []
    it "every catalogue module still answers" $ do
        bad <- fmap concat . forM catalogueModules $ \m -> do
            ToolOk v <- runDiscoverTool True lookupOnly m
            pure [m | stateOf v /= "found"]
        bad `shouldBe` []
    it
        "a name exported by a notebook-imported module is found by the session lookup alone"
        $ do
            ToolOk v <- runDiscoverTool True sessionOnly "gust"
            stateOf v `shouldBe` "found"
            map (hitText "name") (take 1 (hitsOf v)) `shouldBe` ["gust"]
