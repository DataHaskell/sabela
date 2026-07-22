{-# LANGUAGE OverloadedStrings #-}

{- | R5-T2: denial-legal closure (search-api.md section 8.2): closure is
scope-keyed like dedup, a repeat replays its OWN evidence, and every close
carries the best held hit from a final ledger-union sweep.
-}
module Test.DiscoverClosureSpec (discoverClosureSpec) where

import Control.Monad (forM, forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Closure (bestHeldFor, heldHitLine)
import Siza.Agent.Discover.Envelope (envelopeCharBudget, envelopeChars)
import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    heldEvidence,
    ledgerClose,
    ledgerRecord,
    ledgerShortcut,
 )
import Siza.Agent.Discover.HistoryGuard (
    closeSearchLedger,
    guardDiscover,
    newSearchLedger,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Request (requestKey)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    MatchKind (..),
    NotebookEnv (..),
    hitJson,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Test.CatalogueSim (SimWorld (..), simWorldCall)
import Test.DiscoverFixtures (
    SynPkg (..),
    hitText,
    stateOf,
    synHoogle,
    textField,
 )

discoverClosureSpec :: Spec
discoverClosureSpec = describe "denial-legal closure (R5-T2)" $ do
    scopeKeyedClosureSpec
    nineReplaySpec
    closeCarriesHeldHitSpec
    postCloseConsistencySpec
    determinismSpec
    closeBudgetSpec

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True []

args :: [(Text, Value)] -> Value
args kvs = object [(K.fromText k, v) | (k, v) <- kvs]

foundWith :: Text -> DHit -> Value
foundWith q h =
    discoverEnvelope envT (interpret envT q) 8 [okAnswer "session" [h]] hkT

missFor :: Text -> Value
missFor q =
    discoverEnvelope envT (interpret envT q) 8 (map (`okAnswer` []) srcs) hkT
  where
    srcs = ["session", "hoogle"]

-- | The foreign-scope hit of the barChart replay defect (a plotpkg synonym).
foreignHit :: DHit
foreignHit =
    (mkHit "Bar" "Plotly.Types" "plotpkg")
        { dhKind = MkSynonym
        , dhType = "TraceType"
        , dhVersion = "1.0"
        }

barsHit :: DHit
barsHit =
    (mkHit "bars" "Cumulus.Plot" "cumulus")
        { dhType = "[(Text, Double)] -> Plot -> Text"
        , dhVersion = "0.3.1"
        , dhInstall = InstHidden
        , dhCabal = Just "-- cabal: build-depends: cumulus"
        }

script :: SearchLedger -> [(Text, Value)] -> (SearchLedger, [Value])
script led0 = foldl step (led0, [])
  where
    step (led, outs) (q, v) = case ledgerShortcut led q of
        Just out -> (led, outs ++ [out])
        Nothing ->
            let (led2, out) = ledgerRecord q v led
             in (led2, outs ++ [out])

scopeGrid :: [Value]
scopeGrid =
    [ args []
    , args [("module", String "Zephyr.Core")]
    , args [("module", String "Stratus.Air")]
    , args [("package", String "zephyr")]
    , args [("package", String "stratus")]
    , args [("mode", String "inventory")]
    , args [("module", String "Cumulus.Plot"), ("package", String "cumulus")]
    ]

scopeKeyedClosureSpec :: Spec
scopeKeyedClosureSpec = describe "closure is scope-keyed (R3.8)" $ do
    it "an unseen scope key after close is NEVER answered duplicate" $
        forM_ (["gust", "bars", "lull"] :: [Text]) $ \q -> do
            let seen = requestKey q (args [])
                (led1, _) = script emptyLedger [(seen, foundWith q foreignHit)]
                led = ledgerClose led1
            forM_ (drop 1 scopeGrid) $ \a ->
                ledgerShortcut led (requestKey q a) `shouldSatisfy` isNothing
    it "a genuine repeat after close replays its OWN evidence, never foreign" $ do
        let keyA = requestKey "bar chart" (args [])
            keyB = requestKey "bars" (args [("module", String "Cumulus.Plot")])
            (led1, _) =
                script
                    emptyLedger
                    [ (keyA, foundWith "bar chart" foreignHit)
                    , (keyB, foundWith "bars" barsHit)
                    ]
            led = ledgerClose led1
        Just refB <- pure (ledgerShortcut led keyB)
        stateOf refB `shouldBe` "duplicate"
        textField "summary" refB `shouldSatisfy` T.isInfixOf "bars"
        textField "summary" refB
            `shouldSatisfy` (not . T.isInfixOf "Bar :: TraceType")

nineReplaySpec :: Spec
nineReplaySpec = describe "the barChart nine-replay fixture" $
    it "nine differently-scoped queries after close: zero foreign replays" $ do
        let (led1, _) =
                script
                    emptyLedger
                    [("bar chart", foundWith "bar chart" foreignHit)]
            led = ledgerClose led1
            replays =
                [ requestKey q a
                | q <- ["bars", "granite bars", "bar"]
                , a <- take 3 (drop 1 scopeGrid)
                ]
            outs = [out | k <- replays, Just out <- [ledgerShortcut led k]]
            foreign' =
                [ o
                | o <- outs
                , "Bar" `T.isInfixOf` textField "summary" o
                    || stateOf o == "duplicate"
                ]
        length replays `shouldBe` 9
        foreign' `shouldBe` []

closeCarriesHeldHitSpec :: Spec
closeCarriesHeldHitSpec = describe "a close never asserts absence over held evidence" $ do
    it "whole catalogue: every closed repeat carries its held hit (ledger sweep)" $ do
        violations <- fmap concat . forM catalogueHits $ \(nm, h) -> do
            let key = requestKey nm (args [])
                (led1, _) = script emptyLedger [(key, foundWith nm h)]
                led = ledgerClose led1
            pure $ case ledgerShortcut led key of
                Nothing -> ["no closed answer for " <> nm]
                Just out
                    | nm `T.isInfixOf` textField "summary" out -> []
                    | otherwise ->
                        ["close for " <> nm <> " dropped the held hit"]
        violations `shouldBe` []
    it "the held hit carries provenance, install state and the cabal line" $ do
        let key = requestKey "bars" (args [])
            (led1, _) = script emptyLedger [(key, foundWith "bars" barsHit)]
            led = ledgerClose led1
        Just out <- pure (ledgerShortcut led key)
        let s = textField "summary" out
        s `shouldSatisfy` T.isInfixOf "bars"
        s `shouldSatisfy` T.isInfixOf "Cumulus.Plot"
        s `shouldSatisfy` T.isInfixOf "cumulus"
        s `shouldSatisfy` T.isInfixOf "hidden"
        s `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
    it "rung-3 give-up advice hands over held evidence for the entity" $ do
        -- The found lives in another cluster; the missed cluster still NAMES
        -- the entity (its package), so the give-up must hand the hit over.
        let (_, outs) =
                script
                    emptyLedger
                    [ (requestKey "bars" (args []), foundWith "bars" barsHit)
                    , ("cumulus bars render", missFor "cumulus bars render")
                    , ("cumulus bars svg", missFor "cumulus bars svg")
                    , ("cumulus bars text", missFor "cumulus bars text")
                    ]
            closeOut = last outs
            advice = textField "next" closeOut <> textField "summary" closeOut
        advice `shouldSatisfy` T.isInfixOf "bars"
        advice `shouldSatisfy` T.isInfixOf "Cumulus.Plot"
    it "a bare cannot-help names what was consulted (empty catalogue only)" $ do
        let (_, outs) =
                script
                    emptyLedger
                    [ ("ghostname alpha", missFor "ghostname alpha")
                    , ("ghostname beta", missFor "ghostname beta")
                    , ("ghostname gamma", missFor "ghostname gamma")
                    ]
            advice = textField "next" (last outs)
        advice `shouldSatisfy` T.isInfixOf "consulted"
    it "bestHeldFor finds the entity across recorded evidence (union sweep)" $ do
        let key = requestKey "bars" (args [])
            (led1, _) = script emptyLedger [(key, foundWith "bars" barsHit)]
        fmap (hitText "name") (bestHeldFor (heldEvidence led1) "bars")
            `shouldBe` Just "bars"
        -- Named by module and by package, not just by symbol name:
        fmap (hitText "name") (bestHeldFor (heldEvidence led1) "cumulus.plot")
            `shouldBe` Just "bars"
        fmap (hitText "name") (bestHeldFor (heldEvidence led1) "cumulus")
            `shouldBe` Just "bars"
        bestHeldFor (heldEvidence led1) "ghostname" `shouldBe` Nothing

catalogueHits :: [(Text, DHit)]
catalogueHits =
    [ (n, (mkHit n m (spName p)){dhType = ty, dhVersion = spVersion p})
    | p <- synHoogle
    , (m, es) <- spModules p
    , (n, ty) <- es
    ]

postCloseConsistencySpec :: Spec
postCloseConsistencySpec = describe "post-close consistency (R5.7)" $
    it "a literal-minded caller through guardDiscover is never told to search" $ do
        ref <- newSearchLedger
        let world = SimWorld synHoogle synHoogle
            inner (ToolCall _ a) =
                runOk (runDiscoverCall True (simWorldCall world) (qOf a) a)
            disp = guardDiscover ref inner
            call q = disp (ToolCall "discover" (object ["query" .= q]))
        _ <- call ("bars" :: Text)
        _ <- closeSearchLedger ref
        outs <- mapM call (["bars", "bars", "gust"] :: [Text])
        forM_ outs $ \out -> do
            Right (ToolOk v) <- pure out
            let advice =
                    T.toLower (textField "next" v <> textField "summary" v)
            forM_ searchAdvisePhrases $ \p ->
                advice `shouldSatisfy` (not . T.isInfixOf p)
  where
    qOf = textField "query"
    runOk act = Right <$> act
    searchAdvisePhrases =
        ["try another query", "rephrase", "retry discover", "search again"]

determinismSpec :: Spec
determinismSpec = describe "the ledger is deterministic (R3.7)" $
    it "two independent evaluations of one query over one state agree" $ do
        -- Two runs assembled from separately constructed inputs (never the
        -- same thunk compared to itself): interleavings, closes, repeats.
        let mkInputs () =
                [ (requestKey q a, foundWith q barsHit)
                | q <- ["bars", "gust"]
                , a <- scopeGrid
                ]
                    ++ [ (requestKey "ghost" a, missFor "ghost")
                       | a <- scopeGrid
                       ]
            runA = script emptyLedger (mkInputs ())
            runB = script emptyLedger (mkInputs ())
        encode (snd runA) `shouldBe` encode (snd runB)
        let closedA = ledgerClose (fst runA)
            closedB = ledgerClose (fst runB)
            probes () = [requestKey q a | q <- ["bars", "ghost"], a <- scopeGrid]
        map (fmap encode . ledgerShortcut closedA) (probes ())
            `shouldBe` map (fmap encode . ledgerShortcut closedB) (probes ())

closeBudgetSpec :: Spec
closeBudgetSpec = describe "close envelope budget (R3.9)" $
    it "a closed answer over maximal held state stays within 2500 chars" $ do
        let longHit =
                barsHit
                    { dhType = T.replicate 40 "[(Text, Double)] -> "
                    , dhName = "bars"
                    }
            keys = [requestKey q (args []) | q <- ["bars", "gust", "lull"]]
            (led1, _) =
                script emptyLedger [(k, foundWith k longHit) | k <- keys]
            led = ledgerClose led1
        forM_ keys $ \k -> do
            Just out <- pure (ledgerShortcut led k)
            envelopeChars out `shouldSatisfy` (<= envelopeCharBudget)
        heldHitLine (hitJson longHit) `shouldSatisfy` ((<= 400) . T.length)
