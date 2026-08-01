{-# LANGUAGE OverloadedStrings #-}

{- | C1/C3: a hit whose type names a type the caller cannot make carries the
sibling that makes it, and a standing goal is enough on its own to go looking
for that sibling. Stated over generated names, types and packages.
-}
module Test.DiscoverProducerHintSpec (discoverProducerHintSpec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Data.Aeson (Value (..), object, (.=))

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Construct (attachProducers)
import Siza.Agent.Discover.Envelope (envelopeViolations)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.ProducerCard (producerHintMark, withProducerHint)
import Siza.Agent.Discover.Types (
    DHit (..),
    NotebookEnv (..),
    SourceAnswer (..),
    StandingGoal (..),
    okAnswer,
    seededBuiltins,
 )
import Test.CatalogueSim (SimWorld (..), producerPkgs, simWorldCall)
import Test.DiscoverFixtures (SynPkg (..), hitText, hitsOf)
import Test.DiscoverGen (genGoalFreeRow, genPkgName, genValueName)

discoverProducerHintSpec :: Spec
discoverProducerHintSpec = describe "producer affordances (C1, C3)" $ do
    hintSpec
    standingGoalSpec

-- ------------------------------------------------------------------ C3

moduleName :: Text
moduleName = "Some.Module"

hitOf :: Text -> Text -> Text -> Text -> Bool -> Value
hitOf n ty pkg kind withUse =
    object $
        [ "name" .= n
        , "type" .= ty
        , "module" .= moduleName
        , "package" .= pkg
        , "install" .= ("installed" :: Text)
        , "matchKind" .= kind
        , "origin" .= ("hoogle" :: Text)
        ]
            <> ["use" .= ("import " <> moduleName <> " (" <> n <> ")") | withUse]

envWith :: [Value] -> Value
envWith hits =
    object
        [ "query" .= ("q" :: Text)
        , "state" .= ("found" :: Text)
        , "hits" .= hits
        , "shown" .= length hits
        , "omitted" .= (0 :: Int)
        , "total" .= length hits
        , "consulted" .= ([] :: [Value])
        , "next" .= ("act on the hits" :: Text)
        ]

useOfTop :: Value -> Text
useOfTop v = case hitsOf v of
    (h : _) -> hitText "use" h
    [] -> ""

marked :: Value -> Bool
marked v = producerHintMark `T.isInfixOf` allText v
  where
    allText = T.concat . map (hitText "use") . hitsOf

-- | A consumer of the goal beside a nullary producer of it, both attributable.
genPair :: Gen (Text, Text, Text, Text)
genPair = do
    (goal, _, _) <- genGoalFreeRow
    consumer <- genValueName
    producer <- genValueName `suchThat` (/= consumer)
    pkg <- genPkgName
    pure (goal, consumer, producer, pkg)

hintSpec :: Spec
hintSpec = describe "the producer hint rides the top exact hit's use line" $ do
    it "names a nullary same-package producer of an argument type it needs" $
        property $
            forAll genPair $ \(goal, consumer, producer, pkg) ->
                let v =
                        withProducerHint . envWith $
                            [ hitOf consumer (goal <> " -> Text") pkg "exact" True
                            , hitOf producer goal pkg "type" False
                            ]
                 in counterexample (T.unpack (useOfTop v)) $
                        (producerHintMark <> producer <> " :: " <> goal)
                            `T.isSuffixOf` useOfTop v
    it "never names a producer from another package" $
        property $
            forAll ((,) <$> genPair <*> genPkgName) $
                \((goal, consumer, producer, pkg), other) ->
                    pkg
                        /= other
                        ==> not
                            ( marked . withProducerHint . envWith $
                                [ hitOf consumer (goal <> " -> Text") pkg "exact" True
                                , hitOf producer goal other "type" False
                                ]
                            )
    it "never names a producer that is not nullary" $
        property $
            forAll genPair $ \(goal, consumer, producer, pkg) ->
                not
                    ( marked . withProducerHint . envWith $
                        [ hitOf consumer (goal <> " -> Text") pkg "exact" True
                        , hitOf producer ("Int -> " <> goal) pkg "type" False
                        ]
                    )
    it "never annotates a top hit that is not an exact match" $
        property $
            forAll genPair $ \(goal, consumer, producer, pkg) ->
                not
                    ( marked . withProducerHint . envWith $
                        [ hitOf consumer (goal <> " -> Text") pkg "prefix" True
                        , hitOf producer goal pkg "type" False
                        ]
                    )
    it "never invents a use line for a hit that had none" $
        property $
            forAll genPair $ \(goal, consumer, producer, pkg) ->
                not
                    ( marked . withProducerHint . envWith $
                        [ hitOf consumer (goal <> " -> Text") pkg "exact" False
                        , hitOf producer goal pkg "type" False
                        ]
                    )
    it "leaves the envelope schema-clean and the hint short" $
        property $
            forAll genPair $ \(goal, consumer, producer, pkg) ->
                let v =
                        withProducerHint . envWith $
                            [ hitOf consumer (goal <> " -> Text") pkg "exact" True
                            , hitOf producer goal pkg "type" False
                            ]
                    hint = snd (T.breakOn producerHintMark (useOfTop v))
                 in envelopeViolations v === [] .&&. T.length hint <= 80
    it "drops a producer whose name would not fit the cap" $
        property $
            forAll genPair $ \(goal, consumer, producer, pkg) ->
                not
                    ( marked . withProducerHint . envWith $
                        [ hitOf consumer (goal <> " -> Text") pkg "exact" True
                        , hitOf (widen producer) goal pkg "type" False
                        ]
                    )

{- | A name long enough that the hint cannot be stated inside the cap, built
from the generated one so nothing about the shape is assumed.
-}
widen :: Text -> Text
widen n = T.take 90 (T.concat (replicate 90 n))

-- ------------------------------------------------------------------ C1

emptyEnv :: NotebookEnv
emptyEnv = seededBuiltins (NotebookEnv [] [] [] [] [] [])

world :: SimWorld
world = SimWorld [p | p <- pkgs, not (spHidden p)] pkgs
  where
    pkgs = producerPkgs ("plume", "framing", "styling")

{- | The query names nothing the world holds, so the only lead is the goal
itself: without the standing-goal trigger no producer is ever fetched.
-}
standingGoalSpec :: Spec
standingGoalSpec = describe "a standing goal is enough to fetch producers" $ do
    it "attaches producers of the goal for a query that hit nothing exactly" $ do
        answers <-
            attachProducers
                (Just (StandingGoal "Plot" "bars" "plume"))
                (simWorldCall world)
                emptyEnv
                (interpret emptyEnv "unrelatedName")
                [okAnswer "hoogle" []]
        concatMap saHits answers `shouldSatisfy` (not . null)
    it "attaches nothing when no goal stands and nothing hit exactly" $ do
        answers <-
            attachProducers
                Nothing
                (simWorldCall world)
                emptyEnv
                (interpret emptyEnv "unrelatedName")
                [okAnswer "hoogle" []]
        answers `shouldBe` []
    it "attaches nothing when the goal is already produced by a held hit" $ do
        held <-
            attachProducers
                (Just (StandingGoal "Plot" "bars" "plume"))
                (simWorldCall world)
                emptyEnv
                (interpret emptyEnv "unrelatedName")
                [okAnswer "hoogle" []]
        answers <-
            attachProducers
                (Just (StandingGoal "Plot" "bars" "plume"))
                (simWorldCall world)
                emptyEnv
                (interpret emptyEnv "unrelatedName")
                held
        answers `shouldBe` []
    it "asks the session for them, never the index for a bare type" $ do
        seen <- newIORef []
        _ <-
            attachProducers
                (Just (StandingGoal "Plot" "bars" "plume"))
                (recordingCall seen (simWorldCall world))
                emptyEnv
                (interpret emptyEnv "unrelatedName")
                [okAnswer "hoogle" []]
        calls <- readIORef seen
        calls `shouldSatisfy` elem FindByType
        calls `shouldSatisfy` notElem SearchCapability
    it "never names a fit the compiler-backed fit law calls goal-free" $
        property $
            forAll ((,) <$> genGoalFreeRow <*> genPkgName) $
                \((goal, producers, _), pkg) -> ioProperty $ do
                    hits <- standingHits goal pkg ((bottomName, goal) : producers)
                    pure $
                        counterexample (show (map dhName hits)) $
                            bottomName `notElem` map dhName hits
    it "never writes an empty consumer into what it attached" $
        property $
            forAll ((,) <$> genGoalFreeRow <*> genPkgName) $
                \((goal, producers, _), pkg) -> ioProperty $ do
                    hits <- standingHitsFor (StandingGoal goal "" pkg) producers
                    pure $
                        counterexample (show (mapMaybe dhUse hits)) $
                            not (any ("``" `T.isInfixOf`) (mapMaybe dhUse hits))

{- | The inhabitant of every goal that no signature rules out. Named by
'Sabela.AI.FitRule', the oracle the session producer path is judged against.
-}
bottomName :: Text
bottomName = "undefined"

recordingCall ::
    IORef [ToolName] ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    ToolName ->
    Value ->
    IO (Either Text ToolOutcome)
recordingCall seen inner tn args = modifyIORef' seen (++ [tn]) >> inner tn args

-- | A world whose session offers exactly these fits for any goal asked of it.
fitWorldCall ::
    [(Text, Text)] -> ToolName -> Value -> IO (Either Text ToolOutcome)
fitWorldCall fits tn _ = pure . Right . ToolOk $ case tn of
    FindByType -> object ["result" .= blob]
    ListCells -> object ["cells" .= ([] :: [Value])]
    _ -> object []
  where
    blob =
        T.unlines
            ("Valid hole fits include" : ["  " <> n <> " :: " <> ty | (n, ty) <- fits])

standingHits :: Text -> Text -> [(Text, Text)] -> IO [DHit]
standingHits goal pkg = standingHitsFor (StandingGoal goal "consumeIt" pkg)

standingHitsFor :: StandingGoal -> [(Text, Text)] -> IO [DHit]
standingHitsFor sg fits =
    concatMap saHits
        <$> attachProducers
            (Just sg)
            (fitWorldCall fits)
            emptyEnv
            (interpret emptyEnv "unrelatedName")
            [okAnswer "hoogle" []]
