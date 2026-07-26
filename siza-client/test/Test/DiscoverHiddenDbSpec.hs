{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverHiddenDbSpec (discoverHiddenDbSpec) where

import Control.Monad (forM, forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Test.CatalogueSim (SimWorld (..), simWorldCall)
import Test.DiscoverFixtures (
    SynPkg (..),
    argText,
    hitText,
    hitsOf,
    installNamesFile,
    runCatArgsIn,
    stateOf,
    synHoogle,
    textField,
 )

breezeAs :: Text -> SynPkg
breezeAs nm =
    SynPkg
        nm
        "0.2.0"
        True
        [("Breeze.Chart", [("chartz", "[(Text, Double)] -> Chart -> Text")])]

extended :: [SynPkg]
extended = synHoogle ++ [breezeAs "breeze"]

installedPkgs :: [SynPkg]
installedPkgs = [p | p <- extended, spName p /= "nimbus"]

args :: [(Text, Value)] -> Value
args kvs = object [(K.fromText k, v) | (k, v) <- kvs]

installedRows :: [(SynPkg, Text, Text)]
installedRows =
    [(p, m, n) | p <- installedPkgs, (m, es) <- spModules p, (n, _) <- es]

rankOf :: Text -> Text -> Value -> Maybe Int
rankOf n m v =
    case [ i
         | (i, h) <- zip [0 ..] (hitsOf v)
         , hitText "name" h == n
         , hitText "module" h == m
         ] of
        (i : _) -> Just i
        [] -> Nothing

targetHit :: Text -> Text -> Value -> Maybe Value
targetHit n m v =
    case [h | h <- hitsOf v, hitText "name" h == n, hitText "module" h == m] of
        (h : _) -> Just h
        [] -> Nothing

discoverHiddenDbSpec :: Spec
discoverHiddenDbSpec =
    beforeAll_ installNamesFile $
        describe "whole-installed-DB symbol catalogue (R5-T1)" $ do
            wholeCatalogueSpec
            nameInvarianceSpec
            preFixIndexSpec
            exactTierLivenessSpec
            probeOrderSpec
            conservationSpec

wholeCatalogueSpec :: Spec
wholeCatalogueSpec = describe "every installed export findable, 3 shapes (R3.1/R2.4)" $ do
    it "exact name in the top 3, install state = package-DB ground truth" $ do
        ledger <- fmap concat . forM installedRows $ \(p, m, n) -> do
            v <- runCatArgsIn extended n (args [])
            pure (rowViolations p m n "exact" v (rankOf n m v))
        ledger `shouldBe` []
    it "module-scoped query surfaces the export with the same truth" $ do
        ledger <- fmap concat . forM installedRows $ \(p, m, n) -> do
            v <- runCatArgsIn extended n (args [("module", String m)])
            pure (rowViolations p m n "scoped" v (rankOf n m v))
        ledger `shouldBe` []
    it "a prose topic query naming the package finds the export (R2.4)" $ do
        ledger <- fmap concat . forM installedRows $ \(p, m, n) -> do
            v <- runCatArgsIn extended (n <> " " <> spName p) (args [])
            pure (rowViolations p m n "prose" v (rankOf n m v))
        ledger `shouldBe` []

rowViolations :: SynPkg -> Text -> Text -> Text -> Value -> Maybe Int -> [Text]
rowViolations p m n shape v mRank =
    concat
        [ [tag <> "not found" | stateOf v /= "found"]
        , case mRank of
            Nothing -> [tag <> "target hit missing"]
            Just r -> [tag <> "buried at rank " <> tShow r | shape == "exact", r > 2]
        , case targetHit n m v of
            Nothing -> []
            Just h ->
                [ tag <> "install state " <> hitText "install" h <> " /= " <> want
                | hitText "install" h /= want
                ]
                    ++ [ tag <> "hidden hit missing its cabal line"
                       | spHidden p
                       , hitText "cabal" h /= "-- cabal: build-depends: " <> spName p
                       ]
        ]
  where
    want = if spHidden p then "hidden" else "installed"
    tag = spName p <> "." <> n <> " (" <> shape <> "): "
    tShow = T.pack . show

nameInvarianceSpec :: Spec
nameInvarianceSpec = describe "library-name substitution invariance" $
    it "three spellings of the hidden package decide byte-identically" $ do
        let spellings = ["breeze", "squall", "quills"] :: [Text]
            queriesFor nm =
                [ ("chartz", args [])
                , ("Breeze.Chart", args [])
                , ("chartz " <> nm, args [])
                ]
        rendered <- forM spellings $ \nm -> do
            let universe = synHoogle ++ [breezeAs nm]
            vs <- mapM (uncurry (runCatArgsIn universe)) (queriesFor nm)
            pure (T.replace nm "#PKG#" (encodeText vs))
        length (nub rendered) `shouldBe` 1
  where
    encodeText = TE.decodeUtf8 . LBS.toStrict . encode

preFixWorld :: SimWorld
preFixWorld =
    SimWorld
        { swSession = [p | p <- extended, spName p /= "nimbus"]
        , swUniverse = synHoogle
        }

runWorldQ :: SimWorld -> Text -> Value -> IO Value
runWorldQ w q a = do
    out <- runDiscoverCall True (simWorldCall w) q a
    pure (case out of ToolOk v -> v; ToolErr v -> v)

preFixIndexSpec :: Spec
preFixIndexSpec = describe "the pre-fix names-only index misses the local package" $ do
    it "PIN: an exact hidden-local symbol is unreachable without the local DB" $ do
        v <- runWorldQ preFixWorld "chartz" (args [])
        stateOf v `shouldBe` "not_found"
    it "module-shaped queries still classify it hidden from session evidence" $ do
        v <- runWorldQ preFixWorld "Breeze.Chart" (args [])
        stateOf v `shouldBe` "found"
        let states = map (hitText "install") (hitsOf v)
        states `shouldSatisfy` elem "hidden"
    it "GREEN: the extended (whole-installed-DB) index reaches the symbol" $ do
        v <- runCatArgsIn extended "chartz" (args [])
        stateOf v `shouldBe` "found"
        rankOf "chartz" "Breeze.Chart" v `shouldSatisfy` maybe False (< 3)

exactOnlyCall :: SimWorld -> ToolName -> Value -> IO (Either Text ToolOutcome)
exactOnlyCall w tn a = case tn of
    SearchCapability
        | exactFlag /= Just (Bool True) ->
            pure . Right . ToolOk $
                object ["query" .= argText "query" a, "hits" .= ([] :: [Value])]
    _ -> simWorldCall w tn a
  where
    exactFlag = case a of
        Object o -> KM.lookup "exact" o
        _ -> Nothing

exactTierLivenessSpec :: Spec
exactTierLivenessSpec = describe "stage-0 exact tier is live for prose/scoped shapes" $ do
    let world = SimWorld [p | p <- extended, spName p /= "nimbus"] extended
        run q a = do
            out <- runDiscoverCall True (exactOnlyCall world) q a
            pure (case out of ToolOk v -> v; ToolErr v -> v)
    it "a prose query finds the hidden export through the exact tier alone" $ do
        v <- run "chartz breeze" (args [])
        stateOf v `shouldBe` "found"
        targetHit "chartz" "Breeze.Chart" v
            `shouldSatisfy` maybe False ((== "hidden") . hitText "install")
    it "a package-shaped query consults the exact tier" $ do
        v <- run "breeze" (args [])
        stateOf v `shouldBe` "found"
    it "a scoped name query stays answered when fuzzy prose fails" $ do
        v <- run "chartz" (args [("module", String "Breeze.Chart")])
        stateOf v `shouldBe` "found"
        rankOf "chartz" "Breeze.Chart" v `shouldBe` Just 0

foreignFlood :: [SynPkg]
foreignFlood =
    [ SynPkg nm "1.0.0" False [(m, [("chartz", "Int -> Text")])]
    | (nm, m) <-
        [ ("plotwind", "Plotwind.Draw")
        , ("chartfall", "Chartfall.Core")
        , ("svgpeak", "Svgpeak.Render")
        ]
    ]

probeOrderSpec :: Spec
probeOrderSpec = describe "query-named package outranks fuzzy noise in the probe" $
    it "a prose query naming the hidden package classifies it hidden" $ do
        let universe = foreignFlood ++ [breezeAs "breeze"]
        v <- runCatArgsIn universe "chartz breeze" (args [])
        stateOf v `shouldBe` "found"
        targetHit "chartz" "Breeze.Chart" v
            `shouldSatisfy` maybe False ((== "hidden") . hitText "install")
        targetHit "chartz" "Breeze.Chart" v
            `shouldSatisfy` maybe
                False
                ((== "-- cabal: build-depends: breeze") . hitText "cabal")

conservationSpec :: Spec
conservationSpec = describe "union-merge conservation over the extension (R3.3)" $
    it "hidden-local rows never displace an existing source's answer" $ do
        let keysOf v = [(hitText "name" h, hitText "module" h) | h <- hitsOf v]
            names =
                nub [n | p <- synHoogle, (_, es) <- spModules p, (n, _) <- es]
        forM_ names $ \n -> do
            base <- runCatArgsIn synHoogle n (args [])
            ext <- runCatArgsIn extended n (args [])
            forM_ (keysOf base) $ \k ->
                (n, k `elem` keysOf ext) `shouldBe` (n, True)
