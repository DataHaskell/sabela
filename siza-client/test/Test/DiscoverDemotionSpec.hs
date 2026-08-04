{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverDemotionSpec (discoverDemotionSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object)
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Rank (rankKey, stratum)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    seededBuiltins,
 )
import Test.DiscoverFixtures (
    field,
    hitText,
    hitsOf,
    installNamesFileWith,
    runCatArgs,
    stateOf,
    synHackageNames,
    textField,
 )

args :: [(Text, Value)] -> Value
args kvs = object [(K.fromText k, v) | (k, v) <- kvs]

intOf :: Text -> Value -> Int
intOf k v = case field k v of
    Just (Number n) -> round n
    _ -> -1

emptyEnv :: NotebookEnv
emptyEnv = seededBuiltins (NotebookEnv [] [] [] [] [] [])

topicInterp :: Text -> Interpreted
topicInterp q = Interpreted q q Nothing "name" "" []

subHit :: Text -> DHit
subHit m =
    DHit
        "gustish"
        "Int -> Int"
        m
        "zephyr"
        "1.0"
        InstInstalled
        MkSubstring
        "session"
        Nothing
        Nothing
        Nothing
        Nothing

discoverDemotionSpec :: Spec
discoverDemotionSpec = describe "stratum + demotion (R3-T3)" $ do
    describe "within-stratum internal demotion (R3.1-R3.3, R3.7)" $ do
        it "a public module outranks an internal one inside its stratum" $ do
            let public = subHit "Zephyr.Core.Extra"
                internal = subHit "Zephyr.Internal"
                env = emptyEnv
                interp = topicInterp "gust"
            stratum env interp public `shouldBe` stratum env interp internal
            rankKey env interp public
                `shouldSatisfy` (< rankKey env interp internal)
        it "B2: an in-scope prefix hit outranks an unreachable exact hit" $ do
            let inScopePrefix =
                    (subHit "Sabela.Notebook")
                        { dhName = "lineChart"
                        , dhKind = MkPrefix
                        , dhInstall = InstInstalled
                        }
                unreachableExact h =
                    (subHit "Prettyprinter")
                        { dhName = "line"
                        , dhKind = MkExact
                        , dhPackage = "prettyprinter"
                        , dhInstall = h
                        }
                env = emptyEnv
                interp = topicInterp "line"
            mapM_
                ( \state ->
                    rankKey env interp inScopePrefix
                        `shouldSatisfy` (< rankKey env interp (unreachableExact state))
                )
                [InstHidden, InstAbsentKnown, InstAbsentUnknown]

        it "B2: a reachable exact hit still leads an in-scope prefix hit" $ do
            let prefixHit =
                    (subHit "Sabela.Notebook")
                        { dhName = "lineChart"
                        , dhKind = MkPrefix
                        }
                exactHit =
                    (subHit "Sabela.Notebook")
                        { dhName = "line"
                        , dhKind = MkExact
                        }
                env = emptyEnv
                interp = topicInterp "line"
            rankKey env interp exactHit
                `shouldSatisfy` (< rankKey env interp prefixHit)

        it "an exact hit still outranks any public substring hit" $ do
            let exactNoise =
                    (subHit "Zephyr.Internal")
                        { dhName = "gust"
                        , dhKind = MkExact
                        }
                env = emptyEnv
                interp = topicInterp "gust"
            rankKey env interp exactNoise
                `shouldSatisfy` (< rankKey env interp (subHit "Zephyr.Core"))
        it "demoted internal hits are summarised, never dropped" $ do
            installNamesFileWith synHackageNames
            full <- runCatArgs "gust" (args [])
            v <- runCatArgs "gust" (args [("limit", Number 1)])
            intOf "total" v `shouldBe` intOf "total" full
            textField "narrow" v `shouldSatisfy` T.isInfixOf "internal"
        it "ranking is deterministic (R3.7)" $ do
            installNamesFileWith synHackageNames
            a <- runCatArgs "gust" (args [])
            b <- runCatArgs "gust" (args [])
            encode a `shouldBe` encode b

    describe "session-hit version enrichment (carryover 6)" $ do
        it "a session hit gains the version another source holds" $ do
            installNamesFileWith synHackageNames
            v <- runCatArgs "gust" (args [])
            let top = take 1 (hitsOf v)
            top `shouldSatisfy` (not . null)
            forM_ top $ \h -> hitText "version" h `shouldBe` "1.2.0"
        it "a session hit with no known version never leaks raw provenance" $ do
            installNamesFileWith synHackageNames
            v <- runCatArgs "gust" (args [])
            let mine =
                    [h | h <- hitsOf v, hitText "name" h == "puffLoop"]
            mine `shouldSatisfy` (not . null)
            forM_ (take 1 mine) $ \h ->
                hitText "version" h
                    `shouldSatisfy` (not . T.isInfixOf "package env")

    describe "miss guidance points at inventory (R5.3)" $
        it "a nonsense miss names mode=inventory" $ do
            installNamesFileWith synHackageNames
            v <- runCatArgs "qqzzyx" (args [])
            stateOf v `shouldBe` "not_found"
            textField "next" v `shouldSatisfy` T.isInfixOf "inventory"
