{-# LANGUAGE OverloadedStrings #-}

{- | Controlled-catalogue suite per testing-plan R10(a): the four-state install
resolver and union merge exercised over SYNTHETIC packages, with a false-denial
ledger asserting 0 over the whole catalogue, plus bench-shape fixtures as
secondary checks (granite-shaped hidden card, D.col alias, displayHtml).
-}
module Test.DiscoverCatalogueSpec (discoverCatalogueSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.DiscoverTool (runDiscoverTool)
import Test.DiscoverFixtures

discoverCatalogueSpec :: Spec
discoverCatalogueSpec =
    beforeAll_ installNamesFile $
        describe "discover controlled catalogue (R10a)" $ do
            describe "false-denial ledger (R1.1, R1.5)" $
                it "denies nothing the catalogue holds — ledger stays empty" $ do
                    ledger <- concat <$> mapM denialOf discoverables
                    ledger `shouldBe` []

            describe "four install states, never conflated (R1.3)" $ do
                it "an exposed export is installed" $ do
                    v <- runCat "gust"
                    firstHit v "install" `shouldBe` "installed"
                it "a hidden export says hidden and carries the exact cabal line" $ do
                    v <- runCat "bars"
                    firstHit v "install" `shouldBe` "hidden"
                    firstHit v "cabal"
                        `shouldBe` "-- cabal: build-depends: cumulus"
                it "an absent-but-known package is absent-known with the install action (R5.4)" $ do
                    v <- runCat "nimbus"
                    let installs = map (hitText "install") (hitsOf v)
                    installs `shouldSatisfy` elem "absent-known"
                    textField "next" v
                        `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: nimbus"
                it "an unknown name is not_found naming every consulted source (R1.2)" $ do
                    v <- runCat "frobwizzle"
                    stateOf v `shouldBe` "not_found"
                    let srcs = consultedSources v
                    srcs `shouldSatisfy` elem "session"
                    srcs `shouldSatisfy` elem "hoogle"
                    srcs `shouldSatisfy` elem "hackage"
                it "the miss note never instructs the retry loop" $ do
                    v <- runCat "frobwizzle"
                    let nxt = textField "next" v
                    nxt `shouldSatisfy` (not . T.isInfixOf "retry discover")
                    nxt `shouldSatisfy` (not . T.isInfixOf "Do not invent")

            describe "notebook environment resolution (R1.5, R1.6, R2.2)" $ do
                it "resolves an alias-qualified name through the notebook import" $ do
                    v <- runCat "Z.gust"
                    stateOf v `shouldBe` "found"
                    firstHit v "name" `shouldBe` "gust"
                    let interp = T.pack (show (field "interpreted" v))
                    interp `shouldSatisfy` T.isInfixOf "Zephyr.Core"
                it "reports a notebook binding as notebook-defined" $ do
                    v <- runCat "gustTotal"
                    firstHit v "install" `shouldBe` "notebook"
                it "never denies a prompt-documented builtin" $ do
                    v <- runCat "displayHtml"
                    stateOf v `shouldBe` "found"
                    firstHit v "install" `shouldBe` "builtin"

            describe "ambiguity (R4.1, R4.5)" $ do
                it "an ambiguous name returns every provider with provenance" $ do
                    v <- runCat "lull"
                    let pkgs = map (hitText "package") (hitsOf v)
                    pkgs `shouldSatisfy` elem "zephyr"
                    pkgs `shouldSatisfy` elem "stratus"
                it "promotes the notebook's imported module and says why" $ do
                    v <- runCat "lull"
                    firstHit v "module" `shouldBe` "Zephyr.Core"
                    firstHit v "use" `shouldSatisfy` T.isInfixOf "Z"

            describe "bench-shape fixtures (secondary checks, R9.4, R9.5)" $ do
                it "hidden-package prose reaches the card + signature in one call" $ do
                    v <- runCat "bar plot cumulus"
                    case field "card" v of
                        Just c -> do
                            textField "status" c `shouldBe` "hidden-package"
                            textField "cabal" c
                                `shouldBe` "-- cabal: build-depends: cumulus"
                        Nothing -> expectationFailure "no card in envelope"
                    let sigs = map (hitText "type") (hitsOf v)
                    sigs `shouldSatisfy` elem "[(Text, Double)] -> Plot -> Text"
                it "\"bar chart granite\" returns the granite hidden card + bars" $ do
                    ToolOk v <- runDiscoverTool True graniteCall "bar chart granite"
                    case field "card" v of
                        Just c -> do
                            textField "status" c `shouldBe` "hidden-package"
                            textField "cabal" c
                                `shouldBe` "-- cabal: build-depends: granite"
                        Nothing -> expectationFailure "no card in envelope"
                    map (hitText "name") (hitsOf v) `shouldSatisfy` elem "bars"
                it "D.col resolves via the notebook's DataFrame alias" $ do
                    ToolOk v <- runDiscoverTool False dataframeCall "D.col"
                    stateOf v `shouldBe` "found"
                    firstHit v "name" `shouldBe` "col"

-- | The (query, state) denial entries for one discoverable name.
denialOf :: Text -> IO [(Text, Text)]
denialOf n = do
    v <- runCat n
    pure [(n, stateOf v) | stateOf v /= "found"]

firstHit :: Value -> Text -> Text
firstHit v k = case hitsOf v of
    (h : _) -> hitText k h
    [] -> ""

consultedSources :: Value -> [Text]
consultedSources v = case field "consulted" v of
    Just (Array a) -> map (textField "source") (toList a)
    _ -> []

-- | Bench-shape mock: granite installed-but-hidden, bars in Graphics.Granite.
graniteCall :: ToolName -> Value -> IO (Either Text ToolOutcome)
graniteCall ListCells _ =
    pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
graniteCall FindFunction args
    | argIs "query" "Graphics.Granite" args =
        pure . Right . ToolOk $
            object
                [ "module" .= ("Graphics.Granite" :: Text)
                , "status" .= ("hidden-package" :: Text)
                , "package" .= ("granite" :: Text)
                , "cabal" .= ("-- cabal: build-depends: granite" :: Text)
                ]
    | otherwise =
        pure (Right (ToolOk (object ["matches" .= ([] :: [Value])])))
graniteCall SearchCapability _ =
    pure . Right . ToolOk $
        object
            [ "hits"
                .= [ object
                        [ "package" .= ("granite" :: Text)
                        , "cabal"
                            .= ("-- cabal: build-depends: granite" :: Text)
                        , "modules" .= (["Graphics.Granite"] :: [Text])
                        , "api"
                            .= [ object
                                    [ "name" .= ("bars" :: Text)
                                    , "module" .= ("Graphics.Granite" :: Text)
                                    , "type"
                                        .= ("[(Text, Double)] -> Plot -> Text" :: Text)
                                    ]
                               ]
                        ]
                   ]
            ]
graniteCall _ _ = pure (Left "unsupported")

-- | Bench-shape mock: the notebook imports DataFrame qualified as D.
dataframeCall :: ToolName -> Value -> IO (Either Text ToolOutcome)
dataframeCall ListCells _ =
    pure . Right . ToolOk $
        object
            [ "cells"
                .= [ object
                        [ "source"
                            .= ("import qualified DataFrame as D" :: Text)
                        , "defines" .= ([] :: [Text])
                        ]
                   ]
            ]
dataframeCall FindFunction args
    | argIs "query" "col" args =
        pure . Right . ToolOk $
            object
                [ "matches"
                    .= [ object
                            [ "module" .= ("DataFrame" :: Text)
                            , "name" .= ("col" :: Text)
                            , "type"
                                .= ("Columnable a => Text -> Expr a" :: Text)
                            , "via" .= ("name" :: Text)
                            ]
                       ]
                ]
    | otherwise =
        pure (Right (ToolOk (object ["matches" .= ([] :: [Value])])))
dataframeCall _ _ = pure (Left "unsupported")

argIs :: Text -> Text -> Value -> Bool
argIs k want (Object o) = KM.lookup (K.fromText k) o == Just (String want)
argIs _ _ _ = False
