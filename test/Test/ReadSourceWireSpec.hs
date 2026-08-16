{-# LANGUAGE OverloadedStrings #-}

{- | The read_source wire shape, pinned before the executor: the tool def's
schema, both response modes' exact key sets, the version ladder, and the
caps each mode disclosed. Misses live in "Test.ReadSourceMissSpec".
-}
module Test.ReadSourceWireSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ReadSource (
    VersionSource (..),
    resolveVersion,
    versionSourceText,
 )
import Sabela.AI.Capabilities.Tools.World (worldTools)
import Sabela.AI.ReadSourceArgs (ReadSourceReq (..))
import Sabela.Anthropic.Types (ToolDef (..))
import Test.ReadSourceWorld (manyDeclCount, run, textAt, withWorld)

spec :: Spec
spec = describe "read_source wire shape" $ do
    describe "the tool def" $ do
        it "is offered among the world tools" $
            [n | ToolDef n _ _ _ <- worldTools] `shouldSatisfy` elem "read_source"
        it "offers exactly the four arguments, module required" $ do
            let schemas =
                    [ s | ToolDef n _ s _ <- worldTools, n == "read_source"
                    ]
            case schemas of
                [Object o] -> do
                    case KM.lookup "properties" o of
                        Just (Object ps) ->
                            sort (map K.toText (KM.keys ps))
                                `shouldBe` ["module", "name", "package", "version"]
                        _ -> expectationFailure "no properties"
                    KM.lookup "required" o
                        `shouldBe` Just (Array (pure (String "module")))
                _ -> expectationFailure "read_source def missing"

    describe "the version ladder" $ do
        let req = ReadSourceReq "Data.X" Nothing Nothing Nothing
        it "explicit version wins" $
            resolveVersion req{rsVersion = Just "9.9"} (Just "1.0") (Just "2.0") ["3.0"]
                `shouldBe` Just ("9.9", VsRequested)
        it "then the facts index" $
            resolveVersion req (Just "1.0") (Just "2.0") ["3.0"]
                `shouldBe` Just ("1.0", VsFactsIndex)
        it "then hoogle's documented release" $
            resolveVersion req Nothing (Just "2.0") ["3.0"]
                `shouldBe` Just ("2.0", VsHoogleDocs)
        it "then the newest cabal-cached version" $
            resolveVersion req Nothing Nothing ["3.0", "2.9"]
                `shouldBe` Just ("3.0", VsCabalCache)
        it "and states which rung answered" $
            map versionSourceText [VsRequested, VsFactsIndex, VsHoogleDocs, VsCabalCache]
                `shouldBe` ["requested", "facts-index", "hoogle-docs", "cabal-cache"]

    describe "decl mode" $ do
        it "answers with exactly the pinned keys" $
            withWorld $ do
                (isErr, km) <-
                    run
                        ( object
                            [ "module" .= ("Data.HodaTime.Instant" :: Text)
                            , "name" .= ("difference" :: Text)
                            ]
                        )
                isErr `shouldBe` False
                sort (map K.toText (KM.keys km))
                    `shouldBe` [ "content"
                               , "lines"
                               , "located"
                               , "module"
                               , "name"
                               , "note"
                               , "package"
                               , "path"
                               , "source"
                               , "version"
                               , "versionSource"
                               ]
        it "states source, version and its provenance truthfully" $
            withWorld $ do
                (_, km) <-
                    run
                        ( object
                            [ "module" .= ("Data.HodaTime.Instant" :: Text)
                            , "name" .= ("difference" :: Text)
                            ]
                        )
                textAt km "source" `shouldBe` "hackage-sdist"
                textAt km "version" `shouldBe` "0.2.2.1"
                textAt km "versionSource" `shouldBe` "facts-index"
                textAt km "note" `shouldSatisfy` T.isInfixOf "0.2.2.1"
                textAt km "note" `shouldSatisfy` T.isInfixOf "newer release"
        it "carries the decl's span and source" $
            withWorld $ do
                (_, km) <-
                    run
                        ( object
                            [ "module" .= ("Data.HodaTime.Instant" :: Text)
                            , "name" .= ("difference" :: Text)
                            ]
                        )
                KM.lookup "lines" km
                    `shouldBe` Just (object ["from" .= (3 :: Int), "to" .= (4 :: Int)])
                textAt km "content" `shouldSatisfy` T.isInfixOf "difference a b"
                textAt km "located" `shouldBe` "parsed"
        it "carries the aliases the returned decl uses" $
            withWorld $ do
                (isErr, km) <-
                    run
                        ( object
                            [ "module" .= ("Data.HodaTime.Compat" :: Text)
                            , "name" .= ("compatOnly" :: Text)
                            ]
                        )
                isErr `shouldBe` False
                KM.lookup "aliases" km
                    `shouldBe` Just (object ["L" .= ("Data.List" :: Text)])
                sort (map K.toText (KM.keys km))
                    `shouldBe` [ "aliases"
                               , "content"
                               , "lines"
                               , "located"
                               , "module"
                               , "name"
                               , "note"
                               , "package"
                               , "path"
                               , "source"
                               , "version"
                               , "versionSource"
                               ]
        it "omits aliases when the returned decl uses none" $
            withWorld $ do
                (_, km) <-
                    run
                        ( object
                            [ "module" .= ("Data.HodaTime.Instant" :: Text)
                            , "name" .= ("difference" :: Text)
                            ]
                        )
                KM.member "aliases" km `shouldBe` False
        it "restricts aliases to the qualifiers the slice uses" $
            withWorld $ do
                (_, km) <-
                    run
                        ( object
                            [ "module" .= ("Data.HodaTime.Compat" :: Text)
                            , "name" .= ("compatDur" :: Text)
                            ]
                        )
                KM.lookup "aliases" km
                    `shouldBe` Just
                        (object ["I" .= ("Data.HodaTime.Instant" :: Text)])
        it "cuts an over-cap decl at a line and says so" $
            withWorld $ do
                (isErr, km) <-
                    run
                        ( object
                            [ "module"
                                .= ("Data.HodaTime.Calendar.Gregorian" :: Text)
                            , "name" .= ("bulk" :: Text)
                            ]
                        )
                isErr `shouldBe` False
                sort (map K.toText (KM.keys km))
                    `shouldBe` [ "content"
                               , "lines"
                               , "located"
                               , "module"
                               , "name"
                               , "note"
                               , "package"
                               , "path"
                               , "source"
                               , "truncated"
                               , "version"
                               , "versionSource"
                               ]
                KM.lookup "truncated" km `shouldBe` Just (Bool True)
                T.length (textAt km "content") `shouldSatisfy` (<= 8000)

    describe "overview mode" $ do
        it "carries the aliases the shown header and sigs use" $
            withWorld $ do
                (isErr, km) <-
                    run (object ["module" .= ("Data.HodaTime.Compat" :: Text)])
                isErr `shouldBe` False
                KM.lookup "aliases" km
                    `shouldBe` Just
                        (object ["I" .= ("Data.HodaTime.Instant" :: Text)])
        it "answers with exactly the pinned keys" $
            withWorld $ do
                (isErr, km) <-
                    run (object ["module" .= ("Data.HodaTime.Instant" :: Text)])
                isErr `shouldBe` False
                sort (map K.toText (KM.keys km))
                    `shouldBe` [ "count"
                               , "decls"
                               , "header"
                               , "located"
                               , "module"
                               , "note"
                               , "package"
                               , "path"
                               , "shown"
                               , "source"
                               , "version"
                               , "versionSource"
                               ]
        it "caps the decls it shows and still counts them all" $
            withWorld $ do
                (isErr, km) <-
                    run (object ["module" .= ("Data.HodaTime" :: Text)])
                isErr `shouldBe` False
                KM.lookup "shown" km `shouldBe` Just (Number 120)
                KM.lookup "count" km
                    `shouldBe` Just (Number (fromIntegral manyDeclCount))
