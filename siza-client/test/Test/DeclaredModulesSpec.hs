{-# LANGUAGE OverloadedStrings #-}

{- | A write that declares a dependency raises "what can I import now?". The
index already holds the answer, so the write carries it rather than leaving the
caller to guess module names (docs/discover/live/live_hodatime.md).
-}
module Test.DeclaredModulesSpec (declaredModulesSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getTemporaryDirectory)
import System.Environment (setEnv)
import System.FilePath ((</>))
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Ack (withDeclaredModules)

installFacts :: IO ()
installFacts = do
    dir <- getTemporaryDirectory
    let path = dir </> "siza-declared-modules-facts.tsv"
    TIO.writeFile
        path
        ( T.unlines
            [ "hodatime\thttps://example.invalid/hodatime\tDates\t\
              \Data.HodaTime Data.HodaTime.Instant Data.HodaTime.Duration"
            , "nomodules\t\tNothing exposed\t"
            ]
        )
    setEnv "SABELA_HACKAGE_FACTS" path

depCell :: Text -> Value
depCell pkg = object ["source" .= ("-- cabal: build-depends: " <> pkg)]

committed :: Value
committed = object ["cellId" .= (2 :: Int), "status" .= ("completed" :: Text)]

run :: Value -> Value -> IO Value
run args v = do
    out <- withDeclaredModules args (Right (ToolOk v))
    pure $ case out of
        Right (ToolOk r) -> r
        Right (ToolErr r) -> r
        Left _ -> Null

declaredModulesSpec :: Spec
declaredModulesSpec =
    beforeAll_ installFacts $
        describe "a dependency write says what became importable" $ do
            it "names the modules the declared package exposes" $ do
                v <- run (depCell "hodatime") committed
                modulesFor "hodatime" v
                    `shouldSatisfy` elem "Data.HodaTime"
            it "names the package it is speaking for" $ do
                v <- run (depCell "hodatime") committed
                declaredIn v `shouldBe` ["hodatime"]
            it "leaves a write that declares nothing untouched" $ do
                v <- run (object ["source" .= ("1 + 1" :: Text)]) committed
                v `shouldBe` committed
            it "says nothing for a package the index cannot describe" $ do
                v <- run (depCell "nomodules") committed
                v `shouldBe` committed
            it "says nothing for a package the index does not hold" $ do
                v <- run (depCell "notonhackage") committed
                v `shouldBe` committed
            {- A rejected candidate committed nothing, so nothing became
               importable and claiming otherwise would be a false state. -}
            it "stays silent when the write was refused" $ do
                out <-
                    withDeclaredModules
                        (depCell "hodatime")
                        (Right (ToolErr (object ["error" .= ("nope" :: Text)])))
                case out of
                    Right (ToolErr r) ->
                        KM.member "declared" (asObject r) `shouldBe` False
                    _ -> expectationFailure "expected the refusal to stand"

asObject :: Value -> KM.KeyMap Value
asObject (Object o) = o
asObject _ = KM.empty

-- | The `declared` rows the ack carries, as (package, modules) pairs.
declaredIn :: Value -> [Text]
declaredIn v =
    [ p
    | Just (Array rows) <- [KM.lookup "declared" (asObject v)]
    , Object r <- toList rows
    , Just (String p) <- [KM.lookup "package" r]
    ]

modulesFor :: Text -> Value -> [Text]
modulesFor pkg v =
    [ m
    | Just (Array rows) <- [KM.lookup "declared" (asObject v)]
    , Object r <- toList rows
    , Just (String p) <- [KM.lookup "package" r]
    , p == pkg
    , Just (Array ms) <- [KM.lookup (K.fromText "modules") r]
    , String m <- toList ms
    ]
