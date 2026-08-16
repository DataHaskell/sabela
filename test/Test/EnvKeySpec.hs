{-# LANGUAGE OverloadedStrings #-}

{- | The canonical environment key: spelling variants of one environment are
one key; genuinely different environments (including option ORDER, which GHC
honours) are different keys.
-}
module Test.EnvKeySpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (elements, listOf)
import qualified Test.QuickCheck as QC

import ScriptHs.Parser (CabalMeta (..), SourceRepoPin (..))
import Sabela.Session.EnvKey (canonicalDep, canonicalKeyText, envBucketName)

emptyMeta :: CabalMeta
emptyMeta =
    CabalMeta
        { metaDeps = []
        , metaExts = []
        , metaGhcOptions = []
        , metaExtraLibDirs = []
        , metaExtraIncludeDirs = []
        , metaPackages = []
        , metaSourceRepos = []
        , metaUnknownKeys = []
        }

keyWith :: CabalMeta -> Text
keyWith meta = canonicalKeyText [] meta "9.6.7"

depsKey :: [Text] -> Text
depsKey ds = keyWith emptyMeta{metaDeps = ds}

genEntry :: QC.Gen Text
genEntry = T.pack <$> listOf (elements "abcxyz-09 =<>&^|.*")

spec :: Spec
spec = describe "canonical environment keys" $ do
    describe "canonicalDep" $ do
        it "renders the motivating equivalence to one spelling" $ do
            canonicalDep "containers ==0.6.7" `shouldBe` "containers == 0.6.7"
            canonicalDep "containers==0.6.7" `shouldBe` "containers == 0.6.7"
        it "tokenises compound constraints to one spacing" $
            canonicalDep "base >=4&&<5" `shouldBe` "base >= 4 && < 5"
        it "keeps a bare name bare and hyphenated names whole" $ do
            canonicalDep "text" `shouldBe` "text"
            canonicalDep "base16-bytestring" `shouldBe` "base16-bytestring"
        it "keeps `foo -any` distinct from the package foo-any" $
            canonicalDep "foo -any" `shouldNotBe` canonicalDep "foo-any"
        prop "is idempotent on arbitrary entry text" $
            QC.forAll genEntry $ \e ->
                canonicalDep (canonicalDep e) == canonicalDep e

    describe "spelling variants are one key" $ do
        it "whitespace variants of a constraint" $
            depsKey ["hanalyze", "containers ==0.6.7"]
                `shouldBe` depsKey ["hanalyze", "containers==0.6.7"]
        it "dependency order and repetition" $
            depsKey ["text", "hanalyze", "text"]
                `shouldBe` depsKey ["hanalyze", "text"]
        it "extension reorders" $
            keyWith emptyMeta{metaExts = ["GADTs", "DataKinds"]}
                `shouldBe` keyWith emptyMeta{metaExts = ["DataKinds", "GADTs"]}

    describe "genuinely different environments split" $ do
        it "ghc-option ORDER is significant" $
            keyWith emptyMeta{metaGhcOptions = ["-O2", "-threaded"]}
                `shouldNotBe` keyWith emptyMeta{metaGhcOptions = ["-threaded", "-O2"]}
        it "the GHC version splits" $
            canonicalKeyText [] emptyMeta "9.6.7"
                `shouldNotBe` canonicalKeyText [] emptyMeta "9.12.2"
        it "the resolved local-package overlay splits" $
            canonicalKeyText ["/a/pkg"] emptyMeta "9.6.7"
                `shouldNotBe` canonicalKeyText ["/b/pkg"] emptyMeta "9.6.7"
        it "a version pin splits from the bare name" $
            depsKey ["containers ==0.6.7"] `shouldNotBe` depsKey ["containers"]

    describe "the serialization is injective" $ do
        it "a repo ref containing @ never collides with a subdirectory pin" $ do
            let pinA = SourceRepoPin "https://r" "main@sub" Nothing
                pinB = SourceRepoPin "https://r" "main" (Just "sub")
            keyWith emptyMeta{metaSourceRepos = [pinA]}
                `shouldNotBe` keyWith emptyMeta{metaSourceRepos = [pinB]}
        it "an option containing a comma never collides with two options" $
            keyWith emptyMeta{metaGhcOptions = ["-a,-b"]}
                `shouldNotBe` keyWith emptyMeta{metaGhcOptions = ["-a", "-b"]}
        it "whitespace collapse is not parsing: `> =` stays distinct from `>=`" $
            depsKey ["base > = 4"] `shouldNotBe` depsKey ["base >= 4"]
        it "operator spacing variants of one constraint are still one key" $ do
            depsKey ["base >= 4 && < 5"] `shouldBe` depsKey ["base >=4&&<5"]
            depsKey ["vector ^>= 0.13"] `shouldBe` depsKey ["vector ^>=0.13"]

    describe "the key text itself" $ do
        it "carries the schema component" $
            keyWith emptyMeta `shouldSatisfy` T.isInfixOf "schema:2"
        it "names buckets from the full canonical text" $ do
            envBucketName (keyWith emptyMeta)
                `shouldNotBe` envBucketName (depsKey ["text"])
            envBucketName (keyWith emptyMeta)
                `shouldSatisfy` \n -> take 4 n == "env-" && length n > 20
