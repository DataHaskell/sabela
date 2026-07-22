{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.TypecheckPrimitiveSpec (spec) where

import qualified Data.Text as T
import Sabela.Handlers (ReplSupport (..), setupReplProject)
import Sabela.Session (Session, mkSessionConfig, runBlock)
import Sabela.Session.Process (closeSession, newSession)
import Sabela.Session.Query (
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    queryBindings,
    typecheckLetDeclarations,
 )
import ScriptHs.Parser (CabalMeta (..))
import System.Environment (setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "flagged no-add type-check feasibility prototype" $ do
    it "defaults on, with explicit off as the escape hatch" $ do
        unsetEnv "SABELA_TYPECHECK_PRIMITIVE"
        classifyTypecheckInput "candidate = 1 + 1" `shouldBe` ValueBindings
        classifyTypecheckInput "(_ :: Maybe Int)" `shouldBe` ValueExpression

    it "gracefully routes declaration forms outside the Path-2 subset" $ do
        map classifyTypecheckInput
            [ "data Candidate = Candidate"
            , "class Candidate a where candidate :: a"
            , "instance Show Candidate where show _ = \"Candidate\""
            , "import Data.Map"
            , "type Candidate = Int"
            ]
            `shouldBe` replicate 5 OutsideValueSubset

    it "checks good/bad declarations and leaves bindings byte-identical" $
        withPrimitiveSession $ \sess -> do
            _ <- runBlock sess "let liveSeed = [42 :: Int]"
            bindingsBefore <- queryBindings sess
            good <- typecheckLetDeclarations sess "candidate = head liveSeed + 1"
            bad <- typecheckLetDeclarations sess "candidate = head liveSeed + \"wrong\""
            bindingsAfter <- queryBindings sess
            good `shouldSatisfy` tcSucceeded
            bad `shouldSatisfy` not . tcSucceeded
            tcDiagnostics bad `shouldSatisfy` T.isInfixOf "No instance"
            bindingsAfter `shouldBe` bindingsBefore

    it "returns typed-hole diagnostics and fits without pollution" $
        withPrimitiveSession $ \sess -> do
            bindingsBefore <- queryBindings sess
            result <- typecheckLetDeclarations sess "candidate = (_ :: Maybe Int)"
            bindingsAfter <- queryBindings sess
            tcSucceeded result `shouldBe` False
            tcDiagnostics result `shouldSatisfy` T.isInfixOf "Found hole"
            tcDiagnostics result `shouldSatisfy` T.isInfixOf "Nothing"
            bindingsAfter `shouldBe` bindingsBefore

    it "preserves bindings byte-for-byte over generated value and hole queries" $
        withPrimitiveSession $ \sess -> do
            _ <- runBlock sess "let liveSeed = [42 :: Int]"
            bindings0 <- queryBindings sess
            mapM_ (typecheckLetDeclarations sess) generatedCandidates
            bindingsN <- queryBindings sess
            bindingsN `shouldBe` bindings0

withPrimitiveSession :: (Session -> IO a) -> IO a
withPrimitiveSession action =
    withSystemTempDirectory "sabela-typecheck" $ \dir -> do
        setEnv "SABELA_TYPECHECK_PRIMITIVE" "1"
        setupReplProject BareRepl [] dir emptyMeta
        cfg <- mkSessionConfig dir dir
        Just sess <- timeout 60_000_000 (newSession cfg)
        result <- action sess
        closeSession sess
        unsetEnv "SABELA_TYPECHECK_PRIMITIVE"
        pure result

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

generatedCandidates :: [T.Text]
generatedCandidates =
    [ "candidate" <> T.pack (show n) <> " = head liveSeed + " <> T.pack (show n)
    | n <- [0 :: Int .. 20]
    ]
        <> [ "hole" <> T.pack (show n) <> " = (_ :: Maybe Int)"
           | n <- [0 :: Int .. 20]
           ]
