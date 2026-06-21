module Test.LanguageSpec (
    parseSpec,
    securitySpec,
    annotateSpec,
    contractSpec,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (parseToolName, toolWireName)
import Siza.Annotate (
    AnnotateReport (..),
    Annotation (..),
    annotateCell,
    annotatedSource,
    renderReport,
 )
import Siza.Discover (Server (..), serverValue)
import Siza.Lang.Haskell (parseHaskell)
import Siza.Language (
    Diagnostic (..),
    Severity (Error, Warning),
    renderDiagnostic,
 )
import Siza.Preflight (advisoryPolicy, preflight, strictPolicy, vettedSource)
import Siza.Security (Capability (..), scanSource)
import Test.Fixtures (foundCaps, stubQuery, unsigned)
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Pre-flight parse
-- ---------------------------------------------------------------------------

parseSpec :: Spec
parseSpec = describe "pre-flight parse (the Vetted seam)" $ do
    it "rejects a syntactically broken cell with a located diagnostic" $ do
        res <- preflight advisoryPolicy "f x = "
        case res of
            Right _ -> expectationFailure "broken cell should not vet"
            Left [] -> expectationFailure "expected at least one diagnostic"
            Left (d : _) -> do
                dgSeverity d `shouldBe` Error
                dgLine d `shouldSatisfy` (/= Nothing)

    it "vets a good cell and round-trips its source" $ do
        let src = "answer :: Int\nanswer = 42"
        res <- preflight advisoryPolicy src
        case res of
            Left ds -> expectationFailure ("good cell rejected: " <> show ds)
            Right v -> vettedSource v `shouldBe` src

    it "langParse surfaces the failure directly (broken)" $
        parseHaskell "data = " `shouldSatisfy` isLeft

    it "langParse accepts a well-formed binding" $
        parseHaskell "x = 1 + 2" `shouldBe` Right ()

-- ---------------------------------------------------------------------------
-- Security capability scan
-- ---------------------------------------------------------------------------

securitySpec :: Spec
securitySpec = describe "security capability scan (over the AST)" $ do
    it "flags System.Process imports (qualified and aliased too)" $ do
        foundCaps "import System.Process" `shouldContain` [ProcessExec]
        foundCaps "import qualified System.Process as P"
            `shouldContain` [ProcessExec]

    it "flags a bare process call site without an import" $
        foundCaps "go = callCommand \"ls\"" `shouldContain` [ProcessExec]

    it "flags unsafePerformIO at the call site" $
        foundCaps "x = unsafePerformIO (pure 1)" `shouldContain` [UnsafeIO]

    it "flags System.IO.Unsafe imports" $
        foundCaps "import System.IO.Unsafe" `shouldContain` [UnsafeIO]

    it "flags unsafeCoerce" $
        foundCaps "y = unsafeCoerce z" `shouldContain` [UnsafeCoerce]

    it "flags readFile (raw file IO)" $
        foundCaps "main = readFile \"/etc/passwd\""
            `shouldContain` [RawFileIO]

    it "flags getEnv / System.Environment (env access)" $ do
        foundCaps "import System.Environment" `shouldContain` [EnvAccess]
        foundCaps "p = getEnv \"PATH\"" `shouldContain` [EnvAccess]

    it "flags raw handle IO (openFile/withFile/hGetContents/hPutStr)" $ do
        foundCaps "main = openFile \"/etc/passwd\" ReadMode >>= hGetContents"
            `shouldContain` [RawFileIO]
        foundCaps "go = do { h <- openFile \"/x\" WriteMode; hPutStr h \"z\" }"
            `shouldContain` [RawFileIO]
        foundCaps "go = withFile \"/x\" ReadMode hGetContents"
            `shouldContain` [RawFileIO]

    it "flags System.Directory destructive ops (import and call site)" $ do
        foundCaps "import System.Directory" `shouldContain` [RawFileIO]
        foundCaps "go = removeDirectoryRecursive \"/\""
            `shouldContain` [RawFileIO]
        foundCaps "go = removeFile \"/etc/passwd\""
            `shouldContain` [RawFileIO]
        foundCaps "go = renameFile \"a\" \"b\"" `shouldContain` [RawFileIO]

    it "flags non-System.Process exec (System.Posix.Process)" $ do
        foundCaps "import System.Posix.Process" `shouldContain` [ProcessExec]
        foundCaps "x = forkProcess (pure ())" `shouldContain` [ProcessExec]
        foundCaps "x = executeFile \"sh\" True [] Nothing"
            `shouldContain` [ProcessExec]

    it "flags a foreign import (FFI)" $
        foundCaps "foreign import ccall \"sin\" c_sin :: Double -> Double"
            `shouldContain` [ForeignImport]

    it "flags a foreign export (FFI surface)" $
        foundCaps "foreign export ccall \"f\" f :: IO ()"
            `shouldContain` [ForeignImport]

    it "does not flag a benign System.IO use (hFlush stdout)" $
        foundCaps "import System.IO\ngo = hFlush stdout" `shouldBe` []

    it "flags network imports" $ do
        foundCaps "import Network.Socket" `shouldContain` [Network]
        foundCaps "import Network.HTTP.Client" `shouldContain` [Network]

    it "passes clean code with no findings" $
        foundCaps "double :: Int -> Int\ndouble x = x * 2" `shouldBe` []

    it "advises (warns) but vets on a hit by default" $ do
        res <- preflight advisoryPolicy "x = unsafePerformIO (pure 1)"
        case res of
            Left ds -> expectationFailure ("advisory should still vet: " <> show ds)
            Right _ -> pure ()

    it "surfaces an advisory finding as a Warning via scanSource" $
        case scanSource advisoryPolicy "x = unsafePerformIO (pure 1)" of
            Left ds -> expectationFailure ("unexpected parse error: " <> show ds)
            Right (d : _) -> dgSeverity d `shouldBe` Warning
            Right [] -> expectationFailure "expected a finding"

    it "--strict blocks a denied capability (no Vetted)" $ do
        res <- preflight strictPolicy "x = unsafePerformIO (pure 1)"
        case res of
            Right _ -> expectationFailure "strict should block unsafePerformIO"
            Left ds -> do
                map dgSeverity ds `shouldSatisfy` all (== Error)
                any (("UnsafeIO" `isInfixOf`) . T.unpack . dgMessage) ds
                    `shouldBe` True

    it "--strict still vets clean code" $ do
        res <- preflight strictPolicy "g :: Int -> Int\ng = (+ 1)"
        either (const False) (const True) res `shouldBe` True

    it "renders a finding diagnostic legibly" $
        renderDiagnostic
            (Diagnostic Warning (Just 1) Nothing "denied capability: x")
            `shouldSatisfy` ("denied capability" `T.isInfixOf`)

    it "scanFindings is a set of distinct capabilities" $
        let cs = foundCaps "import System.Process\nz = system \"ls\""
         in S.fromList cs `shouldBe` S.fromList [ProcessExec]

-- ---------------------------------------------------------------------------
-- Annotate: pull inferred types for unsigned binds
-- ---------------------------------------------------------------------------

annotateSpec :: Spec
annotateSpec = describe "annotate (inferred-type pull)" $ do
    it "finds an unsigned top-level bind" $
        unsigned "answer = 42" `shouldBe` ["answer"]

    it "skips a bind that already has a signature" $
        unsigned "answer :: Int\nanswer = 42" `shouldBe` []

    it "finds the unsigned ones among signed siblings, in source order" $
        unsigned "a = 1\nb :: Int\nb = 2\nc = 3" `shouldBe` ["a", "c"]

    it "ignores data/class decls (no value bind to annotate)" $
        unsigned "data T = T\nclass C a where m :: a" `shouldBe` []

    it "assembles inferred types from a stubbed query" $ do
        report <-
            annotateCell (stubQuery [("a", "Int"), ("c", "[Char]")]) "a = 1\nc = \"x\""
        report
            `shouldBe` AnnReport [AnnInferred "a" "Int", AnnInferred "c" "[Char]"]

    it "degrades a bind the query cannot type to AnnFailed" $ do
        report <- annotateCell (stubQuery [("a", "Int")]) "a = 1\nb = undefined"
        report
            `shouldBe` AnnReport
                [ AnnInferred "a" "Int"
                , AnnFailed "b" "no type inferred (cold session)"
                ]

    it "short-circuits to a parse error without querying" $ do
        report <- annotateCell (stubQuery []) "f x = "
        case report of
            AnnParseError (_ : _) -> pure ()
            other -> expectationFailure ("expected parse error, got " <> show other)

    it "renders an inferred line and a failure comment" $ do
        report <- annotateCell (stubQuery [("a", "Int")]) "a = 1\nb = undefined"
        let out = renderReport report
        ("a :: Int" `T.isInfixOf` out) `shouldBe` True
        ("-- b:" `T.isInfixOf` out) `shouldBe` True

    it "annotatedSource prepends only the inferred signatures" $ do
        let src = "a = 1\nb = undefined"
        report <- annotateCell (stubQuery [("a", "Int")]) src
        let out = annotatedSource report src
        ("a :: Int" `T.isInfixOf` out) `shouldBe` True
        (src `T.isInfixOf` out) `shouldBe` True
        ("b ::" `T.isInfixOf` out) `shouldBe` False

-- ---------------------------------------------------------------------------
-- Shared contract wire shapes
-- ---------------------------------------------------------------------------

contractSpec :: Spec
contractSpec = do
    describe "ToolName round-trip (shared with the sabela contract)" $ do
        it "parses the wire spelling siza-tool.sh used" $
            fmap toolWireName (parseToolName "execute_cell")
                `shouldBe` Just "execute_cell"
        it "rejects an unknown tool" $
            parseToolName "not_a_tool" `shouldBe` Nothing

    describe "Server discovery wire shape (parity with siza-discover.sh)" $ do
        let srv =
                Server
                    { srvBaseUrl = "http://localhost:3000"
                    , srvPort = Just 3000
                    , srvPid = Just "123"
                    , srvWorkDir = Just "/tmp"
                    , srvAuthRequired = Just False
                    , srvTokenHint = Nothing
                    }
        it "appends live:true and keeps baseUrl, matching the bash discover" $
            case serverValue srv of
                Object o -> do
                    KM.lookup "live" o `shouldBe` Just (Bool True)
                    KM.lookup "baseUrl" o
                        `shouldBe` Just (String "http://localhost:3000")
                other -> expectationFailure ("expected object, got " <> show other)

    describe "request envelope shape" $
        it "is {name, input} as the server's aiToolH expects" $
            object ["name" .= ("list_cells" :: String), "input" .= object []]
                `shouldBe` object
                    [ "name" .= ("list_cells" :: String)
                    , "input" .= (object [] :: Value)
                    ]
