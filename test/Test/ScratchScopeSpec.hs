{-# LANGUAGE OverloadedStrings #-}

module Test.ScratchScopeSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Unique (newUnique)
import Test.Hspec
import Test.QuickCheck

import qualified Sabela.SessionTypes as ST

import Sabela.AI.Capabilities.Edit.ScratchVet (
    ScopeFailure (..),
    cellScopeLines,
    runScopeLines,
    scopeEstablished,
    scopeFailureOutcome,
    scopeFailures,
    scopeReportOf,
    vetAlias,
    vetInScope,
 )
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)

cellSrc :: T.Text
cellSrc =
    T.unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , "-- cabal: build-depends: megaparsec"
        , "import Text.Megaparsec"
        , "import qualified Text.Megaparsec.Char.Lexer as L"
        , "type Parser = Parsec Void String"
        , "sc :: Parser ()"
        , "sc = L.space space1 empty empty"
        ]

{- | A module name the harness has never heard of, so nothing in the classifier
can be keyed on which module it is.
-}
genModule :: Gen Text
genModule = T.intercalate "." <$> resize 3 (listOf1 genName)

-- | An upper-initial name the harness has never heard of.
genName :: Gen Text
genName = do
    c <- elements ['A' .. 'Z']
    cs <- resize 6 (listOf (elements (['a' .. 'z'] ++ ['0' .. '9'])))
    pure (T.pack (c : cs))

{- | The shapes a session hands back when an import does not resolve. Every one
must be read as the import's failure; none is a fact about an expression.
-}
genImportFailure :: Text -> Gen Text
genImportFailure m =
    elements
        [ "<no location info>: error: [GHC-87110]\n\
          \    Could not find module \8216"
            <> m
            <> "\8217"
        , "<no location info>: error:\n\
          \    Could not load module \8216"
            <> m
            <> "\8217"
        , "<no location info>: error: [GHC-87110]\n\
          \    Could not find module \8216"
            <> m
            <> "\8217\n\
               \    It is a member of the hidden package \8216some-pkg-0.1\8217."
        ]

{- | The shapes a session hands back when a scope line it did not import is
refused. None hides a body, so none may be reported as masking.
-}
genOtherFailure :: Text -> Gen Text
genOtherFailure n =
    elements
        [ "<interactive>:1:1: error: [GHC-58481]\n\
          \    parse error on input \8216=\8217"
        , "<interactive>:1:1: error: [GHC-76037]\n\
          \    Not in scope: type constructor or class \8216"
            <> n
            <> "\8217"
        , "<interactive>:1:1: error: [GHC-83865]\n\
          \    Expected a type, but \8216"
            <> n
            <> "\8217 has kind \8216* -> *\8217"
        ]

-- | A scope line that is not an import, spelled with a generated name.
synonymLine :: Text -> Text
synonymLine n = "type " <> n <> "_ = " <> n

recordingBackend ::
    IORef [Text] -> (Text -> (Text, Text)) -> IO ST.SessionBackend
recordingBackend probes respond = do
    uid <- newUnique
    let backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbJsonDiagnostics = False
                , ST.sbRunBlock = pure . respond
                , ST.sbRunBlockStreaming = \t _ -> pure (respond t)
                , ST.sbClose = pure ()
                , ST.sbReset = pure backend
                , ST.sbInterrupt = pure ()
                , ST.sbBusy = pure False
                , ST.sbSessionGen = pure 0
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure []
                , ST.sbQueryType = \t -> modifyIORef' probes (++ [t]) >> pure "Int"
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                , ST.sbEvalPureLive =
                    \req -> pure (ST.pureEvalUnavailableResult req "fake backend")
                }
    pure backend

renderOutcomeText :: A.Value -> Text
renderOutcomeText = TE.decodeUtf8 . LBS.toStrict . A.encode

spec :: Spec
spec = describe "scratch scope replay (intention)" $ do
    it "replays the cell's imports" $ do
        cellScopeLines cellSrc
            `shouldSatisfy` elem "import Text.Megaparsec"
        cellScopeLines cellSrc
            `shouldSatisfy` elem "import qualified Text.Megaparsec.Char.Lexer as L"

    it "replays a single-line type synonym — the goal type may be spelled with it" $
        cellScopeLines cellSrc
            `shouldSatisfy` elem "type Parser = Parsec Void String"

    it "does not replay bindings, signatures, pragmas, or cabal comments" $ do
        let ls = cellScopeLines cellSrc
        ls `shouldSatisfy` (not . any ("sc " `T.isPrefixOf`))
        ls `shouldSatisfy` (not . any ("sc ::" `T.isInfixOf`))
        ls `shouldSatisfy` (not . any ("{-#" `T.isPrefixOf`))
        ls `shouldSatisfy` (not . any ("-- cabal:" `T.isPrefixOf`))

    it "preserves source order (a synonym may use an imported type)" $
        cellScopeLines cellSrc
            `shouldBe` [ "import Text.Megaparsec"
                       , "import qualified Text.Megaparsec.Char.Lexer as L"
                       , "type Parser = Parsec Void String"
                       ]

    describe "a scope line that failed is reported as that line's failure" $ do
        it "classifies every shape of import failure as a failure" $
            property $
                forAll genModule $ \m ->
                    forAll (genImportFailure m) $ \out ->
                        let line = "import " <> m
                            rep = scopeReportOf [(line, out)]
                         in map sfLine (scopeFailures rep) `shouldBe` [line]

        it "marks an import failure as masking, since it hides the body" $
            property $
                forAll genModule $ \m ->
                    forAll (genImportFailure m) $ \out ->
                        all sfMasking (scopeFailures (scopeReportOf [("import " <> m, out)]))

        it "reports a refusal that is not an import failure, unmasked" $
            property $
                forAll genName $ \n ->
                    forAll (genOtherFailure n) $ \out ->
                        let line = synonymLine n
                            rep = scopeReportOf [(line, out)]
                         in map sfLine (scopeFailures rep)
                                === [line]
                                .&&. map sfMasking (scopeFailures rep) === [False]

        it "keeps the session's own words as the diagnostic" $
            property $
                forAll genModule $ \m ->
                    forAll (genImportFailure m) $ \out ->
                        all
                            (\f -> m `T.isInfixOf` sfDiagnostic f)
                            (scopeFailures (scopeReportOf [("import " <> m, out)]))

        it "never invents a failure for a line the session said nothing about" $
            property $
                forAll genModule $ \m ->
                    scopeReportOf [("import " <> m, "")] `shouldSatisfy` scopeEstablished

        it "blames only the line that failed, never its neighbours" $
            property $
                forAll ((,) <$> genModule <*> genModule) $ \(good, bad) ->
                    good /= bad ==> forAll (genImportFailure bad) $ \out ->
                        map
                            sfLine
                            ( scopeFailures
                                ( scopeReportOf
                                    [ ("import " <> good, "")
                                    , ("import " <> bad, out)
                                    ]
                                )
                            )
                            === ["import " <> bad]

    describe "establishing a scope keeps every line's verdict" $ do
        it "reports each refused line, not only the last" $
            property $
                forAll (resize 4 (listOf1 genModule)) $ \mods -> ioProperty $ do
                    probes <- newIORef []
                    let bad = take 1 mods <> drop 2 mods
                        failFor l =
                            if any (\m -> l == "import " <> m) bad
                                then ("", "error:\n    Could not find module")
                                else ("", "")
                    backend <- recordingBackend probes failFor
                    report <- runScopeLines backend ["import " <> m | m <- mods]
                    pure
                        ( map sfLine (scopeFailures report)
                            == ["import " <> m | m <- mods, m `elem` bad]
                        )

        it "reports nothing when the session refused nothing" $
            property $
                forAll (resize 4 (listOf1 genModule)) $ \mods -> ioProperty $ do
                    probes <- newIORef []
                    backend <- recordingBackend probes (const ("", ""))
                    report <- runScopeLines backend ["import " <> m | m <- mods]
                    pure (scopeEstablished report)

    describe "a failed scope never becomes a claim about the expression" $ do
        it "does not run the probe when the candidate's import failed" $
            property $
                forAll genModule $ \m -> ioProperty $ do
                    probes <- newIORef []
                    out <- generate (genImportFailure m)
                    backend <- recordingBackend probes (const ("", out))
                    verdict <- vetInScope backend mempty m "someName" "Int"
                    issued <- readIORef probes
                    pure (isLeft verdict && null issued)

        it "runs the probe exactly once when the scope holds" $
            property $
                forAll genModule $ \m -> ioProperty $ do
                    probes <- newIORef []
                    backend <- recordingBackend probes (const ("", ""))
                    verdict <- vetInScope backend mempty m "someName" "Int"
                    issued <- readIORef probes
                    pure (isRight verdict && length issued == 1)

        it "the probe it runs is qualified by the alias the import established" $
            property $
                forAll genModule $ \m -> ioProperty $ do
                    probes <- newIORef []
                    backend <- recordingBackend probes (const ("", ""))
                    _ <- vetInScope backend mempty m "someName" "Int"
                    issued <- readIORef probes
                    pure (all (vetAlias m `T.isInfixOf`) issued)

    describe "the refusal a failed scope earns" $ do
        it "is an error naming the failing line and stamped diagnostic" $
            property $
                forAll genModule $ \m ->
                    forAll (genImportFailure m) $ \out ->
                        let line = "import " <> m
                            payload = scopeFailureOutcome (scopeReportOf [(line, out)])
                            rendered = renderOutcomeText (toolOutcomeValue payload)
                         in toolOutcomeIsError payload
                                && line `T.isInfixOf` rendered
                                && verdictTag VerdictDiagnostic `T.isInfixOf` rendered

        it "carries each failure's masking as classified, not as a constant" $
            property $
                forAll genName $ \n ->
                    forAll ((,) <$> genImportFailure n <*> genOtherFailure n) $
                        \(impOut, othOut) ->
                            let rendered r =
                                    renderOutcomeText
                                        ( toolOutcomeValue
                                            (scopeFailureOutcome (scopeReportOf [r]))
                                        )
                             in "\"masking\":true"
                                    `T.isInfixOf` rendered ("import " <> n, impOut)
                                    && "\"masking\":false"
                                        `T.isInfixOf` rendered (synonymLine n, othOut)

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft
