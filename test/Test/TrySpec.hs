{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.TrySpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (
    doesFileExist,
    findExecutable,
    listDirectory,
    makeAbsolute,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Sabela.AI.Capabilities.Try (execTry)
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Server (newApp)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)

spec :: Spec
spec = describe "unified try operation" $ do
    it "fails Python closed without executing it" $
        withSystemTempDirectory "sabela-try-python" $ \dir -> do
            app <- newApp dir Set.empty Nothing Nothing []
            let marker = dir </> "python-escaped"
            outcome <-
                execTry
                    app
                    ( object
                        [ "language" .= ("Python" :: Text)
                        , "code" .= ("open(" <> T.pack (show marker) <> ", 'w').write('x')")
                        ]
                    )
            toolOutcomeIsError outcome `shouldBe` True
            doesFileExist marker `shouldReturn` False
            textField "route" (toolOutcomeValue outcome) `shouldBe` Just "unavailable"

    it
        "re-materializes notebook bindings, discards candidate state, and leaves no try directory"
        $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH"
                Just _ ->
                    withSystemTempDirectory "sabela-try-context" $ \dir -> do
                        overlay <- supportOverlay
                        app <- newApp dir Set.empty Nothing Nothing overlay
                        let notebook =
                                Notebook
                                    "try.md"
                                    [ codeCell 2 "answer = seed + 2"
                                    , codeCell 1 "seed = (40 :: Int)"
                                    ]
                        modifyNotebook (appNotebook app) (const notebook)
                        beforeNotebook <- readNotebook (appNotebook app)
                        beforeDirs <- tryDirectories app

                        first <- bounded (execTry app (object ["code" .= ("answer" :: Text)]))
                        toolOutcomeIsError first `shouldBe` False
                        let firstValue = toolOutcomeValue first
                        textField "route" firstValue `shouldBe` Just "disposable_scratch"
                        textField "stdout" firstValue `shouldBe` Just "42"
                        intArrayField "replayedCells" firstValue `shouldBe` [1, 2]

                        defined <-
                            bounded
                                ( execTry
                                    app
                                    (object ["code" .= ("scratchOnly = (99 :: Int)\nscratchOnly" :: Text)])
                                )
                        toolOutcomeIsError defined `shouldBe` False
                        textField "stdout" (toolOutcomeValue defined) `shouldBe` Just "99"

                        absent <-
                            bounded (execTry app (object ["code" .= ("scratchOnly" :: Text)]))
                        toolOutcomeIsError absent `shouldBe` True

                        afterNotebook <- readNotebook (appNotebook app)
                        afterDirs <- tryDirectories app
                        afterNotebook `shouldBe` beforeNotebook
                        afterDirs `shouldBe` beforeDirs

    it
        "live_test4 regression: a red prefix cell is skipped so the candidate is reached and succeeds"
        $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH"
                Just _ ->
                    withSystemTempDirectory "sabela-try-skip" $ \dir -> do
                        overlay <- supportOverlay
                        app <- newApp dir Set.empty Nothing Nothing overlay
                        let greenCell = codeCell 1 "baseline = (100 :: Int)"
                            redCell =
                                (codeCell 4 "import Sabela.Notebook\nline (_ :: Point) (_ :: Point)")
                                    { cellError =
                                        Just
                                            "cell 4, line 4: Expecting two more arguments to `Point'\n\
                                            \Expected a type, but `Point' has kind `Type -> Type -> Type'"
                                    }
                            notebook = Notebook "sine.md" [greenCell, redCell]
                        modifyNotebook (appNotebook app) (const notebook)

                        outcome <-
                            bounded
                                ( execTry
                                    app
                                    ( object
                                        [ "code"
                                            .= ( "baseline + length (map (sin . fromIntegral) [1 .. 10 :: Int])" ::
                                                    Text
                                               )
                                        ]
                                    )
                                )
                        let v = toolOutcomeValue outcome
                        toolOutcomeIsError outcome `shouldBe` False
                        textField "route" v `shouldBe` Just "disposable_scratch"
                        textField "outcome" v `shouldBe` Just "ok"
                        textField "stdout" v `shouldBe` Just "110"
                        intArrayField "replayedCells" v `shouldBe` [1]
                        skippedCellIds v `shouldBe` [4]

    it "runs an IO candidate on the disposable route instead of refusing it" $ do
        cabal <- findExecutable "cabal"
        case cabal of
            Nothing -> pendingWith "cabal not found on PATH"
            Just _ ->
                withSystemTempDirectory "sabela-try-io" $ \dir -> do
                    overlay <- supportOverlay
                    app <- newApp dir Set.empty Nothing Nothing overlay
                    before <- readNotebook (appNotebook app)
                    outcome <-
                        bounded
                            ( execTry
                                app
                                (object ["code" .= ("putStrLn \"sabela-io-ran\"" :: Text)])
                            )
                    let v = toolOutcomeValue outcome
                    toolOutcomeIsError outcome `shouldBe` False
                    textField "route" v `shouldBe` Just "disposable_scratch"
                    textField "outcome" v `shouldBe` Just "ok"
                    textField "stdout" v
                        `shouldSatisfy` maybe False (T.isInfixOf "sabela-io-ran")
                    readNotebook (appNotebook app) `shouldReturn` before

    it "reports a display builtin as a runnable trial, not an unavailable one" $ do
        cabal <- findExecutable "cabal"
        case cabal of
            Nothing -> pendingWith "cabal not found on PATH"
            Just _ ->
                withSystemTempDirectory "sabela-try-display" $ \dir -> do
                    overlay <- supportOverlay
                    app <- newApp dir Set.empty Nothing Nothing overlay
                    outcome <-
                        bounded
                            ( execTry
                                app
                                (object ["code" .= ("displaySvg \"<svg/>\"" :: Text)])
                            )
                    let v = toolOutcomeValue outcome
                    textField "route" v `shouldBe` Just "disposable_scratch"
                    textField "outcome" v `shouldSatisfy` (/= Just "unavailable")

    it "try-dep-autofix: a trial-declared dependency is never a bare ok" $ do
        cabal <- findExecutable "cabal"
        case cabal of
            Nothing -> pendingWith "cabal not found on PATH"
            Just _ ->
                withSystemTempDirectory "sabela-try-hidden" $ \dir -> do
                    overlay <- supportOverlay
                    app <- newApp dir Set.empty Nothing Nothing overlay
                    outcome <-
                        bounded (execTry app (object ["code" .= sineHiddenText]))
                    let v = toolOutcomeValue outcome
                    textField "stderr" v
                        `shouldSatisfy` maybe True (not . T.isInfixOf "hidden package")
                    let autofix = textField "autofix" v
                    autofix
                        `shouldSatisfy` maybe
                            False
                            (T.isInfixOf "-- cabal: build-depends: text")
                    autofix
                        `shouldSatisfy` maybe False (T.isInfixOf "sineWaveSvg")
                    autofix `shouldNotBe` Nothing

    it "strips `main` mechanically instead of refusing the candidate" $ do
        cabal <- findExecutable "cabal"
        case cabal of
            Nothing -> pendingWith "cabal not found on PATH"
            Just _ ->
                withSystemTempDirectory "sabela-try-main" $ \dir -> do
                    overlay <- supportOverlay
                    app <- newApp dir Set.empty Nothing Nothing overlay
                    outcome <-
                        bounded
                            ( execTry
                                app
                                (object ["code" .= ("main = putStrLn \"from-main\"" :: Text)])
                            )
                    let v = toolOutcomeValue outcome
                    textField "normalized" v
                        `shouldSatisfy` maybe False (T.isInfixOf "main")
                    textField "outcome" v `shouldBe` Just "ok"
                    textField "stdout" v
                        `shouldSatisfy` maybe False (T.isInfixOf "from-main")

sineHiddenText :: Text
sineHiddenText =
    T.unlines
        [ "import Data.Text (Text)"
        , "import qualified Data.Text as T"
        , "import Data.List (intercalate)"
        , ""
        , "sineWaveSvg :: Text"
        , "sineWaveSvg ="
        , "  let width = 400"
        , "      height = 200"
        , "      nPoints = 1000"
        , "      xs = [fromIntegral i / fromIntegral nPoints | i <- [0..nPoints]]"
        , "      points = [(x, sin (2*pi*x)) | x <- xs]"
        , "      scaled = map (\\(x,y) -> (x * fromIntegral width, \
          \(1 - y)/2 * fromIntegral height)) points"
        , "      pathData = intercalate \" L\" $ \
          \map (\\(x,y)-> show x ++ \",\" ++ show y) scaled"
        , "  in T.pack $ \"<svg xmlns=\\\"http://www.w3.org/2000/svg\\\" \
          \viewBox=\\\"0 0 \"++show width++\" \"++show height++\"\\\">\
          \<path d=\\\"M\"++pathData++\"\\\" stroke=\\\"black\\\" \
          \fill=\\\"none\\\"/></svg>\""
        , ""
        , "main = putStrLn (T.unpack sineWaveSvg)"
        ]

bounded :: IO a -> IO a
bounded action = do
    result <- timeout 180_000_000 action
    case result of
        Nothing -> expectationFailure "try integration timed out" >> error "unreachable"
        Just value -> pure value

codeCell :: Int -> Text -> Cell
codeCell cid source = Cell cid CodeCell Haskell source [] Nothing False

supportOverlay :: IO [FilePath]
supportOverlay = do
    present <- doesFileExist ("sabela-notebook" </> "sabela-notebook.cabal")
    if present then (: []) <$> makeAbsolute "sabela-notebook" else pure []

tryDirectories :: App -> IO [FilePath]
tryDirectories app =
    map T.unpack . filter ("sabela-try" `T.isPrefixOf`) . map T.pack
        <$> listDirectory (envTmpDir (appEnv app))

field :: Text -> Value -> Maybe Value
field key (Object obj) = KM.lookup (Key.fromText key) obj
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of
    Just (String text) -> Just text
    _ -> Nothing

intArrayField :: Text -> Value -> [Int]
intArrayField key value = case field key value of
    Just (Array values) ->
        [ round n
        | Number n <- foldr (:) [] values
        ]
    _ -> []

skippedCellIds :: Value -> [Int]
skippedCellIds value = case field "skippedCells" value of
    Just (Array values) ->
        [ round n
        | entry <- foldr (:) [] values
        , Just (Number n) <- [field "cellId" entry]
        ]
    _ -> []
