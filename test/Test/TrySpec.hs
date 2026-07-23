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
