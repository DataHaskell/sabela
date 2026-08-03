{-# LANGUAGE OverloadedStrings #-}

{- | C1-21b: the gate stats every path a cell names, and what it learns must
reach the model — on the write ack and on the refusal alike — described the
same way read_file describes it.
-}
module Test.PathFactsSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Files (execReadFile)
import Sabela.AI.PathGate (pathFacts, pathGateCheck, pathRefs)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.State (App (..), newApp)
import Sabela.State.Environment (Environment (..))
import Test.ArtefactGen (genArtefactText, genIdent)
import Test.GateFixture (insertSrc, withFixture)
import Test.Live (requireLiveIntegration)

-- | Reader-shaped identifiers, the vocabulary PathGate.readsLocalFile knows.
readers :: [Text]
readers = ["readFile", "loadTable", "parseInput", "decodeBlob", "openStore"]

dataExts :: [Text]
dataExts = [".csv", ".tsv", ".json", ".parquet", ".txt", ".dat", ".yaml", ".db"]

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

intOf :: Maybe Value -> Integer
intOf (Just (Number n)) = round n
intOf _ = -1

textOf :: Maybe Value -> Text
textOf (Just (String s)) = s
textOf _ = ""

keysOf :: Value -> [Text]
keysOf (Object o) = sort (map K.toText (KM.keys o))
keysOf _ = []

writeBytes :: FilePath -> FilePath -> BS.ByteString -> IO ()
writeBytes root rel body = do
    createDirectoryIfMissing True (takeDirectory (root </> rel))
    BS.writeFile (root </> rel) body

-- | A cell that hands one path to one reader, both drawn arbitrarily.
genCellFor :: Text -> Gen Text
genCellFor path = do
    reader <- elements readers
    binding <- genIdent
    pure (binding <> " = " <> reader <> " \"" <> path <> "\"")

{- | A work-dir-relative path whose segments are path-shaped: the gate does
not treat a literal carrying a quote as a filename, and neither does a disk.
-}
genRelPath :: Gen Text
genRelPath = do
    (dir, stem, ext, _) <- genPathParts
    pure ("./" <> dir <> "/" <> stem <> ext)

genPathParts :: Gen (Text, Text, Text, Text)
genPathParts = do
    dir <- genSegment
    stem <- genSegment
    ext <- elements dataExts
    other <- elements dataExts `suchThat` (/= ext)
    pure (dir, stem, ext, other)

genSegment :: Gen Text
genSegment =
    T.filter (/= '\'') <$> genIdent `suchThat` (not . T.null . T.filter (/= '\''))

spec :: Spec
spec = do
    liveSpec
    pureSpec

pureSpec :: Spec
pureSpec = describe "path facts reach the model (C1-21b)" $ do
    it "returns one fact record per gateable path the cell names" $
        property $
            forAll ((,) <$> genRelPath <*> genArtefactText) $ \(path, body) ->
                ioProperty $
                    withApp $ \_app root -> do
                        src <- generate (genCellFor path)
                        writeBytes root (T.unpack (T.drop 2 path)) (TE.encodeUtf8 body)
                        facts <- pathFacts root src
                        pure $
                            counterexample (show (src, facts)) $
                                length facts
                                    == length (pathRefs src)
                                    && map (textOf . field "path") facts
                                        == pathRefs src

    it "reports the artefact's real size on every fact" $
        property $
            forAll ((,) <$> genRelPath <*> genArtefactText) $ \(path, body) ->
                ioProperty $
                    withApp $ \_app root -> do
                        src <- generate (genCellFor path)
                        let bytes = TE.encodeUtf8 body
                        writeBytes root (T.unpack (T.drop 2 path)) bytes
                        facts <- pathFacts root src
                        pure $
                            counterexample (show facts) $
                                map (intOf . field "bytes") facts
                                    == [fromIntegral (BS.length bytes)]

    it "has no facts exactly when the cell names no gateable path" $
        property $
            forAll genIdent $ \name ->
                ioProperty $
                    withApp $ \_app root -> do
                        facts <- pathFacts root (name <> " = 1 + 1")
                        pure (counterexample (show facts) (null facts))

    it "describes an artefact the same way read_file does" $
        property $
            forAll ((,) <$> genRelPath <*> genArtefactText) $ \(path, body) ->
                ioProperty $
                    withApp $ \app root -> do
                        src <- generate (genCellFor path)
                        writeBytes root (T.unpack (T.drop 2 path)) (TE.encodeUtf8 body)
                        facts <- pathFacts root src
                        readv <-
                            toolOutcomeValue
                                <$> execReadFile app (object ["path" .= T.drop 2 path])
                        pure $
                            counterexample (show (facts, readv)) $
                                all (agreesWith readv) facts

    it "describes the candidates it offers when it refuses a missing path" $
        property $
            forAll ((,) <$> genPathParts <*> genArtefactText) $ \((dir, stem, ext, other), body) ->
                ioProperty $
                    withApp $ \_app root -> do
                        let named = "./" <> dir <> "/" <> stem <> ext
                            real = dir <> "/" <> stem <> other
                        writeBytes root (T.unpack real) (TE.encodeUtf8 body)
                        src <- generate (genCellFor named)
                        got <- pathGateCheck root [] src
                        pure $ case got of
                            Right _ -> counterexample ("expected a refusal for " <> show named) False
                            Left v ->
                                counterexample (show v) $
                                    candidatesDescribed v

liveSpec :: Spec
liveSpec = describe "the write ack carries them (C1-21b, live)" $
    it "a committed cell that names a file gets that file described back" $ do
        requireLiveIntegration
        withFixture "sabela-path-facts" $ \(app, store, rn) -> do
            let workDir = envWorkDir (appEnv app)
            writeBytes workDir "notes.txt" "alpha,beta\n1,2\n"
            ack <- insertSrc app store rn "sabelaReadsNotes = readFile \"./notes.txt\""
            expected <- pathFacts workDir "sabelaReadsNotes = readFile \"./notes.txt\""
            field "notCommitted" ack `shouldBe` Nothing
            (field "execution" ack >>= field "artefacts")
                `shouldBe` Just (toJSON expected)

{- | Every candidate a refusal names is described, so a failing turn ends
knowing the shape of the file it should have read.
-}
candidatesDescribed :: Value -> Bool
candidatesDescribed v = case (field "candidates" v, field "candidateFacts" v) of
    (Just (Array cs), Just (Array fs)) -> length cs == length fs
    (Just (Array cs), Nothing) -> null (foldr (:) [] cs)
    _ -> False

{- | A fact record and read_file's own answer must agree on every key they
share: one artefact, one description.
-}
agreesWith :: Value -> Value -> Bool
agreesWith readv fact =
    and
        [ field k readv == field k fact
        | k <- keysOf fact
        , k /= "path"
        ]

withApp :: (App -> FilePath -> IO a) -> IO a
withApp k = withSystemTempDirectory "path-facts" $ \root -> do
    app <- newApp root Set.empty Nothing Nothing []
    k app root
