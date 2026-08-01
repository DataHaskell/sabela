{-# LANGUAGE OverloadedStrings #-}

{- | C1-14a/C1-14b/C1-14e: read_file describes whatever it is pointed at,
list_files counts honestly and speaks one entry shape, and the view a read
returns is decided by the bytes rather than by the name of the file.
-}
module Test.ArtefactViewSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (nub, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Artefact (sampleBytes)
import Sabela.AI.Capabilities.Files (
    entryJson,
    execListFiles,
    execReadFile,
    localEntryJson,
 )
import Sabela.AI.Files (FileEntry (..), entryOf)
import Sabela.AI.GitHub (GhEntry (..))
import Sabela.AI.PeekData (peekData, peekResultJSON)
import Sabela.AI.Types (ToolOutcome, toolOutcomeIsError, toolOutcomeValue)
import Sabela.State (App (..), newApp)
import Test.ArtefactGen (genArtefactText, genBlobBytes, genGridSpec, renderGrid)

extensions :: [String]
extensions = [".csv", ".md", ".json", ".parquet", ".png", ".bin", ""]

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

textOf :: Maybe Value -> Text
textOf (Just (String s)) = s
textOf _ = ""

intOf :: Maybe Value -> Integer
intOf (Just (Number n)) = round n
intOf _ = -1

boolOf :: Maybe Value -> Bool
boolOf (Just (Bool b)) = b
boolOf _ = False

keysOf :: Value -> [Text]
keysOf (Object o) = sort (map K.toText (KM.keys o))
keysOf _ = []

arrayOf :: Maybe Value -> [Value]
arrayOf (Just (Array a)) = foldr (:) [] a
arrayOf _ = []

writeBytes :: FilePath -> FilePath -> BS.ByteString -> IO ()
writeBytes root rel body = do
    createDirectoryIfMissing True (takeDirectory (root </> rel))
    BS.writeFile (root </> rel) body

readVia :: App -> Text -> IO ToolOutcome
readVia app path = execReadFile app (object ["path" .= path])

listVia :: App -> Text -> IO ToolOutcome
listVia app prefix = execListFiles app (object ["path" .= prefix])

spec :: Spec
spec = describe "read_file / list_files describe what is there (C1-14)" $ do
    it "never errors on a file that exists inside the work dir" $
        property $
            forAll ((,) <$> genBlobBytes <*> elements extensions) $ \(ws, ext) ->
                ioProperty $
                    inWorkDir $ \app root -> do
                        let rel = "data/sample" <> ext
                        writeBytes root rel (BS.pack ws)
                        out <- readVia app (T.pack rel)
                        pure
                            (counterexample (show (toolOutcomeValue out)) (not (toolOutcomeIsError out)))

    it "reports the artefact's real size and a content that is a real prefix" $
        property $
            forAll ((,) <$> genBlobBytes <*> elements extensions) $ \(ws, ext) ->
                ioProperty $
                    inWorkDir $ \app root -> do
                        let rel = "data/sample" <> ext
                            bytes = BS.pack ws
                        writeBytes root rel bytes
                        v <- toolOutcomeValue <$> readVia app (T.pack rel)
                        let whole = TE.decodeUtf8With lenientDecode bytes
                            content = textOf (field "content" v)
                        pure $
                            counterexample (show v) $
                                intOf (field "bytes" v)
                                    == fromIntegral (BS.length bytes)
                                    && T.length content
                                        <= sampleBytes
                                    && content
                                        `T.isPrefixOf` whole

    it "chooses the view from the bytes, not from the file's name" $
        property $
            forAll ((,) <$> genArtefactText <*> pure extensions) $ \(txt, exts) ->
                ioProperty $
                    inWorkDir $ \app root -> do
                        views <- mapM (viewUnder app root txt) (zip [0 :: Int ..] exts)
                        pure (counterexample (show views) (length (nub views) == 1))

    it "folds the delimited summary in verbatim: one artefact, one answer" $
        property $
            forAll genGridSpec $ \gs ->
                ioProperty $
                    inWorkDir $ \app root -> do
                        let txt = renderGrid gs
                        writeBytes root "grid.dat" (TE.encodeUtf8 txt)
                        v <- toolOutcomeValue <$> readVia app "grid.dat"
                        pure $
                            counterexample (show v) $
                                field "shape" v == Just (peekResultJSON (peekData 5 txt))

    it "counts the files that exist, not the files it showed" $
        property $
            forAll (choose (0, 12 :: Int)) $ \n ->
                ioProperty $
                    inWorkDir $ \app root -> do
                        mapM_ (\i -> writeBytes root ("t/f" <> show i <> ".csv") "a,b\n1,2\n") [1 .. n]
                        v <- toolOutcomeValue <$> listVia app "t"
                        let shown = arrayOf (field "files" v)
                        pure $
                            counterexample (show v) $
                                intOf (field "count" v)
                                    == fromIntegral n
                                    && intOf (field "shown" v)
                                        == fromIntegral (length shown)
                                    && boolOf (field "truncated" v)
                                        == (fromIntegral n > length shown)

    it "reports every listed file's real size" $
        property $
            forAll (listOf1 (choose (0, 200 :: Int))) $ \sizes ->
                ioProperty $
                    inWorkDir $ \app root -> do
                        mapM_ (uncurry (writeSized root)) (zip [1 :: Int ..] sizes)
                        v <- toolOutcomeValue <$> listVia app "t"
                        let got =
                                [ (textOf (field "path" e), intOf (field "size" e))
                                | e <- arrayOf (field "files" v)
                                ]
                            want =
                                sort
                                    [ ("t/f" <> T.pack (show i) <> ".dat", fromIntegral s)
                                    | (i, s) <- zip [1 :: Int ..] sizes
                                    ]
                        pure (counterexample (show (got, want)) (sort got == want))

    it "speaks one entry shape whichever backend answered" $
        property $
            forAll (choose (0, 200 :: Integer)) $ \size ->
                ioProperty $
                    inWorkDir $ \app root -> do
                        writeSized root 1 (fromIntegral size)
                        v <- toolOutcomeValue <$> listVia app "t"
                        let localKeys = map keysOf (arrayOf (field "files" v))
                            gh = keysOf (entryJson (GhEntry "t/f1.dat" (Just (fromIntegral size))))
                        pure $
                            counterexample (show (v, gh)) $
                                localKeys == [gh]
                                    && gh == ["path", "size"]
                                    && keysOf (entryJson (GhEntry "t/f1.dat" Nothing))
                                        == keysOf (localEntryJson (FileEntry "t/f1.dat" Nothing))

    it "omits a size it could not measure rather than reporting zero" $
        inWorkDir $ \_app root -> do
            e <- entryOf root "gone.dat"
            feBytes e `shouldBe` Nothing
            keysOf (localEntryJson e) `shouldBe` ["path"]

writeSized :: FilePath -> Int -> Int -> IO ()
writeSized root i s =
    writeBytes root ("t/f" <> show i <> ".dat") (BS.replicate s 97)

viewUnder :: App -> FilePath -> Text -> (Int, String) -> IO Text
viewUnder app root txt (i, ext) = do
    let rel = "v" <> show i <> ext
    writeBytes root rel (TE.encodeUtf8 txt)
    v <- toolOutcomeValue <$> readVia app (T.pack rel)
    pure (textOf (field "view" v))

inWorkDir :: (App -> FilePath -> IO a) -> IO a
inWorkDir k = withSystemTempDirectory "artefact-view" $ \root -> do
    app <- newApp root Set.empty Nothing Nothing []
    k app root
