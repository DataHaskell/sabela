{-# LANGUAGE OverloadedStrings #-}

{- | C1-14a/C1-14c above the sample size: a file too big to read whole is
described from two samples, and nothing the description says about it may
come from anywhere but the bytes that were actually read.
-}
module Test.ArtefactSampleSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Artefact (sampleBytes)
import Sabela.AI.Capabilities.Files (execReadFile)
import Sabela.AI.Types (ToolOutcome, toolOutcomeIsError, toolOutcomeValue)
import Sabela.State (App (..), newApp)
import Test.ArtefactGen (
    SeamBlob (..),
    genRun,
    genSeamBlob,
    genSizedBytes,
 )

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

stringsOf :: Value -> [Text]
stringsOf v = case field "strings" v of
    Just (Array a) -> [s | String s <- foldr (:) [] a]
    _ -> []

{- | A reported run without its cut marker: what must have been in the file
verbatim.
-}
uncut :: Text -> Text
uncut = T.dropWhileEnd (== '…')

-- | The file's own bytes as characters, which every run must be found in.
asChars :: BS.ByteString -> Text
asChars = T.pack . map (toEnum . fromIntegral) . BS.unpack

readBytes :: BS.ByteString -> IO ToolOutcome
readBytes body = inWorkDir $ \app root -> do
    createDirectoryIfMissing True (takeDirectory (root </> "d/sample"))
    BS.writeFile (root </> "d/sample") body
    execReadFile app (object ["path" .= ("d/sample" :: Text)])

spec :: Spec
spec = do
    sampleSpec
    cutSpec

sampleSpec :: Spec
sampleSpec = describe "a file larger than the sample (C1-14a, C1-14c)" $ do
    it "reports no string that is not in the file: runs never cross the seam" $
        withMaxSuccess 8 $
            property $
                forAll (genSeamBlob sampleBytes) $ \sb ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> readBytes (sbBytes sb)
                        let ss = stringsOf v
                            whole = asChars (sbBytes sb)
                        pure $
                            counterexample (show (ss, sbHeadRun sb, sbTailRun sb)) $
                                all ((`T.isInfixOf` whole) . uncut) ss
                                    && (sbHeadRun sb <> sbTailRun sb) `notElem` ss

    it "still reads both ends: each sample's own run is reported" $
        withMaxSuccess 8 $
            property $
                forAll (genSeamBlob sampleBytes) $ \sb ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> readBytes (sbBytes sb)
                        let ss = stringsOf v
                        pure $
                            counterexample (show ss) $
                                sbHeadRun sb `elem` ss && sbTailRun sb `elem` ss

    it "says which bytes it read and how many it did not" $
        withMaxSuccess 8 $
            property $
                forAll (genSeamBlob sampleBytes) $ \sb ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> readBytes (sbBytes sb)
                        let sampled = field "sampled" v
                            unread = intOf (sampled >>= field "unreadBytes")
                            why = textOf (field "shape" v >>= field "reason")
                        pure $
                            counterexample (show (sampled, why)) $
                                boolOf (field "truncated" v)
                                    && unread == fromIntegral (sbUnread sb)
                                    && intOf (sampled >>= field "headBytes")
                                        == fromIntegral sampleBytes
                                    && intOf (sampled >>= field "tailBytes")
                                        == fromIntegral (sampleBytes `div` 2)
                                    && T.pack (show unread) `T.isInfixOf` why
                                    && "were not read" `T.isInfixOf` why

    it "qualifies the shape's counts exactly when there is more file than sample" $
        withMaxSuccess 20 $
            property $
                forAll (genSizedBytes sampleBytes) $ \body ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> readBytes body
                        let covers = field "shape" v >>= field "countsCover"
                            oversize = BS.length body > sampleBytes
                        pure $
                            counterexample (show (BS.length body, covers)) $
                                covers == (if oversize then Just (String "head-sample") else Nothing)
                                    && boolOf (field "truncated" v) == oversize

    it "states no count of the sample where its coverage is not stated" $
        withMaxSuccess 8 $
            property $
                forAll (choose (sampleBytes + 1, sampleBytes * 2)) $ \n ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> readBytes (BS.replicate n 0xFF)
                        let shape = field "shape" v
                            inShape k = shape >>= field k
                        pure $
                            counterexample (show v) $
                                isNothing (field "undecodable" v)
                                    && intOf (inShape "undecodable") > 0
                                    && inShape "countsCover" == Just (String "head-sample")

    it "never errors, whatever the size" $
        withMaxSuccess 20 $
            property $
                forAll (genSizedBytes sampleBytes) $ \body ->
                    ioProperty $ do
                        out <- readBytes body
                        pure $
                            counterexample (show (toolOutcomeValue out)) $
                                not (toolOutcomeIsError out)

-- | Both caps on the printable-run report disclose what they cut.
cutSpec :: Spec
cutSpec = describe "the printable-run report says what it left out" $ do
    it "accounts for every run it did not show" $
        withMaxSuccess 20 $
            property $
                forAll (nub <$> vectorOf 200 genRun) $ \rs ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> readBytes (runsBlob rs)
                        let shown = length (stringsOf v)
                            omitted = intOf' (field "stringsOmitted" v)
                        pure $
                            counterexample (show (length rs, shown, omitted)) $
                                shown + omitted == length rs && omitted > 0

    it "speaks of a strings field only when there is one" $
        withMaxSuccess 20 $
            property $
                forAll (choose (1, 3000 :: Int)) $ \n ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> readBytes (BS.replicate n 0)
                        let why = textOf (field "shape" v >>= field "reason")
                        pure $
                            counterexample (show (stringsOf v, why)) $
                                null (stringsOf v)
                                    && isNothing (field "strings" v)
                                    && not ("`strings` field" `T.isInfixOf` why)

    it "marks a run it had to cut, and the kept part is really in the file" $
        withMaxSuccess 20 $
            property $
                forAll (choose (200, 400 :: Int)) $ \n ->
                    ioProperty $ do
                        run <- generate (T.replicate n <$> elements ["ab", "xy", "q7"])
                        v <- toolOutcomeValue <$> readBytes (runsBlob [run])
                        let ss = stringsOf v
                        pure $
                            counterexample (show ss) $
                                all ("…" `T.isSuffixOf`) ss
                                    && all ((`T.isInfixOf` run) . uncut) ss
                                    && not (null ss)

-- | Printable runs separated by the NUL bytes that make the file undecodable.
runsBlob :: [Text] -> BS.ByteString
runsBlob rs = BS.concat [BS.replicate 4 0 <> ascii r | r <- rs] <> BS.replicate 4 0

ascii :: Text -> BS.ByteString
ascii = BS.pack . map (fromIntegral . fromEnum) . T.unpack

intOf' :: Maybe Value -> Int
intOf' = fromIntegral . intOf

inWorkDir :: (App -> FilePath -> IO a) -> IO a
inWorkDir k = withSystemTempDirectory "artefact-sample" $ \root -> do
    app <- newApp root Set.empty Nothing Nothing []
    k app root
