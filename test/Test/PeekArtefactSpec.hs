{-# LANGUAGE OverloadedStrings #-}

{- | C1-14b/C1-14c through the delimited reader: it answers about the same
bytes the describer read, says each thing about the file once, and states no
count where it does not also say which bytes it counted.
-}
module Test.PeekArtefactSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Artefact (sampleBytes)
import Sabela.AI.Capabilities.Query (execPeekData)
import Sabela.AI.Types (ToolOutcome, toolOutcomeIsError, toolOutcomeValue)
import Sabela.State (newApp)
import Test.ArtefactGen (
    GridSpec (..),
    genGridSpec,
    genOversizeGrid,
    genUndecodableBytes,
    renderGrid,
 )

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

hasKey :: Value -> Text -> Bool
hasKey v k = isJust (field k v)

intOf :: Maybe Value -> Integer
intOf (Just (Number n)) = round n
intOf _ = -1

-- | Every object anywhere in a payload, the payload itself included.
objectsIn :: Value -> [Value]
objectsIn v = case v of
    Object o -> v : concatMap objectsIn (KM.elems o)
    Array a -> concatMap objectsIn (foldr (:) [] a)
    _ -> []

{- | The fields a reader states as a count of what it read. An object holding
one of these has counted something, and must say what it counted over.
-}
countKeys :: [Text]
countKeys = ["lines", "rowCount", "undecodable"]

peekBytes :: [Pair] -> BS.ByteString -> IO ToolOutcome
peekBytes extra body =
    withSystemTempDirectory "peek-artefact" $ \root -> do
        app <- newApp root Set.empty Nothing Nothing []
        createDirectoryIfMissing True (takeDirectory (root </> "d/sample"))
        BS.writeFile (root </> "d/sample") body
        execPeekData app (object (("path" .= ("d/sample" :: Text)) : extra))

-- | An oversize delimited body and a row count the caller chose.
overSample :: Gen (BS.ByteString, Int)
overSample = do
    body <- TE.encodeUtf8 <$> genOversizeGrid sampleBytes
    n <- choose (0, 20)
    pure (body, n)

{- | An oversize body that is genuinely a table. A one-column file has no
delimiter to find, so it is no counterexample to reading tables.
-}
overTable :: Gen (BS.ByteString, Int)
overTable = do
    gs <- genGridSpec `suchThat` table
    let one = renderGrid gs
        reps = sampleBytes `div` max 1 (T.length one) + 2
    n <- choose (0, 20)
    pure (TE.encodeUtf8 (T.replicate reps one), n)
  where
    table g = length (gsRows g) >= 2 && all ((>= 2) . length) (gsRows g)

-- | A counterexample the size of the case, not the size of the file.
sizeOf :: (BS.ByteString, Int) -> String
sizeOf (body, n) = show (BS.length body, n)

keysOf :: Value -> [Text]
keysOf (Object o) = map K.toText (KM.keys o)
keysOf _ = []

spec :: Spec
spec = describe "the delimited peek answers from one artefact (C1-14b)" $ do
    it "answers for bytes that do not decode rather than erroring" $
        withMaxSuccess 20 $
            property $
                forAll genUndecodableBytes $ \body ->
                    ioProperty $ do
                        out <- peekBytes [] body
                        let v = toolOutcomeValue out
                        pure $
                            counterexample (show (BS.length body, keysOf v)) $
                                isLeft (TE.decodeUtf8' body)
                                    && not (toolOutcomeIsError out)
                                    && intOf (field "bytes" v) == fromIntegral (BS.length body)
                                    && isJust (field "shape" v)

    it "says one thing about one file, whatever row count is asked for" $
        withMaxSuccess 10 $
            property $
                forAllShow overSample sizeOf $ \(body, n) ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> peekBytes ["n" .= n] body
                        let verdicts = [o | o <- objectsIn v, hasKey o "verdict"]
                        pure $
                            counterexample (show (map keysOf verdicts)) $
                                length verdicts == 1

    it "still reads a grid as delimited when the file outruns the sample" $
        withMaxSuccess 10 $
            property $
                forAllShow overTable sizeOf $ \(body, n) ->
                    ioProperty $ do
                        v <- toolOutcomeValue <$> peekBytes ["n" .= n] body
                        let verdicts =
                                [field "verdict" o | o <- objectsIn v, hasKey o "verdict"]
                        pure $
                            counterexample (show (verdicts, field "view" v)) $
                                verdicts == [Just (String "delimited")]

    it
        "counts only where it says which bytes it counted"
        $ withMaxSuccess 10
        $ property
        $ forAllShow overSample sizeOf
        $ \(body, n) ->
            ioProperty $ do
                v <- toolOutcomeValue <$> peekBytes ["n" .= n] body
                let counting = [o | o <- objectsIn v, any (hasKey o) countKeys]
                pure $
                    counterexample (show (map keysOf counting)) $
                        not (null counting)
                            && all (`hasKey` "countsCover") counting
