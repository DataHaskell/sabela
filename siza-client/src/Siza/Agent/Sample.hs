module Siza.Agent.Sample (
    SampleVerify (..),
    SampleResult (..),
    sampleVerifyOne,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Text (Text)

data SampleVerify = SampleVerify
    { svSample :: Int -> IO Text
    , svRollout :: Text -> IO Bool
    , svInsert :: Text -> IO ()
    }

data SampleResult = SampleResult
    { srAccepted :: Maybe Text
    , srVerified :: [Text]
    , srProbed :: [Text]
    }
    deriving (Eq, Show)

sampleVerifyOne :: Int -> SampleVerify -> IO SampleResult
sampleVerifyOne k sv = do
    cands <- sampleConcurrent k (svSample sv)
    verifySerial sv cands []

sampleConcurrent :: Int -> (Int -> IO Text) -> IO [Text]
sampleConcurrent k sample = do
    boxes <- mapM (const newEmptyMVar) [0 .. k - 1]
    mapM_ (\(i, box) -> forkIO (sample i >>= putMVar box)) (zip [0 ..] boxes)
    mapM takeMVar boxes

verifySerial :: SampleVerify -> [Text] -> [Text] -> IO SampleResult
verifySerial _ [] verified =
    pure (SampleResult Nothing (reverse verified) (reverse verified))
verifySerial sv (c : rest) verified = do
    passed <- svRollout sv c
    let verified' = c : verified
    if passed
        then do
            svInsert sv c
            pure (SampleResult (Just c) (reverse verified') (reverse verified))
        else verifySerial sv rest verified'
