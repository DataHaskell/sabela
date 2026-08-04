{-# LANGUAGE OverloadedStrings #-}

{- | The pieces the search-ladder properties share: a found answer, a miss, and
the dispatch step both surfaces take when a question repeats.
-}
module Test.LadderFixtures (
    encodeT,
    envT,
    exactHit,
    foundOver,
    hkT,
    missOver,
    projected,
    runLadder,
    scopedKey,
) where

import Data.Aeson (Value (..), encode, object)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Siza.Agent.Discover.Dedup (ledgerShortcutStep)
import Siza.Agent.Discover.Envelope (requiredHitKeys)
import Siza.Agent.Discover.History (SearchLedger, ledgerRecord)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Request (requestKey)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    MatchKind (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (hitText)

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True [] []

foundOver :: Text -> [DHit] -> Value
foundOver q hs =
    discoverEnvelope envT (interpret envT q) 10 [okAnswer "session" hs] hkT

missOver :: Text -> Value
missOver q =
    discoverEnvelope
        envT
        (interpret envT q)
        10
        [okAnswer "session" [], okAnswer "hoogle" []]
        hkT

exactHit :: Text -> Text -> Text -> DHit
exactHit n m p =
    (mkHit n m p)
        { dhKind = MkExact
        , dhType = "Int -> Int"
        , dhVersion = "1.0"
        , dhInstall = InstInstalled
        , dhOrigin = "session"
        }

encodeT :: Value -> Text
encodeT = TE.decodeUtf8 . LBS.toStrict . encode

projected :: Value -> [(Text, Text)]
projected h = [(k, hitText k h) | k <- requiredHitKeys ++ ["module", "type"]]

-- | The request key a module- and package-scoped call is filed under.
scopedKey :: Text -> Text -> Text -> Text
scopedKey m p q =
    requestKey
        q
        ( object
            [ (K.fromText "module", String m)
            , (K.fromText "package", String p)
            ]
        )

{- | The path both surfaces take: 'ledgerShortcutStep' owns the hard stop, and
only it reaches the terminal rung.
-}
runLadder :: SearchLedger -> [(Text, Value)] -> (SearchLedger, [Value])
runLadder led0 = foldl step (led0, [])
  where
    step (led, outs) (q, v) = case ledgerShortcutStep led q of
        (led1, Just out) -> (led1, outs ++ [out])
        (led1, Nothing) ->
            let (led2, out) = ledgerRecord q v led1
             in (led2, outs ++ [out])
