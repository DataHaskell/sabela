{-# LANGUAGE OverloadedStrings #-}

{- | The discover-envelope payloads the emit-ledger protection specs run over:
one shaped example per answer class, big enough to cross the elision floor.
-}
module Test.EmitLedgerFixtures (
    classes,
    encodeT,
    esc,
    occursIn,
    runSeq,
    foundE,
    hitJ,
    longAutofix,
    longDiagnostic,
    longSig,
 ) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE

import Siza.Agent.EmitLedger (dedupText, emptyEmitLedger)

import Test.DiscoverFixtures (hitsOf)
import Data.Text (Text)
import qualified Data.Text as T

longDiagnostic :: Text
longDiagnostic =
    T.intercalate
        "\n"
        ( "<interactive>:238:1: error: [GHC-88464]"
            : replicate 6 "    Variable not in scope: sineWaveSvg :: String"
        )

longAutofix :: Text
longAutofix =
    "Declared build-depends: text for this trial (the module was in a hidden \
    \package). Commit this CURRENT source, which carries the dependency line:\n"
        <> T.intercalate "\n" (replicate 5 "import qualified Data.Text as T")

longSig :: Text
longSig =
    T.intercalate " -> " (replicate 8 "Maybe (Either Text Double)")
        <> " -> [(Text, Double)] -> Plot"

hitJ :: Text -> Text -> Text -> Value
hitJ name kind ty =
    object
        [ "name" .= name
        , "module" .= ("Cumulus.Plot" :: Text)
        , "package" .= ("cumulus" :: Text)
        , "version" .= ("0.3.1" :: Text)
        , "install" .= ("hidden" :: Text)
        , "matchKind" .= kind
        , "origin" .= ("hoogle" :: Text)
        , "type" .= ty
        , "cabal" .= ("-- cabal: build-depends: cumulus" :: Text)
        ]

foundE :: Text -> Text -> Value
foundE kind q =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "hits" .= [hitJ "bars" kind longSig]
        , "shown" .= (1 :: Int)
        , "omitted" .= (0 :: Int)
        , "total" .= (1 :: Int)
        ]

missE :: Text -> Value
missE q =
    object
        [ "query" .= q
        , "state" .= ("not_found" :: Text)
        , "next" .= next
        ]
  where
    next =
        "No match held anywhere consulted. "
            <> T.unwords (replicate 8 "Narrow with module= or package= or act on held facts.")

cardE :: Text -> Value
cardE q =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "card"
            .= object
                [ "module" .= ("Cumulus.Plot" :: Text)
                , "status" .= ("installed-not-loaded" :: Text)
                , "exports" .= ["bars :: " <> longSig, "cols :: " <> longSig]
                ]
        ]

constructE :: Text -> Value
constructE q =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "hits"
            .= [ Object
                    ( km (hitJ "defaultPlot" "type" longSig)
                        <> KM.fromList
                            [
                                ( "use"
                                , String
                                    ( "produces Plot, the argument bars needs. "
                                        <> T.unwords (replicate 6 "Apply it before rendering the chart output.")
                                    )
                                )
                            ]
                    )
               ]
        ]
  where
    km (Object o) = o
    km _ = KM.empty

dupE :: Text -> Value
dupE q =
    object
        [ "query" .= q
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ("call 3" :: Text)
        , "summary"
            .= T.unwords
                (replicate 10 "same ranked answer; your query change did not change it.")
        ]

classes :: [(String, Text -> Value)]
classes =
    [ ("found-exact", foundE "exact")
    , ("found-weak", foundE "substring")
    , ("miss", missE)
    , ("card", cardE)
    , ("construct", constructE)
    , ("duplicate", dupE)
    ]

encodeT :: Value -> Text
encodeT = TE.decodeUtf8 . LBS.toStrict . encode

-- | JSON-escaped spelling of a value, as it appears inside a rendered block.
esc :: Text -> Text
esc t = T.dropEnd 1 (T.drop 1 (TE.decodeUtf8 (LBS.toStrict (encode t))))

occursIn :: Text -> Text -> Bool
occursIn v emission = v `T.isInfixOf` emission || esc v `T.isInfixOf` emission

-- | One emission per turn, threading the ledger the way the loop does.
runSeq :: [Text] -> [Text]
runSeq = go 1 emptyEmitLedger
  where
    go _ _ [] = []
    go turn led (c : cs) =
        let (c', led') = dedupText turn c led
         in c' : go (turn + 1) led' cs
