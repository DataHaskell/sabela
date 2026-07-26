module Test.Fixtures (
    foundCaps,
    unsigned,
    stubQuery,
    sampleEvent,
    retroSession,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Sabela.AI.Capabilities.ToolName (
    ToolName (ExecuteCell, ReplaceCellSource),
 )
import Sabela.AI.KernelState (Activity (Idle), KernelState (Alive))
import Sabela.AI.Types (ToolOutcome (ToolErr, ToolOk))
import Siza.Annotate (unsignedTopLevelBinds)
import qualified Siza.Lang.Haskell as Hs
import Siza.Language (Diagnostic (..), Severity (Warning))
import Siza.Provenance (
    Actor (Agent),
    Preflight (..),
    SessionEvent (..),
 )
import Siza.Security (Capability (..), scanFindings)

foundCaps :: Text -> [Capability]
foundCaps src = case Hs.parseModuleE src of
    Left ds -> error ("parse failed: " <> show ds)
    Right m -> map (\(c, _, _) -> c) (scanFindings m)

unsigned :: Text -> [Text]
unsigned src = case Hs.parseModuleE src of
    Left ds -> error ("parse failed: " <> show ds)
    Right m -> unsignedTopLevelBinds m

stubQuery :: [(Text, Text)] -> Text -> IO (Either Text Text)
stubQuery table name =
    pure (maybe (Left "no type inferred (cold session)") Right (lookup name table))

sampleEvent :: SessionEvent
sampleEvent =
    SessionEvent
        { seAt = UTCTime (fromGregorian 2026 6 20) (secondsToDiffTime 3600)
        , seSession = "siza-host-42"
        , seNotebook = "demo"
        , seActor = Agent
        , seCall = ReplaceCellSource
        , seInput = object ["cell_id" .= (3 :: Int), "source" .= ("x = 1" :: Text)]
        , sePreflight =
            Just (Preflight{pfParsed = True, pfFindings = [], pfVetted = True})
        , seOutcome = ToolErr (object ["error" .= ("boom" :: Text)])
        , seKernelBefore = Alive 2 Idle False
        , seGen = 7
        , sePrev = Nothing
        }

retroSession :: [SessionEvent]
retroSession =
    [ sampleEvent
    , sampleEvent
        { seCall = ExecuteCell
        , seOutcome = ToolOk (object [])
        , sePreflight = Nothing
        }
    , sampleEvent
        { seCall = ExecuteCell
        , seOutcome = ToolOk (object [])
        , sePreflight =
            Just
                ( Preflight
                    { pfParsed = True
                    , pfFindings =
                        [Diagnostic Warning Nothing Nothing "denied capability: x"]
                    , pfVetted = True
                    }
                )
        }
    , sampleEvent
        { seCall = ReplaceCellSource
        , seOutcome = ToolErr (object ["error" .= ("blocked" :: Text)])
        , sePreflight =
            Just (Preflight{pfParsed = True, pfFindings = [], pfVetted = False})
        }
    ]
