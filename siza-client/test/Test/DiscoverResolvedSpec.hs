{-# LANGUAGE OverloadedStrings #-}

{- | The compiler-resolved-names ledger (R7-T1, search-api.md 3.3/11): a name
proven by clean check_type or a landed compile cancels lexical not_found
under EVERY mode/filter key until the world changes; inventory unregressed.
-}
module Test.DiscoverResolvedSpec (discoverResolvedSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.History (
    emptyLedger,
    ledgerRecord,
    ledgerResolve,
    ledgerWorldChanged,
 )
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Test.DiscoverFixtures (installNamesFile, runCatArgs, stateOf, textField)

-- | A minimal honest not_found envelope for @name@ under a mode/filter key.
missFor :: Text -> Value
missFor n =
    object
        [ "query" .= n
        , "state" .= ("not_found" :: Text)
        , "interpreted" .= object ["shape" .= ("name" :: Text)]
        , "next" .= ("No match for '" <> n <> "'." :: Text)
        , "hits" .= ([] :: [Value])
        , "total" .= (0 :: Int)
        ]

-- | Every ledger-key decoration a discover call can carry (modes, filters).
keyForms :: Text -> [Text]
keyForms n =
    [ n
    , n <> " [mode=inventory]"
    , n <> " [mode=construct]"
    , n <> " [module=Frame]"
    , n <> " [package=frameio] [mode=inventory]"
    ]

-- | The generated proof events: each proves @n@ by a different mechanism.
proofEvents :: [(Text, [Text])]
proofEvents =
    [ ("check_type expr", ["colList"])
    , ("qualified check_type expr", ["D.colGet", "colGet", "D"])
    , ("landed compile uses", ["totRev", "colSum", "frameGo"])
    ]

discoverResolvedSpec :: Spec
discoverResolvedSpec = describe "compiler-resolved-names ledger (R7-T1)" $ do
    it "a resolved name cancels not_found under every mode/filter key" $ do
        let denials =
                [ (names, key)
                | (_, names) <- proofEvents
                , let led = ledgerResolve names emptyLedger
                , n <- names
                , key <- keyForms n
                , let (_, out) = ledgerRecord key (missFor n) led
                , stateOf out == "not_found"
                ]
        denials `shouldBe` []

    it "the blocked denial names the compiler as the authority" $ do
        let led = ledgerResolve ["colList"] emptyLedger
            (_, out) = ledgerRecord "colList [mode=construct]" (missFor "colList") led
        stateOf out `shouldNotBe` "not_found"
        T.toLower (textField "summary" out)
            `shouldSatisfy` ("compiler" `T.isInfixOf`)

    it "denial becomes legal again after ledgerWorldChanged" $ do
        let led = ledgerWorldChanged (ledgerResolve ["colList"] emptyLedger)
            (_, out) = ledgerRecord "colList" (missFor "colList") led
        stateOf out `shouldBe` "not_found"

    it "an unproven name's honest miss passes through untouched" $ do
        let led = ledgerResolve ["colList"] emptyLedger
            (_, out) = ledgerRecord "granite" (missFor "granite") led
        stateOf out `shouldBe` "not_found"

    describe "HistoryGuard wiring (the dispatch seam)" $ do
        it "a clean check_type proves the expression's names against later denial" $ do
            ref <- newSearchLedger
            _ <-
                guardDiscover ref (cleanCheckType "D.colList :: Expr a -> [a]") $
                    ToolCall "check_type" (object ["expr" .= ("D.colList" :: Text)])
            out <- guardDiscover ref (constMiss "colList") (discoverCall "colList")
            fmap stateOfOutcome out `shouldBe` Right "duplicate"

        it "an erroring check_type proves nothing" $ do
            ref <- newSearchLedger
            _ <-
                guardDiscover ref (cleanCheckType "error: Not in scope: colNope") $
                    ToolCall "check_type" (object ["expr" .= ("colNope" :: Text)])
            out <- guardDiscover ref (constMiss "colNope") (discoverCall "colNope")
            fmap stateOfOutcome out `shouldBe` Right "not_found"

        it "a landed compile proves the cell's used names" $ do
            ref <- newSearchLedger
            _ <-
                guardDiscover ref landedInsert $
                    ToolCall
                        "insert_cell"
                        (object ["source" .= ("tot = colSum frame" :: Text)])
            out <- guardDiscover ref (constMiss "colSum") (discoverCall "colSum")
            fmap stateOfOutcome out `shouldBe` Right "duplicate"

        it "a red compile proves nothing" $ do
            ref <- newSearchLedger
            _ <-
                guardDiscover ref redInsert $
                    ToolCall
                        "insert_cell"
                        (object ["source" .= ("tot = colBad frame" :: Text)])
            out <- guardDiscover ref (constMiss "colBad") (discoverCall "colBad")
            fmap stateOfOutcome out `shouldBe` Right "not_found"

        it "a worldChanged wipe happens before, never after, the landed cell's proof" $ do
            -- A dep-declaring landed write both bumps the world AND proves its
            -- own names: the proof must survive the wipe it triggers.
            ref <- newSearchLedger
            _ <-
                guardDiscover ref landedInsert $
                    ToolCall
                        "insert_cell"
                        ( object
                            [ "source"
                                .= ("-- cabal: build-depends: frameio\ntot = colSum frame" :: Text)
                            ]
                        )
            out <- guardDiscover ref (constMiss "colSum") (discoverCall "colSum")
            fmap stateOfOutcome out `shouldBe` Right "duplicate"

    it "the blank-query inventory path is unregressed by the resolved ledger" $ do
        installNamesFile
        v <-
            runCatArgs
                ""
                ( object
                    [ "mode" .= ("inventory" :: Text)
                    , "module" .= ("Zephyr.Core" :: Text)
                    ]
                )
        stateOf v `shouldBe` "found"

-- | A dispatch answering one clean/red check_type payload.
cleanCheckType :: Text -> ToolCall -> IO (Either Text ToolOutcome)
cleanCheckType result _ =
    pure (Right (ToolOk (object ["expr" .= ("x" :: Text), "result" .= result])))

-- | A dispatch answering an honest lexical miss for @n@.
constMiss :: Text -> ToolCall -> IO (Either Text ToolOutcome)
constMiss n _ = pure (Right (ToolOk (missFor n)))

discoverCall :: Text -> ToolCall
discoverCall n = ToolCall "discover" (object ["query" .= n])

landedInsert :: ToolCall -> IO (Either Text ToolOutcome)
landedInsert _ =
    pure
        ( Right
            ( ToolOk
                ( object
                    [ "cellId" .= (3 :: Int)
                    , "execution" .= object ["ok" .= True]
                    ]
                )
            )
        )

redInsert :: ToolCall -> IO (Either Text ToolOutcome)
redInsert _ =
    pure
        ( Right
            ( ToolOk
                ( object
                    [ "cellId" .= (3 :: Int)
                    , "execution" .= object ["ok" .= False, "error" .= ("boom" :: Text)]
                    ]
                )
            )
        )

stateOfOutcome :: ToolOutcome -> Text
stateOfOutcome (ToolOk v) = stateOf v
stateOfOutcome _ = "err"
