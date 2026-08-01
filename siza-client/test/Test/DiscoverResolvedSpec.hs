{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverResolvedSpec (discoverResolvedSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.QualifiedName (
    AliasEnv (..),
    QualifiedName (..),
    resolveName,
 )
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.History (
    emptyLedger,
    ledgerRecord,
    ledgerResolve,
    ledgerWorldChanged,
 )
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Types (Interpreted (..), NotebookEnv (..))
import Test.DiscoverFixtures (installNamesFile, runCatArgs, stateOf, textField)

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

keyForms :: Text -> [Text]
keyForms n =
    [ n
    , n <> " [mode=inventory]"
    , n <> " [mode=construct]"
    , n <> " [module=Frame]"
    , n <> " [package=frameio] [mode=inventory]"
    ]

-- | What the index says about a name that exists: clean-looking, compiler-free.
indexAnswer :: Text
indexAnswer = "colList :: Expr a -> [a]\nPackage frameio is declared by this notebook."

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

        it "an index-answered check_type proves nothing about this session" $ do
            ref <- newSearchLedger
            _ <-
                guardDiscover ref (checkTypeVia "local-index" indexAnswer) $
                    ToolCall "check_type" (object ["expr" .= ("colList" :: Text)])
            out <- guardDiscover ref (constMiss "colList") (discoverCall "colList")
            fmap stateOfOutcome out `shouldBe` Right "not_found"

        it "an unlabelled check_type payload proves nothing" $ do
            ref <- newSearchLedger
            _ <-
                guardDiscover
                    ref
                    (\_ -> pure (Right (ToolOk (object ["result" .= indexAnswer]))))
                    (ToolCall "check_type" (object ["expr" .= ("colList" :: Text)]))
            out <- guardDiscover ref (constMiss "colList") (discoverCall "colList")
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

    describe "the client and the server resolve a qualifier identically" $ do
        it "name, module and disclosure agree for an aliased qualifier" $
            property $ \(Alias a) (Modul m) (Ident n) ->
                a /= m ==> agrees (envOf [(a, m)] [m]) (a <> "." <> n)

        it "they agree for a qualifier the notebook never imported" $
            property $ \(Alias a) (Ident n) ->
                agrees (envOf [] []) (a <> "." <> n)

        it "they agree for a module imported under its own name" $
            property $ \(Modul m) (Ident n) ->
                agrees (envOf [] [m]) (m <> "." <> n)

        it "they agree on a qualified type name, which neither side splits" $
            property $ \(Alias a) (Modul m) (TypeIdent n) ->
                a /= m ==> agrees (envOf [(a, m)] [m]) (a <> "." <> n)

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

-- | A check_type payload as the tool emits one: the source travels with it.
checkTypeVia :: Text -> Text -> ToolCall -> IO (Either Text ToolOutcome)
checkTypeVia via result _ =
    pure
        ( Right
            ( ToolOk
                ( object
                    [ "expr" .= ("x" :: Text)
                    , "via" .= via
                    , "result" .= result
                    ]
                )
            )
        )

cleanCheckType :: Text -> ToolCall -> IO (Either Text ToolOutcome)
cleanCheckType = checkTypeVia "session-type"

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

newtype Ident = Ident Text
    deriving (Show)

instance Arbitrary Ident where
    arbitrary = Ident <$> genLower

newtype Modul = Modul Text
    deriving (Show)

instance Arbitrary Modul where
    arbitrary = Modul . T.intercalate "." <$> resize 3 (listOf1 genUpper)

newtype Alias = Alias Text
    deriving (Show)

instance Arbitrary Alias where
    arbitrary = Alias <$> genUpper

-- | An upper-headed name: the shape a module path is ambiguous with.
newtype TypeIdent = TypeIdent Text
    deriving (Show)

instance Arbitrary TypeIdent where
    arbitrary = TypeIdent <$> genUpper

-- | A name a Haskell parser would accept, so no reserved word is generated.
genLower :: Gen Text
genLower = genWord `suchThat` (`notElem` reserved)
  where
    genWord = do
        c <- elements ['a' .. 'z']
        rest <- resize 6 (listOf (elements (['a' .. 'z'] <> ['0' .. '9'])))
        pure (T.pack (c : rest))
    reserved =
        T.words
            "case class data default deriving do else foreign if import in \
            \infix infixl infixr instance let module newtype of then type where"

genUpper :: Gen Text
genUpper = do
    c <- elements ['A' .. 'Z']
    rest <- resize 5 (listOf (elements (['a' .. 'z'] <> ['0' .. '9'])))
    pure (T.pack (c : rest))

envOf :: [(Text, Text)] -> [Text] -> NotebookEnv
envOf aliases imports = NotebookEnv aliases imports [] [] [] []

-- | The client's interpreter and the shared seam must read a qualifier alike.
agrees :: NotebookEnv -> Text -> Property
agrees env t =
    (iName i, iScope i, iNote i)
        === (qnBare qn, qnModule qn, qnNote qn)
  where
    i = interpret env t
    qn = resolveName (AliasEnv (neAliases env) (neImports env)) t
