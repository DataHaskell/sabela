{-# LANGUAGE OverloadedStrings #-}

{- | A tools/call driven end to end through the same stack the chat loop uses:
distilled results, the routed retry, and the pre-flight gate.
-}
module Test.McpCallSpec (mcpCallSpec) where

import Data.Aeson (Value (..), decodeStrict, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Lang.Haskell (parseModuleE)
import Siza.Mcp (McpEnv, mcpEnvOver, mcpSession, toolsCall)
import Test.StackFixtures (Fake, argText)
import Test.TruthGen (genIdent, genModuleName)

mcpCallSpec :: Spec
mcpCallSpec = describe "an MCP tools/call on the shared stack" $ do
    it "sheds a huge output instead of shipping it whole" $ do
        env <- liveEnv hugeOutputNotebook
        r <- toolsCall env (callParams "execute_cell" (object ["cell_id" .= (1 :: Int)]))
        let txt = contentText 0 r
        T.length txt `shouldSatisfy` (< 6000)
        txt `shouldSatisfy` T.isInfixOf "outputCount"

    it "routes an insert blocked by a red cell, and says so in a note" $ do
        env <- liveEnv blockedInsertNotebook
        r <-
            toolsCall env $
                callParams "insert_cell" (object ["source" .= ("x = 1" :: Text)])
        contentText 1 r `shouldSatisfy` T.isInfixOf "routed retry"

    it "keeps parseable JSON first, so a client reading one block is fine" $ do
        env <- liveEnv blockedInsertNotebook
        r <-
            toolsCall env $
                callParams "insert_cell" (object ["source" .= ("x = 1" :: Text)])
        decodeStrict (TE.encodeUtf8 (contentText 0 r))
            `shouldSatisfy` (maybe False isObject :: Maybe Value -> Bool)

    it "refuses source that does not parse, before it reaches the wire" $ do
        env <- liveEnv blockedInsertNotebook
        r <-
            toolsCall env $
                callParams "insert_cell" (object ["source" .= ("x = " :: Text)])
        isErrorResult r `shouldBe` True

    it "gives a refused write the fields a server refusal carries (C3-2)" $
        property $
            forAll genUnparseable $ \src -> ioProperty $ do
                env <- liveEnv blockedInsertNotebook
                r <-
                    toolsCall env $
                        callParams "insert_cell" (object ["source" .= src])
                pure $ case decodeStrict (TE.encodeUtf8 (contentText 0 r)) of
                    Just (Object o) ->
                        conjoin
                            ( counterexample
                                "source is not echoed as submitted"
                                (KM.lookup (K.fromText "source") o == Just (String src))
                                : [ counterexample (show k) (KM.member (K.fromText k) o)
                                  | k <- refusalKeys
                                  ]
                            )
                    _ ->
                        counterexample
                            (T.unpack (contentText 0 r))
                            (property False)

    it "repairs a red cell it wrote itself" $ do
        (fake, tape) <- redNotebook
        env <- liveEnv fake
        _ <-
            toolsCall env $
                callParams "insert_cell" (object ["source" .= ("red = boom" :: Text)])
        seen <- readIORef tape
        map tcName seen `shouldSatisfy` elem "read_cell"

    it "never touches a red cell it did not write" $ do
        (fake, tape) <- redNotebook
        env <- liveEnv fake
        _ <-
            toolsCall env $
                callParams "insert_cell" (object ["source" .= ("red = boom" :: Text)])
        seen <- readIORef tape
        map cellArg seen `shouldSatisfy` notElem (Just strangersCell)

    it "emits no note block when nothing was done behind the caller's back" $ do
        env <- liveEnv hugeOutputNotebook
        r <- toolsCall env (callParams "execute_cell" (object ["cell_id" .= (1 :: Int)]))
        blockCount r `shouldBe` 1

-- | An env built exactly the way runMcp builds one, repair and all.
liveEnv :: Fake -> IO McpEnv
liveEnv fake = do
    ss <- mcpSession
    pure (mcpEnvOver ss fake [])

callParams :: Text -> Value -> Value
callParams name args = object ["name" .= name, "arguments" .= args]

hugeOutputNotebook :: Fake
hugeOutputNotebook (ToolCall name _)
    | name == "execute_cell" =
        ok
            ( object
                [ "cellId" .= (1 :: Int)
                , "execution"
                    .= object
                        [ "ok" .= True
                        , "outputs"
                            .= [ object
                                    [ "oiMime" .= ("text/html" :: Text)
                                    , "oiOutput" .= T.replicate 40000 "<svg/>"
                                    ]
                               ]
                        ]
                ]
            )
hugeOutputNotebook _ = ok (object ["result" .= ("" :: Text)])

blockedInsertNotebook :: Fake
blockedInsertNotebook (ToolCall name argv) = case name of
    "insert_cell" ->
        pure
            ( Right
                ( ToolErr
                    ( object
                        [ "notCommitted" .= ("pending-error" :: Text)
                        , "cellId" .= blocker
                        ]
                    )
                )
            )
    "read_cell" ->
        ok
            ( object
                [ "id" .= blocker
                , "source" .= ("rendered = boom" :: Text)
                , "error" .= ("Variable not in scope: boom" :: Text)
                ]
            )
    "find_by_type" -> ok (object ["fits" .= ([] :: [Value]), "shown" .= (0 :: Int)])
    "replace_cell_source" ->
        ok
            ( object
                [ "cellId" .= blocker
                , "execution" .= object ["ok" .= True]
                , "echo" .= argText "new_source" argv
                ]
            )
    "list_cells" -> ok (object ["cells" .= ([] :: [Value])])
    _ -> ok (object ["result" .= ("" :: Text)])
  where
    blocker = 7 :: Int

{- | One cell the session writes red, and one red cell it never wrote. The
second must come back untouched.
-}
redNotebook :: IO (Fake, IORef [ToolCall])
redNotebook = do
    tape <- newIORef []
    let disp call@(ToolCall name argv) = do
            modifyIORef' tape (<> [call])
            case name of
                "insert_cell" ->
                    ok
                        ( object
                            [ "cellId" .= mineCell
                            , "execution"
                                .= object ["ok" .= False, "error" .= dym]
                            ]
                        )
                "read_cell" ->
                    ok
                        ( object
                            [ "id" .= argInt "cell_id" argv
                            , "source" .= ("red = boom" :: Text)
                            , "error" .= dym
                            ]
                        )
                "list_cells" ->
                    ok
                        ( object
                            [ "cells"
                                .= [ redCell mineCell
                                   , redCell strangersCell
                                   ]
                            ]
                        )
                "replace_cell_source" ->
                    ok (object ["execution" .= object ["ok" .= True]])
                _ -> ok (object ["result" .= ("" :: Text)])
    pure (disp, tape)
  where
    dym =
        "Variable not in scope: boom :: Int\n\
        \  Perhaps use `boo' (imported from Zephyr.Core)" ::
            Text
    redCell cid =
        object
            [ "id" .= cid
            , "source" .= ("red = boom" :: Text)
            , "defines" .= (["red"] :: [Text])
            , "hasError" .= True
            ]

mineCell :: Int
mineCell = 1

strangersCell :: Int
strangersCell = 999

cellArg :: ToolCall -> Maybe Int
cellArg (ToolCall _ argv) = argInt "cell_id" argv

argInt :: Text -> Value -> Maybe Int
argInt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (Number n) -> Just (round n)
    _ -> Nothing
argInt _ _ = Nothing

ok :: Value -> IO (Either Text ToolOutcome)
ok = pure . Right . ToolOk

contentText :: Int -> Value -> Text
contentText i v = case blocks v of
    bs | length bs > i -> textOf (bs !! i)
    _ -> ""
  where
    textOf (Object b) = case KM.lookup (K.fromText "text") b of
        Just (String s) -> s
        _ -> ""
    textOf _ = ""

blockCount :: Value -> Int
blockCount = length . blocks

blocks :: Value -> [Value]
blocks (Object o) = case KM.lookup (K.fromText "content") o of
    Just (Array a) -> foldr (:) [] a
    _ -> []
blocks _ = []

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

isErrorResult :: Value -> Bool
isErrorResult (Object o) = KM.lookup (K.fromText "isError") o == Just (Bool True)
isErrorResult _ = False

{- | What every refusal says, wherever it was decided: the class, that nothing
was committed, why, and the source it was decided about.
-}
refusalKeys :: [Text]
refusalKeys = ["verdict", "notCommitted", "diagnostic", "source"]

{- | Sources no Haskell parser accepts, mutated from well-formed programs over
arbitrary identifiers and modules.
-}
genUnparseable :: Gen Text
genUnparseable = (`suchThat` (isLeft . parseModuleE)) $ do
    n <- genIdent
    m <- genModuleName
    elements
        [ n <> " = "
        , n <> " = ("
        , "data = 1"
        , n <> " = do"
        , "import " <> m <> "\n" <> n <> " = [1, 2"
        , n <> " :: Int\n" <> n <> " = case of"
        ]
