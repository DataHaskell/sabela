{-# LANGUAGE OverloadedStrings #-}

{- | The optional @goal@ on a write: advertised in the schema and stripped
before the wire. It may pick the medium a rendering uses; whether the display
contract fires at all is not its to decide (C1-15c).
-}
module Test.GoalPlumbingSpec (goalPlumbingSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.GrammarCards (GrammarMode (..))
import Siza.Agent.Stack (newStackSession, stackDispatch)
import Siza.Agent.Stack.Call (runToolCall)
import Siza.Agent.Tools (catalogueWith)
import Test.StackFixtures (Fake, argText)

goalPlumbingSpec :: Spec
goalPlumbingSpec = describe "the optional goal on a write" $ do
    it "is advertised on both write tools, and required by neither" $
        mapM_ advertisesGoal ["insert_cell", "replace_cell_source"]

    it "is not advertised on tools that do not write" $
        mapM_ lacksGoal ["list_cells", "read_cell", "execute_cell"]

    it "reaches the display contract, which asks the kernel about a rendering" $ do
        tape <- writeWith (Just "show a chart of the residuals")
        map tcName tape `shouldSatisfy` elem "try"

    it "reaches it just the same when no goal was ever given" $ do
        withGoal <- map tcName <$> writeWith (Just "show a chart of the residuals")
        without <- map tcName <$> writeWith Nothing
        without `shouldBe` withGoal

    it "never reaches the wire" $ do
        tape <- writeWith (Just "show a chart")
        map tcArgs tape `shouldSatisfy` not . any (hasProp "goal")

writeWith :: Maybe Text -> IO [ToolCall]
writeWith mGoal = do
    (fake, tape) <- displayNotebook
    ss <- newStackSession GrammarOn ""
    _ <- runToolCall ss (stackDispatch ss fake) (writeCall mGoal)
    readIORef tape

writeCall :: Maybe Text -> ToolCall
writeCall mGoal =
    ToolCall "insert_cell" (object (("source" .= src) : goalArg))
  where
    src = "pic = mkChart values" :: Text
    goalArg = ["goal" .= g | Just g <- [mGoal]]

{- | A write that commits clean but renders nothing, which is the situation
the display contract exists for.
-}
displayNotebook :: IO (Fake, IORef [ToolCall])
displayNotebook = do
    tape <- newIORef []
    let okExec = object ["ok" .= True]
        disp call@(ToolCall name argv) = do
            modifyIORef' tape (<> [call])
            case name of
                "insert_cell" ->
                    ok (object ["cellId" .= (1 :: Int), "execution" .= okExec])
                "replace_cell_source" ->
                    ok
                        ( object
                            [ "cellId" .= (1 :: Int)
                            , "execution" .= okExec
                            , "echo" .= argText "new_source" argv
                            ]
                        )
                "check_type" -> ok (object ["result" .= ("pic :: Svg" :: Text)])
                "list_cells" -> ok (object ["cells" .= ([] :: [Value])])
                _ -> ok (object ["result" .= ("" :: Text)])
    pure (disp, tape)

ok :: Value -> IO (Either Text ToolOutcome)
ok = pure . Right . ToolOk

advertisesGoal :: Text -> Expectation
advertisesGoal name = do
    schemaOf name `shouldSatisfy` maybe False (hasProp "goal" . propsOf)
    requiredOf name `shouldSatisfy` notElem "goal"

lacksGoal :: Text -> Expectation
lacksGoal name =
    schemaOf name `shouldSatisfy` maybe False (not . hasProp "goal" . propsOf)

schemaOf :: Text -> Maybe Value
schemaOf name = case filter isNamed functions of
    (f : _) -> KM.lookup "parameters" f
    [] -> Nothing
  where
    functions =
        [ f | Object o <- catalogueWith False, Just (Object f) <- [KM.lookup "function" o]
        ]
    isNamed f = KM.lookup "name" f == Just (String name)

propsOf :: Value -> Value
propsOf (Object o) = fromMaybe (object []) (KM.lookup "properties" o)
propsOf _ = object []

requiredOf :: Text -> [Text]
requiredOf name = case schemaOf name of
    Just (Object p) -> case KM.lookup "required" p of
        Just (Array a) -> [t | String t <- foldr (:) [] a]
        _ -> []
    _ -> []

hasProp :: Text -> Value -> Bool
hasProp k (Object o) = KM.member (K.fromText k) o
hasProp _ _ = False
