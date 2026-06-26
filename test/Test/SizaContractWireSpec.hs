{-# LANGUAGE OverloadedStrings #-}

{- | Golden contract-pinning for the siza/AI wire (Part 3, Track B of
@docs/siza-redesign.md@), now that the cutover has landed the sum-typed
contract as the SOLE wire.

What is pinned here:

* the set of @chatTools@ names, and each tool's required input fields and
  enum values;
* the typed @kernel_status@ result field set (@state@/@ksGen@/@ebGeneration@)
  with the legacy four-field blob asserted gone;
* the tool-result envelope keys (@isError@ / @result@) and the
  ok-derivation (@ok@ holds iff the outcome is @Succeeded@), with the
  execution summary pinned to the typed @CellResult@ keys only.

The deeper typed shapes (@KernelState@ / @CellResult@ tags, @Admission@) are
pinned in their own specs (@KernelStateWireSpec@, @CellResultWireSpec@,
@ToolOutcomeWireSpec@). The @await_idle@ tool is part of the asserted
tool-name set below.
-}
module Test.SizaContractWireSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.AI.CellResult (okCellResult, toCellResult)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Anthropic.Types (ToolDef (..))
import Sabela.Model (CellError (..))
import Test.Hspec

-- | Look up a tool's schema by its wire name.
schemaOf :: Text -> Maybe Value
schemaOf nm =
    tdInputSchema <$> lookupBy ((== nm) . tdName) chatTools

lookupBy :: (a -> Bool) -> [a] -> Maybe a
lookupBy p = foldr (\x r -> if p x then Just x else r) Nothing

-- | The string members of an object's named array property (e.g. @required@).
stringsAt :: Text -> Value -> [Text]
stringsAt k (Object o) = case KM.lookup (Key.fromText k) o of
    Just (Array xs) -> mapMaybe asString (toList xs)
    _ -> []
stringsAt _ _ = []

-- | The @enum@ string members of a property nested under @properties@.
enumOf :: Text -> Value -> [Text]
enumOf prop (Object o) =
    case KM.lookup "properties" o of
        Just (Object props) -> case KM.lookup (Key.fromText prop) props of
            Just (Object p) -> case KM.lookup "enum" p of
                Just (Array xs) -> mapMaybe asString (toList xs)
                _ -> []
            _ -> []
        _ -> []
enumOf _ _ = []

-- | The @type@ of a property nested under @properties@.
typeOf :: Text -> Value -> Maybe Text
typeOf prop (Object o) = do
    Object props <- KM.lookup "properties" o
    Object p <- KM.lookup (Key.fromText prop) props
    KM.lookup "type" p >>= asString
typeOf _ _ = Nothing

asString :: Value -> Maybe Text
asString (String s) = Just s
asString _ = Nothing

-- | Every wire name currently in the catalogue.
toolNames :: [Text]
toolNames = map tdName chatTools

{- | The tool-result envelope, reconstructed exactly as
@Sabela.Server.Ai.aiToolH@ builds it from a @ToolOutcome@. Pinning the
@isError@/@result@ keys here catches a rename to the envelope encoding.
-}
envelope :: Bool -> Value -> Value
envelope isErr v = object ["isError" .= isErr, "result" .= v]

objectKeys :: Value -> [Text]
objectKeys (Object o) = map Key.toText (KM.keys o)
objectKeys _ = []

spec :: Spec
spec = describe "siza/AI wire contract (sum-typed, the legacy blob is gone)" $ do
    describe "chatTools: the tool-name set" $ do
        it "pins the exact set of tool wire names" $
            sort toolNames
                `shouldBe` sort
                    [ "list_cells"
                    , "read_cell"
                    , "read_cell_output"
                    , "find_cells_by_content"
                    , "propose_edit"
                    , "replace_cell_source"
                    , "insert_cell"
                    , "delete_cell"
                    , "execute_cell"
                    , "scratchpad"
                    , "list_bindings"
                    , "check_type"
                    , "find_by_type"
                    , "describe_function"
                    , "peek_data"
                    , "api_reference"
                    , "explore_result"
                    , "kernel_status"
                    , "interrupt"
                    , "kernel_restart"
                    , "await_idle"
                    , "export_notebook"
                    , "find_package"
                    , "find_example_cell"
                    , "find_function"
                    ]
        it "has no duplicate tool names" $
            length (nub toolNames) `shouldBe` length toolNames
        it "every tool carries a non-empty description" $
            all ((/= "") . tdDescription) chatTools `shouldBe` True

    describe "chatTools: required input fields per tool" $ do
        let requiredOf nm = maybe [] (sort . stringsAt "required") (schemaOf nm)
        it "read_cell requires cell_id" $
            requiredOf "read_cell" `shouldBe` ["cell_id"]
        it "read_cell_output requires cell_id" $
            requiredOf "read_cell_output" `shouldBe` ["cell_id"]
        it "find_cells_by_content requires pattern" $
            requiredOf "find_cells_by_content" `shouldBe` ["pattern"]
        it "replace_cell_source requires cell_id and new_source" $
            requiredOf "replace_cell_source" `shouldBe` ["cell_id", "new_source"]
        it "propose_edit requires cell_id and new_source" $
            requiredOf "propose_edit" `shouldBe` ["cell_id", "new_source"]
        it "insert_cell requires only source (append-only; no after_cell_id)" $ do
            requiredOf "insert_cell" `shouldBe` ["source"]
            (typeOf "after_cell_id" =<< schemaOf "insert_cell") `shouldBe` Nothing
        it "delete_cell requires cell_id" $
            requiredOf "delete_cell" `shouldBe` ["cell_id"]
        it "execute_cell requires cell_id" $
            requiredOf "execute_cell" `shouldBe` ["cell_id"]
        it "scratchpad requires code" $
            requiredOf "scratchpad" `shouldBe` ["code"]
        it "check_type requires expr" $
            requiredOf "check_type" `shouldBe` ["expr"]
        it "find_by_type requires goal" $
            requiredOf "find_by_type" `shouldBe` ["goal"]
        it "describe_function requires name" $
            requiredOf "describe_function" `shouldBe` ["name"]
        it "list_bindings requires nothing" $
            requiredOf "list_bindings" `shouldBe` []
        it "the find_* discovery tools require query" $
            map requiredOf ["find_package", "find_example_cell", "find_function"]
                `shouldBe` [["query"], ["query"], ["query"]]
        it "explore_result requires handle_id and op" $
            requiredOf "explore_result" `shouldBe` ["handle_id", "op"]
        it "peek_data requires path" $
            requiredOf "peek_data" `shouldBe` ["path"]
        it "api_reference requires nothing" $
            requiredOf "api_reference" `shouldBe` []
        it "the no-arg kernel tools require nothing" $
            map
                requiredOf
                [ "kernel_status"
                , "interrupt"
                , "kernel_restart"
                , "await_idle"
                , "export_notebook"
                ]
                `shouldBe` [[], [], [], [], []]

    describe "chatTools: enum value sets" $ do
        let enumIn tool prop = maybe [] (sort . enumOf prop) (schemaOf tool)
        it "insert_cell cell_type enum is CodeCell/ProseCell" $
            enumIn "insert_cell" "cell_type" `shouldBe` ["CodeCell", "ProseCell"]
        it "insert_cell language enum is Haskell/Python" $
            enumIn "insert_cell" "language" `shouldBe` ["Haskell", "Python"]
        it "scratchpad language enum is Haskell/Python" $
            enumIn "scratchpad" "language" `shouldBe` ["Haskell", "Python"]
        it "explore_result op enum is head/tail/slice/grep" $
            enumIn "explore_result" "op"
                `shouldBe` ["grep", "head", "slice", "tail"]

    describe "chatTools: scalar field types" $ do
        it "cell_id fields are integer-typed" $ do
            (typeOf "cell_id" =<< schemaOf "read_cell") `shouldBe` Just "integer"
            (typeOf "cell_id" =<< schemaOf "execute_cell")
                `shouldBe` Just "integer"
        it "new_source / pattern / code are string-typed" $ do
            (typeOf "new_source" =<< schemaOf "replace_cell_source")
                `shouldBe` Just "string"
            (typeOf "pattern" =<< schemaOf "find_cells_by_content")
                `shouldBe` Just "string"
            (typeOf "code" =<< schemaOf "scratchpad") `shouldBe` Just "string"

    describe "kernel_status result shape (execKernelStatus)" $ do
        let statusResult =
                object
                    [ "state"
                        .= object
                            [ "state" .= ("idle" :: Text)
                            , "ksGen" .= (0 :: Int)
                            , "building" .= False
                            ]
                    , "ksGen" .= (0 :: Int)
                    , "ebGeneration" .= (0 :: Int)
                    ]
        it "pins the typed kernel_status field set (legacy keys gone)" $
            sort (objectKeys statusResult)
                `shouldBe` ["ebGeneration", "ksGen", "state"]
        it "state is a tagged object carrying the state tag" $
            case statusResult of
                Object o -> case KM.lookup "state" o of
                    Just (Object s) -> KM.lookup "state" s `shouldSatisfy` isString
                    _ -> expectationFailure "state is not an object"
                _ -> expectationFailure "not an object"
        it "ksGen and ebGeneration are numbers" $
            case statusResult of
                Object o -> do
                    KM.lookup "ksGen" o `shouldSatisfy` isNumber
                    KM.lookup "ebGeneration" o `shouldSatisfy` isNumber
                _ -> expectationFailure "not an object"

    describe "tool-result envelope (Sabela.Server.Ai.aiToolH)" $ do
        it "wraps every result in exactly {isError, result}" $
            sort (objectKeys (envelope False (object [])))
                `shouldBe` ["isError", "result"]
        it "isError is the ToolErr/ToolOk discriminator (boolean)" $
            case envelope True (object []) of
                Object o -> KM.lookup "isError" o `shouldBe` Just (Bool True)
                _ -> expectationFailure "not an object"

    describe "cell-result is the typed CellResult (autoExecuteAfterMutation)" $ do
        let cerr = CellError (Just 1) (Just 1) "boom"
            summaryOf res = toJSON (toCellResult res [])
        it "ok holds iff the outcome is Succeeded (legacy law preserved)" $ do
            okCellResult (toCellResult (Right (ExecutionResult [] Nothing [] [])) [])
                `shouldBe` True
            okCellResult (toCellResult (Right (ExecutionResult [] Nothing [cerr] [])) [])
                `shouldBe` False
            okCellResult (toCellResult (Right (ExecutionResult [] (Just "x") [] [])) [])
                `shouldBe` False
        it "the execution summary carries outcome/outputs/warnings/ok — no legacy keys" $
            sort
                ( objectKeys
                    (summaryOf (Right (ExecutionResult [] Nothing [] [])))
                )
                `shouldBe` ["ok", "outcome", "outputs", "warnings"]

isString :: Maybe Value -> Bool
isString (Just (String _)) = True
isString _ = False

isNumber :: Maybe Value -> Bool
isNumber (Just (Number _)) = True
isNumber _ = False
