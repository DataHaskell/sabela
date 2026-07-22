{-# LANGUAGE OverloadedStrings #-}

module Sabela.LLM.Ollama.Client.Parse (
    ParseFailure (..),
    ParseFailureClass (..),
    parseTurnWithTools,
) where

import Data.Aeson (Value (..), eitherDecodeStrict')
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isPunctuation, isSymbol)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.LLM.Ollama.Client.Repair (recoverFromError, repairJsonEscapes)
import Sabela.LLM.Ollama.Client.Types (ToolCall (..), Turn (..), rawWithCalls)

data ParseFailureClass
    = MalformedResponseJson
    | MalformedToolCall
    | GarbledToolName
    | InvalidToolArguments
    | UnsafeCellSource
    | TransportFailure
    deriving (Eq, Show)

data ParseFailure = ParseFailure
    { pfClass :: ParseFailureClass
    , pfDiagnostic :: Text
    , pfReprompt :: Text
    , pfSuggestedTool :: Maybe Text
    }
    deriving (Eq, Show)

data Schema = Schema
    { schemaName :: Text
    , schemaProps :: KM.KeyMap Value
    , schemaRequired :: [Text]
    }

parseTurnWithTools :: [Value] -> LBS.ByteString -> Either ParseFailure Turn
parseTurnWithTools tools raw = do
    response <- case eitherDecodeStrict' (LBS.toStrict raw) of
        Left err -> Left (failure MalformedResponseJson (T.pack err) Nothing)
        Right value -> Right value
    parseResponse (mapMaybe schemaFromTool tools) response

parseResponse :: [Schema] -> Value -> Either ParseFailure Turn
parseResponse schemas (Object outer)
    | Just (String err) <- KM.lookup "error" outer =
        case recoverFromError err of
            Just turn -> validateTurn schemas turn
            Nothing -> Left (failure MalformedToolCall err Nothing)
    | Just msg@(Object fields) <- KM.lookup "message" outer = do
        let content = textField "content" fields
            thinking = textField "thinking" fields
        native <- parseNativeCalls (KM.lookup "tool_calls" fields)
        calls <-
            if null native then recoverChannels schemas content thinking else Right native
        validated <- traverse (validateCall schemas) calls
        pure
            Turn
                { turnRaw = rawWithCalls msg validated
                , turnContent = content
                , turnCalls = validated
                }
    | otherwise =
        Left (failure MalformedResponseJson "missing message object" Nothing)
parseResponse _ _ = Left (failure MalformedResponseJson "response is not an object" Nothing)

parseNativeCalls :: Maybe Value -> Either ParseFailure [ToolCall]
parseNativeCalls Nothing = Right []
parseNativeCalls (Just (Array calls)) = traverse parseCallEnvelope (toList calls)
parseNativeCalls _ = Left (failure MalformedToolCall "tool_calls is not an array" Nothing)

parseCallEnvelope :: Value -> Either ParseFailure ToolCall
parseCallEnvelope (Object call)
    | Just (Object fn) <- KM.lookup "function" call = callFromObject fn
parseCallEnvelope _ = Left (failure MalformedToolCall "invalid tool-call envelope" Nothing)

callFromObject :: KM.KeyMap Value -> Either ParseFailure ToolCall
callFromObject fields = case (KM.lookup "name" fields, KM.lookup "arguments" fields) of
    (Just (String name), Just args@(Object _))
        | not (T.null name) ->
            Right (ToolCall name args)
    (Just (String name), Just (String raw)) ->
        case eitherDecodeStrict' (TE.encodeUtf8 (repairJsonEscapes raw)) of
            Right args@(Object _) -> Right (ToolCall name args)
            _ ->
                Left (failure InvalidToolArguments "arguments contain malformed JSON" Nothing)
    _ ->
        Left
            (failure MalformedToolCall "tool name or argument object is missing" Nothing)

recoverChannels :: [Schema] -> Text -> Text -> Either ParseFailure [ToolCall]
recoverChannels schemas content thinking =
    case mapMaybe (channelCall schemas) [content, thinking] of
        []
            | any looksToolPayload [content, thinking] ->
                Left
                    ( failure
                        MalformedToolCall
                        "tool-shaped channel payload is incomplete or malformed"
                        Nothing
                    )
            | otherwise -> Right []
        call : _ -> Right [call]

channelCall :: [Schema] -> Text -> Maybe ToolCall
channelCall schemas text = do
    objectText <- balancedObject text
    Object value <- decodeText objectText
    explicitCall value `orElse` argsOnlyCall schemas (Object value)
  where
    explicitCall value
        | Just (String name) <- KM.lookup "name" value
        , Just args@(Object _) <- KM.lookup "arguments" value =
            Just (ToolCall name args)
        | Just (Object fn) <- KM.lookup "function" value =
            either (const Nothing) Just (callFromObject fn)
        | otherwise = Nothing

argsOnlyCall :: [Schema] -> Value -> Maybe ToolCall
argsOnlyCall schemas args = case matchingSchemas schemas args of
    [schema] -> Just (ToolCall (schemaName schema) args)
    _ -> Nothing

validateTurn :: [Schema] -> Turn -> Either ParseFailure Turn
validateTurn schemas turn = do
    calls <- traverse (validateCall schemas) (turnCalls turn)
    pure turn{turnCalls = calls, turnRaw = rawWithCalls (turnRaw turn) calls}

validateCall :: [Schema] -> ToolCall -> Either ParseFailure ToolCall
validateCall schemas call = case lookupSchema (tcName call) schemas of
    Nothing ->
        let matches = matchingSchemas schemas (tcArgs call)
            suggestion = case matches of
                [schema] -> Just (schemaName schema)
                _ -> Nothing
         in Left
                (failure GarbledToolName ("unknown tool name '" <> tcName call <> "'") suggestion)
    Just schema
        | not (argsFit schema (tcArgs call)) ->
            Left
                ( failure
                    InvalidToolArguments
                    ("arguments do not match schema for " <> tcName call)
                    (Just (tcName call))
                )
        | Just diagnostic <- unsafeSource (tcArgs call) ->
            Left (failure UnsafeCellSource diagnostic (Just (tcName call)))
        | otherwise -> Right call

matchingSchemas :: [Schema] -> Value -> [Schema]
matchingSchemas schemas args = [schema | schema <- schemas, argsFit schema args]

argsFit :: Schema -> Value -> Bool
argsFit schema (Object args) =
    all (`KM.member` schemaProps schema) (KM.keys args)
        && all (\key -> KM.member (K.fromText key) args) (schemaRequired schema)
        && all fieldTypeFits (KM.toList args)
  where
    fieldTypeFits (key, value) = maybe False (`valueHasType` value) (KM.lookup key (schemaProps schema))
argsFit _ _ = False

valueHasType :: Value -> Value -> Bool
valueHasType (Object spec) value = case KM.lookup "type" spec of
    Just (String "string") -> isString value
    Just (String "integer") -> isInteger value
    Just (String "number") -> isNumber value
    Just (String "boolean") -> isBoolean value
    Just (String "object") -> isObject value
    Just (String "array") -> isArray value
    _ -> True
  where
    isString (String _) = True
    isString _ = False
    isInteger (Number n) = fromInteger (round n) == n
    isInteger _ = False
    isNumber (Number _) = True
    isNumber _ = False
    isBoolean (Bool _) = True
    isBoolean _ = False
    isObject (Object _) = True
    isObject _ = False
    isArray (Array _) = True
    isArray _ = False
valueHasType _ _ = True

unsafeSource :: Value -> Maybe Text
unsafeSource (Object args) =
    firstUnsafe
        [value | key <- sourceKeys, Just (String value) <- [KM.lookup key args]]
  where
    sourceKeys = map K.fromText ["source", "new_source", "code"]
    firstUnsafe [] = Nothing
    firstUnsafe (source : rest)
        | balloonedGarble source =
            Just "cell source is ballooned and dominated by garbled tokens"
        | otherwise = firstUnsafe rest
unsafeSource _ = Nothing

balloonedGarble :: Text -> Bool
balloonedGarble source = length ls >= 64 && suspicious * 3 >= length ls
  where
    ls = filter (not . T.null) (T.lines source)
    suspicious = length (filter garbledLine ls)
    garbledLine line =
        let chars = T.unpack line
            punctuation = length (filter (\c -> isPunctuation c || isSymbol c) chars)
            alphaNum = length (filter isAlphaNum chars)
         in punctuation >= 4 && punctuation > alphaNum

schemaFromTool :: Value -> Maybe Schema
schemaFromTool (Object tool) = do
    Object fn <- KM.lookup "function" tool
    String name <- KM.lookup "name" fn
    Object params <- KM.lookup "parameters" fn
    let props = case KM.lookup "properties" params of
            Just (Object values) -> values
            _ -> KM.empty
        required = case KM.lookup "required" params of
            Just (Array values) -> [name' | String name' <- toList values]
            _ -> []
    pure (Schema name props required)
schemaFromTool _ = Nothing

lookupSchema :: Text -> [Schema] -> Maybe Schema
lookupSchema name =
    foldr
        (\schema found -> if schemaName schema == name then Just schema else found)
        Nothing

balancedObject :: Text -> Maybe Text
balancedObject text = case T.breakOn "{" text of
    (_, rest) | T.null rest -> Nothing
    (_, rest) -> Just (T.dropWhileEnd (/= '}') rest)

decodeText :: Text -> Maybe Value
decodeText text = case eitherDecodeStrict' (TE.encodeUtf8 (repairJsonEscapes text)) of
    Right value -> Just value
    Left _ -> Nothing

looksToolPayload :: Text -> Bool
looksToolPayload text =
    let stripped = T.strip text
     in "{" `T.isPrefixOf` stripped
            && any
                (`T.isInfixOf` stripped)
                ["\"name\"", "\"arguments\"", "\"source\"", "\"new_source\"", "\"code\""]

failure :: ParseFailureClass -> Text -> Maybe Text -> ParseFailure
failure cls diagnostic suggested =
    ParseFailure
        { pfClass = cls
        , pfDiagnostic = diagnostic
        , pfReprompt =
            "Ollama tool-call parse failure ("
                <> T.pack (show cls)
                <> "): "
                <> diagnostic
                <> correction
                <> ". Re-send one complete tool call with an exact offered name and schema-valid arguments."
        , pfSuggestedTool = suggested
        }
  where
    correction =
        maybe "" (". The argument shape uniquely identifies '" <>) suggested
            <> maybe "" (const "'") suggested

textField :: Text -> KM.KeyMap Value -> Text
textField key fields = case KM.lookup (K.fromText key) fields of
    Just (String value) -> value
    _ -> ""

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just value) _ = Just value
orElse Nothing other = other
