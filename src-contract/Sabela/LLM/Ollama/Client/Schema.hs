{-# LANGUAGE OverloadedStrings #-}

{- | The tool schemas a turn is checked against: reading them off the offered
tool list, deciding whether an argument object fits one, coercing the
unambiguously-misspelled values, and naming the first argument that does not
fit precisely enough for the model to repair it.
-}
module Sabela.LLM.Ollama.Client.Schema (
    Schema (..),
    schemaFromTool,
    lookupSchema,
    matchingSchemas,
    argsFit,
    valueHasType,
    coerceArgs,
    argsFault,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

data Schema = Schema
    { schemaName :: Text
    , schemaProps :: KM.KeyMap Value
    , schemaRequired :: [Text]
    }

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

{- | Coerce arguments whose intent is unambiguous. A quoted @"5"@ where an
integer is declared is not a different request, it is the same request spelled
badly; rejecting it costs the whole turn. Anything genuinely ambiguous still
fails, with the offending argument named.
-}
coerceArgs :: Schema -> Value -> Value
coerceArgs schema (Object args) = Object (KM.mapWithKey fix args)
  where
    fix k v = case KM.lookup k (schemaProps schema) of
        Just spec
            | not (valueHasType spec v)
            , Just v' <- coerceTo spec v ->
                v'
        _ -> v
coerceArgs _ v = v

coerceTo :: Value -> Value -> Maybe Value
coerceTo (Object spec) (String s) = case KM.lookup "type" spec of
    Just (String "integer") -> Number . fromInteger <$> readInteger s
    Just (String "number") -> Number . fromInteger <$> readInteger s
    Just (String "boolean") -> readBool s
    _ -> Nothing
coerceTo _ _ = Nothing

readInteger :: Text -> Maybe Integer
readInteger t = case reads (T.unpack (T.strip t)) of
    [(n, "")] -> Just n
    _ -> Nothing

readBool :: Text -> Maybe Value
readBool t = case T.toLower (T.strip t) of
    "true" -> Just (Bool True)
    "false" -> Just (Bool False)
    _ -> Nothing

{- | Why these arguments do not fit, named precisely enough to repair. The
caller re-sends a tool call; "does not match schema" alone tells it nothing
about which argument to change, so it re-sends the same shape.
-}
argsFault :: Schema -> Value -> Maybe Text
argsFault schema (Object args) = case unknown ++ missing ++ mistyped of
    (why : _) -> Just why
    [] -> Nothing
  where
    unknown =
        [ "unknown argument '"
            <> K.toText k
            <> "'; this tool takes "
            <> offered
        | k <- KM.keys args
        , not (KM.member k (schemaProps schema))
        ]
    missing =
        [ "missing required argument '" <> r <> "'"
        | r <- schemaRequired schema
        , not (KM.member (K.fromText r) args)
        ]
    mistyped =
        [ "argument '"
            <> K.toText k
            <> "' should be "
            <> declaredType spec
            <> ", got "
            <> actualType v
        | (k, v) <- KM.toList args
        , Just spec <- [KM.lookup k (schemaProps schema)]
        , not (valueHasType spec v)
        ]
    offered =
        T.intercalate ", " (map K.toText (KM.keys (schemaProps schema)))
argsFault _ _ = Just "arguments must be a JSON object"

declaredType :: Value -> Text
declaredType (Object spec) = case KM.lookup "type" spec of
    Just (String t) -> "a " <> t
    _ -> "another type"
declaredType _ = "another type"

actualType :: Value -> Text
actualType v = case v of
    String _ -> "a string"
    Number _ -> "a number"
    Bool _ -> "a boolean"
    Object _ -> "an object"
    Array _ -> "an array"
    Null -> "null"
