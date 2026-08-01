{-# LANGUAGE OverloadedStrings #-}

{- | Reading a try payload back: the fields a property asserts over, and the one
invariant that relates two of them.
-}
module Test.TryPayloadRead (
    field,
    textField,
    renderAll,
    tierAgreesWithExecution,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ToolDoc (tierEvaluatePure, tierTypecheckOnly)

field :: Text -> Value -> Maybe Value
field key (Object obj) = KM.lookup (Key.fromText key) obj
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of
    Just (String t) -> Just t
    _ -> Nothing

renderAll :: Value -> Text
renderAll (Object o) = T.unwords (map renderAll (KM.elems o))
renderAll (String s) = s
renderAll (Array a) = T.unwords (map renderAll (foldr (:) [] a))
renderAll other = T.pack (show other)

{- | The tier a payload names never contradicts the execution fact it is read
from, and neither is stated without the other.
-}
tierAgreesWithExecution :: Value -> Bool
tierAgreesWithExecution v = case (textField "tier" v, field "executed" v) of
    (Nothing, Nothing) -> True
    (Just t, Just (Bool ran)) ->
        t == (if ran then tierEvaluatePure else tierTypecheckOnly)
    _ -> False
